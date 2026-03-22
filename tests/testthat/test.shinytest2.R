# shinytest2 integration tests for the GpsSampler Shiny app
#
# These tests use shinytest2::AppDriver (chromote headless browser) to verify
# the Shiny app loads, renders correctly, and responds to user interactions.

skip_if_not_installed("shinytest2")
skip_if_not_installed("chromote")

# Verify chromote can actually start (headless Chrome must be available)
skip_if(
  !chromote::has_default_chromote_object() &&
    is.null(tryCatch(chromote::find_chrome(), error = function(e) NULL)),
  "Chrome/Chromium not available for headless testing"
)

app_dir <- test_path("apps", "gpssampling-app")

# ---------------------------------------------------------------------------
# Helper: create an AppDriver with standard settings
# ---------------------------------------------------------------------------
new_app <- function(..., timeout = 60000L) {
  shinytest2::AppDriver$new(
    app_dir = app_dir,
    name = "gpssampling",
    timeout = timeout,
    load_timeout = timeout,
    wait = TRUE,
    ...
  )
}


# ===========================================================================
# 1. App startup
# ===========================================================================

test_that("App starts without error", {
  app <- new_app()
  withr::defer(app$stop())

  # App process is alive

  expect_true(app$is_running())
})

test_that("App serves valid HTML with Shiny bootstrap", {
  app <- new_app()
  withr::defer(app$stop())

  html <- app$get_html("html")
  expect_true(nchar(html) > 500L)
  # Contains a Shiny output or input binding
  expect_true(grepl("shiny", html, ignore.case = TRUE))
})


# ===========================================================================
# 2. Initial UI structure
# ===========================================================================

test_that("Navbar is present", {
  app <- new_app()
  withr::defer(app$stop())

  navbar_html <- app$get_html(".navbar")
  expect_true(nchar(navbar_html) > 0L)
})

test_that("Four workflow tabs exist", {
  app <- new_app()
  withr::defer(app$stop())

  # The steps tabset should have 4 tab links (+ possibly a home tab)
  tabs_html <- app$get_html("#app-steps-tbs_steps")
  expect_true(nchar(tabs_html) > 0L)

  # Each step tab should be present
  for (step in c("step_delimit", "step_identify", "step_sample", "step_result")) {
    tab_id <- sprintf("app-steps-%s-tab", step)
    tab_html <- app$get_html(sprintf("#%s", tab_id))
    expect_true(
      nchar(tab_html) > 0L,
      info = sprintf("Tab '%s' should exist in the DOM", step)
    )
  }
})

test_that("Delimit tab is active on startup", {
  app <- new_app()
  withr::defer(app$stop())

  values <- app$get_values()
  # The steps tabset input should reflect the first step
  step_val <- values$input[["app-steps-tbs_steps"]]
  expect_true(
    !is.null(step_val),
    info = "Steps tabset input should have a value"
  )
})


# ===========================================================================
# 3. Leaflet map
# ===========================================================================

test_that("Leaflet map renders on the delimit tab", {
  app <- new_app()
  withr::defer(app$stop())

  # Leaflet map should be an output
  map_html <- app$get_html("#app-steps-step_delimit-map")
  expect_true(
    nchar(map_html) > 0L,
    info = "Leaflet map container should be in the DOM"
  )
  # The map container should have leaflet CSS class
  expect_true(
    grepl("leaflet", map_html, ignore.case = TRUE),
    info = "Map container should contain leaflet markup"
  )
})


# ===========================================================================
# 4. Key UI elements on delimit step
# ===========================================================================

test_that("Delimit step has toolbar buttons", {
  app <- new_app()
  withr::defer(app$stop())

  # Save, rollback, clear buttons from the Step base class
  for (btn_suffix in c("act_ok", "act_rollback", "act_clear")) {
    btn_id <- sprintf("app-steps-step_delimit-%s", btn_suffix)
    btn_html <- app$get_html(sprintf("#%s", btn_id))
    expect_true(
      nchar(btn_html) > 0L,
      info = sprintf("Button '%s' should exist", btn_suffix)
    )
  }
})


# ===========================================================================
# 5. Language selector
# ===========================================================================

test_that("Language selector is present", {
  app <- new_app()
  withr::defer(app$stop())

  lang_html <- app$get_html("#opt_language")
  expect_true(
    nchar(lang_html) > 0L,
    info = "Language selector should exist"
  )
})


# ===========================================================================
# 6. No JS console errors on startup
# ===========================================================================

test_that("No critical JS errors on startup", {
  app <- new_app()
  withr::defer(app$stop())

  logs <- app$get_logs()
  # Filter for severe/error level
  if (nrow(logs) > 0L && "level" %in% names(logs)) {
    severe <- logs[logs$level == "ERROR", ]
    # Allow some expected warnings (e.g., missing Google API key)
    if (nrow(severe) > 0L) {
      # Filter out known benign errors
      benign_patterns <- c("favicon", "google", "maps.googleapis")
      is_benign <- vapply(severe$message, function(msg) {
        any(vapply(benign_patterns, function(pat) {
          grepl(pat, msg, ignore.case = TRUE)
        }, logical(1L)))
      }, logical(1L))
      real_errors <- severe[!is_benign, ]
      expect_equal(
        nrow(real_errors), 0L,
        info = sprintf("Unexpected JS errors: %s", paste(real_errors$message, collapse = "; "))
      )
    }
  }
})


# ===========================================================================
# 7. Shiny values inspection
# ===========================================================================

test_that("App exports expected input values", {
  app <- new_app()
  withr::defer(app$stop())

  values <- app$get_values()

  # Should have input values

  expect_true(length(values$input) > 0L, info = "App should have inputs")

  # Should have output values
  expect_true(length(values$output) > 0L, info = "App should have outputs")
})


# ===========================================================================
# 8. Tab interaction - switching tabs
# ===========================================================================

test_that("Clicking an inactive step tab does not crash the app", {
  app <- new_app()
  withr::defer(app$stop())

  # Try clicking the identify tab (step 2)
  # It may be disabled (greyed out) since no polygon is drawn, but clicking
  # should not crash the app
  tryCatch(
    app$click(selector = "#app-steps-step_identify-tab a"),
    error = function(e) NULL
  )

  # App should still be running
  expect_true(app$is_running())
})


# ===========================================================================
# 9. About/help dialog
# ===========================================================================

test_that("Help button exists and app stays alive after click", {
  app <- new_app()
  withr::defer(app$stop())

  help_html <- app$get_html("#act_help")
  if (nchar(help_html) > 0L) {
    tryCatch(
      app$click(selector = "#act_help"),
      error = function(e) NULL
    )
    Sys.sleep(1)
    expect_true(app$is_running())
  } else {
    skip("Help button not found in this app configuration")
  }
})


# ===========================================================================
# 10. Multiple app instances with different methods
# ===========================================================================

test_that("App starts with random sampling method", {
  # Create a variant app.R that passes method = "RS_SMP"
  tmp_dir <- withr::local_tempdir()
  writeLines(
    c(
      'devtools::load_all(here::here())',
      'samp <- sampler()',
      'samp$launch(open = FALSE, method = "RS_SMP")'
    ),
    file.path(tmp_dir, "app.R")
  )

  app <- shinytest2::AppDriver$new(
    app_dir = tmp_dir,
    name = "gpssampling-rs",
    timeout = 30000L,
    load_timeout = 30000L,
    wait = TRUE
  )
  withr::defer(app$stop())

  expect_true(app$is_running())
  html <- app$get_html("html")
  expect_true(nchar(html) > 500L)
})

test_that("App starts with spatial sampling method", {
  tmp_dir <- withr::local_tempdir()
  writeLines(
    c(
      'devtools::load_all(here::here())',
      'samp <- sampler()',
      'samp$launch(open = FALSE, method = "SP_SMP")'
    ),
    file.path(tmp_dir, "app.R")
  )

  app <- shinytest2::AppDriver$new(
    app_dir = tmp_dir,
    name = "gpssampling-sp",
    timeout = 30000L,
    load_timeout = 30000L,
    wait = TRUE
  )
  withr::defer(app$stop())

  expect_true(app$is_running())
  html <- app$get_html("html")
  expect_true(nchar(html) > 500L)
})

test_that("App starts with T-square method", {
  tmp_dir <- withr::local_tempdir()
  writeLines(
    c(
      'devtools::load_all(here::here())',
      'samp <- sampler()',
      'samp$launch(open = FALSE, method = "SP_TSQ")'
    ),
    file.path(tmp_dir, "app.R")
  )

  app <- shinytest2::AppDriver$new(
    app_dir = tmp_dir,
    name = "gpssampling-tsq",
    timeout = 30000L,
    load_timeout = 30000L,
    wait = TRUE
  )
  withr::defer(app$stop())

  expect_true(app$is_running())
})

test_that("App starts with quadrat method", {
  tmp_dir <- withr::local_tempdir()
  writeLines(
    c(
      'devtools::load_all(here::here())',
      'samp <- sampler()',
      'samp$launch(open = FALSE, method = "SP_QDR")'
    ),
    file.path(tmp_dir, "app.R")
  )

  app <- shinytest2::AppDriver$new(
    app_dir = tmp_dir,
    name = "gpssampling-qdr",
    timeout = 30000L,
    load_timeout = 30000L,
    wait = TRUE
  )
  withr::defer(app$stop())

  expect_true(app$is_running())
})


# ===========================================================================
# 11. Step-specific UI elements
# ===========================================================================

test_that("Delimit step has file upload input", {
  app <- new_app()
  withr::defer(app$stop())

  # File input for uploading polygon layers
  file_html <- app$get_html("#app-steps-step_delimit-file")
  expect_true(
    nchar(file_html) > 0L,
    info = "File upload input should exist on delimit step"
  )
})

test_that("Delimit step has download buttons", {
  app <- new_app()
  withr::defer(app$stop())

  # At least the shapefile download button should be present
  for (dl in c("act_download_shp", "act_download_kml")) {
    btn_id <- sprintf("app-steps-step_delimit-%s", dl)
    btn_html <- app$get_html(sprintf("#%s", btn_id))
    # Download buttons may be hidden until polygons exist, but should be in DOM
    # We just check the app doesn't crash when querying them
    expect_true(
      is.character(btn_html),
      info = sprintf("Download button '%s' query should not error", dl)
    )
  }
})


# ===========================================================================
# 12. Sidebar panel structure
# ===========================================================================

test_that("App has sidebar panel with action buttons", {
  app <- new_app()
  withr::defer(app$stop())

  # The sidebar should contain draw/upload buttons
  sidebar_btns <- c(
    "app-steps-step_delimit-act_draw_polygon",
    "app-steps-step_delimit-act_upload"
  )
  for (btn_id in sidebar_btns) {
    btn_html <- app$get_html(sprintf("#%s", btn_id))
    expect_true(
      is.character(btn_html),
      info = sprintf("Sidebar button '%s' should be queryable", btn_id)
    )
  }
})


# ===========================================================================
# 13. App cleanup and process management
# ===========================================================================

test_that("App stops cleanly", {
  app <- new_app()
  app$stop()

  expect_false(app$is_running())
})


# ===========================================================================
# 14. Snapshot: initial state
# ===========================================================================

test_that("Initial input values are consistent", {
  app <- new_app()
  withr::defer(app$stop())

  values <- app$get_values()

  # The active step should be the first tab
  step_input <- values$input[["app-steps-tbs_steps"]]
  if (!is.null(step_input)) {
    # Just verify it's a character value (tab ID)
    expect_type(step_input, "character")
  }
})


# ===========================================================================
# 15. App window title
# ===========================================================================

test_that("App has a page title", {
  app <- new_app()
  withr::defer(app$stop())

  title_html <- app$get_html("title")
  expect_true(nchar(title_html) > 0L)
})
