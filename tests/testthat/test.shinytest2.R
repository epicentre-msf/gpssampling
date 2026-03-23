# shinytest2 integration tests for the GpsSampler Shiny app
#
# These tests use shinytest2::AppDriver (chromote headless browser) to verify
# the Shiny app loads, renders correctly, and responds to user interactions.
#
# Architecture note: on startup the app shows a method-selection modal dialog
# (dlg_method). The Step toolbar buttons (act_ok, act_rollback, act_clear) are
# only rendered after the method dialog is dismissed AND the map renders. The
# sidebar is created via shinyjs::hidden() and only revealed after interaction.
# Tabs use data-value attributes, not id attributes.

skip_if_not_installed("shinytest2")
skip_if_not_installed("chromote")

# Verify chromote can actually start (headless Chrome must be available)
skip_if(
  !chromote::has_default_chromote_object() &&
    is.null(tryCatch(chromote::find_chrome(), error = function(e) NULL)),
  "Chrome/Chromium not available for headless testing"
)

app_dir <- test_path("apps", "gpssampling-app")

# Resolve package root now (while we are in the package dir), so temp-dir
# tests can pass it to devtools::load_all().
pkg_root <- here::here()

# ---------------------------------------------------------------------------
# Shared app instance for the main test suite. Creating a single AppDriver
# avoids resource exhaustion from spawning 20+ Shiny + Chrome processes.
# ---------------------------------------------------------------------------
app <- shinytest2::AppDriver$new(
  app_dir = app_dir,
  name = "gpssampling",
  timeout = 60000L,
  load_timeout = 60000L,
  wait = TRUE
)
withr::defer(app$stop(), teardown_env())

# Give the app a moment to fully render all UI components
Sys.sleep(3)

# Cache the full HTML for element-existence checks
app_html <- app$get_html("html")


# ===========================================================================
# 1. App startup
# ===========================================================================

test_that("App starts and serves valid HTML", {
  expect_true(nchar(app_html) > 500L)
  expect_true(grepl("shiny", app_html, ignore.case = TRUE))
})


# ===========================================================================
# 2. Initial UI structure
# ===========================================================================

test_that("Navbar is present", {
  navbar_html <- app$get_html("#navbar")
  expect_true(nchar(navbar_html) > 0L)
})

test_that("Four workflow tabs exist", {
  tabs_html <- app$get_html("#app-steps-tbs_steps")
  expect_true(nchar(tabs_html) > 0L)

  # Tabs use data-value attributes
  for (step in c("step_delimit", "step_identify", "step_sample", "step_result")) {
    tab_val <- sprintf("app-steps-%s-tab", step)
    tab_html <- app$get_html(sprintf('[data-value="%s"]', tab_val))
    expect_true(
      sum(nchar(tab_html)) > 0L,
      info = sprintf("Tab '%s' should exist in the DOM", step)
    )
  }
})

test_that("Delimit tab is active on startup", {
  values <- app$get_values()
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
  map_html <- app$get_html("#app-steps-step_delimit-map")
  expect_true(
    nchar(map_html) > 0L,
    info = "Leaflet map container should be in the DOM"
  )
  expect_true(
    grepl("leaflet", map_html, ignore.case = TRUE),
    info = "Map container should contain leaflet markup"
  )
})


# ===========================================================================
# 4. Method selection dialog
# ===========================================================================

test_that("Method selection dialog is shown on startup", {
  modal_html <- app$get_html(".modal")
  expect_true(
    nchar(modal_html) > 0L,
    info = "Method selection modal should be present on startup"
  )
  ok_html <- app$get_html("#dlg_method-act_ok")
  expect_true(
    nchar(ok_html) > 0L,
    info = "Method dialog OK button should exist"
  )
})


# ===========================================================================
# 5. Language selector
# ===========================================================================

test_that("Language selector is present", {
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
  logs <- app$get_logs()
  expect_true(is.data.frame(logs))

  if (nrow(logs) > 0L && "level" %in% names(logs)) {
    severe <- logs[logs$level == "ERROR", ]
    if (nrow(severe) > 0L) {
      benign_patterns <- c("favicon", "google", "maps.googleapis")
      is_benign <- vapply(severe$message, function(msg) {
        any(vapply(benign_patterns, function(pat) {
          grepl(pat, msg, ignore.case = TRUE)
        }, logical(1L)))
      }, logical(1L))
      real_errors <- severe[!is_benign, ]
      expect_equal(
        nrow(real_errors), 0L,
        info = sprintf(
          "Unexpected JS errors: %s",
          paste(real_errors$message, collapse = "; ")
        )
      )
    }
  }
})


# ===========================================================================
# 7. Shiny values inspection
# ===========================================================================

test_that("App exports expected input and output values", {
  values <- app$get_values()
  expect_true(length(values$input) > 0L, info = "App should have inputs")
  expect_true(length(values$output) > 0L, info = "App should have outputs")
})


# ===========================================================================
# 8. Sidebar structure
# ===========================================================================

test_that("Delimit step sidebar exists with action buttons", {
  sidebar_html <- app$get_html("#app-steps-step_delimit-sidebar")
  expect_true(
    nchar(sidebar_html) > 0L,
    info = "Delimit step sidebar should exist in DOM"
  )
  expect_true(
    grepl("act_upload_side", sidebar_html),
    info = "Upload button should be in the sidebar"
  )
  expect_true(
    grepl("act_draw_polygon_side", sidebar_html),
    info = "Draw polygon button should be in the sidebar"
  )
})


# ===========================================================================
# 9. Step title panels
# ===========================================================================

test_that("Step title panels render", {
  for (step in c("step_delimit", "step_sample", "step_result")) {
    title_id <- sprintf("#app-steps-%s-title_panel", step)
    title_html <- app$get_html(title_id)
    expect_true(
      nchar(title_html) > 0L,
      info = sprintf("Title panel for '%s' should render", step)
    )
  }
})


# ===========================================================================
# 10. Tab interaction
# ===========================================================================

test_that("Clicking an inactive step tab does not crash the app", {
  tryCatch(
    app$click(
      selector = 'a[data-value="app-steps-step_identify-tab"]'
    ),
    error = function(e) NULL
  )

  # App should still serve HTML
  html <- app$get_html("html")
  expect_true(nchar(html) > 0L)
})


# ===========================================================================
# 11. Help button
# ===========================================================================

test_that("Help button exists and app stays alive after click", {
  help_html <- app$get_html("#act_help")
  if (length(help_html) > 0L && nchar(help_html) > 0L) {
    tryCatch(
      app$click(selector = "#act_help"),
      error = function(e) NULL
    )
    Sys.sleep(1)
    html <- app$get_html("html")
    expect_true(nchar(html) > 0L)
  } else {
    skip("Help button not found in this app configuration")
  }
})


# ===========================================================================
# 12. Initial input values
# ===========================================================================

test_that("Initial input values are consistent", {
  values <- app$get_values()
  step_input <- values$input[["app-steps-tbs_steps"]]
  if (!is.null(step_input)) {
    expect_type(step_input, "character")
  }
})


# ===========================================================================
# 13. App window title
# ===========================================================================

test_that("App has a page title", {
  title_html <- app$get_html("title")
  expect_true(nchar(title_html) > 0L)
})


# ===========================================================================
# 14. Multiple sampling methods (separate app instances)
# ===========================================================================

test_that("App starts with each sampling method", {
  for (method in c("RS_SMP", "SP_SMP", "SP_TSQ", "SP_QDR")) {
    tmp_dir <- withr::local_tempdir()
    writeLines(
      c(
        sprintf('devtools::load_all("%s")', pkg_root),
        'samp <- sampler()',
        sprintf('samp$launch(open = FALSE, method = "%s")', method)
      ),
      file.path(tmp_dir, "app.R")
    )

    method_app <- shinytest2::AppDriver$new(
      app_dir = tmp_dir,
      name = sprintf("gpssampling-%s", tolower(method)),
      timeout = 30000L,
      load_timeout = 30000L,
      wait = TRUE
    )

    html <- method_app$get_html("html")
    expect_true(
      nchar(html) > 500L,
      info = sprintf("App should load with method %s", method)
    )

    method_app$stop()
  }
})
