test_that('New AppShinyBase', {
  app <- AppShinyBase$new()

  expect_s3_class(app, c('R6', 'AppShinyBase'))
  expect_null(AppShinyBase$new()$domain)
})

test_that('New AppShinyWithAuthentification', {
  app <- AppShinyWithAuthentification$new()

  expect_s3_class(app, c('R6', 'AppShinyWithAuthentification'))
})

test_that('New AppShinyWithTranslation', {
  app <- AppShinyWithTranslation$new()

  expect_s3_class(app, c('R6', 'AppShinyWithTranslation'))
})

test_that('New AppShinyNav', {
  app <- AppShinyNav$new()

  expect_s3_class(app, c('R6', 'AppShinyNav'))
})

test_that('GpsSampler launch returns shiny.appobj', {
  samp <- sampler()
  app <- samp$launch(open = FALSE)
  expect_s3_class(app, 'shiny.appobj')
})

test_that('GpsSampler app serves HTTP', {
  # callr::r_bg spawns a fresh R process that needs gpssampling installed;
  # devtools::load_all() only loads in the current session.
  pkg_ok <- tryCatch(
    callr::r(function() library(gpssampling), timeout = 15),
    error = function(e) FALSE
  )
  skip_if(identical(pkg_ok, FALSE), "gpssampling not installed for subprocess")
  port <- httpuv::randomPort()
  # launch() returns a shinyApp object — runApp() blocks and serves
  rx <- callr::r_bg(
    function(port) {
      library(gpssampling)
      samp <- sampler()
      app <- samp$launch(open = FALSE, port = port)
      shiny::runApp(app, port = port, launch.browser = FALSE)
    },
    args = list(port = port),
    stdout = '', stderr = '', supervise = TRUE
  )
  withr::defer(rx$kill())

  # Wait for app to serve
  ready <- FALSE
  for (i in 1:15) {
    Sys.sleep(1)
    if (pingr::is_up('127.0.0.1', port = port)) {
      ready <- TRUE
      break
    }
    if (!rx$is_alive()) break
  }

  expect_true(ready, info = 'GpsSampler app should be serving HTTP')

  # Verify HTTP response
  resp <- httr::GET(sprintf('http://127.0.0.1:%d', port))
  expect_equal(resp$status_code, 200L)
  html <- httr::content(resp, 'text', encoding = 'UTF-8')
  expect_true(nchar(html) > 200L)
  expect_true(grepl('shiny', html, ignore.case = TRUE))
})
