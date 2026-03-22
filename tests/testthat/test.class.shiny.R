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
  port <- httpuv::randomPort()
  pkg_root <- here::here()

  # launch() returns a shinyApp object — runApp() blocks and serves
  rx <- callr::r_bg(
    function(pkg_root, port) {
      devtools::load_all(pkg_root)
      samp <- sampler()
      app <- samp$launch(open = FALSE, port = port)
      shiny::runApp(app, port = port, launch.browser = FALSE)
    },
    args = list(pkg_root = pkg_root, port = port),
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
