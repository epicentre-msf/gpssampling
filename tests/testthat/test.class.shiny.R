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

runBackgroundApp(
  func = function(port) {
    app <- AppShinyBase$new()
    app$launch(open = FALSE, port = port)
  },
  force = TRUE
)

session <- getSeleniumSession()

test_that('Shiny App Base', {

  selenider::open_url(url = 'http://127.0.0.1:8000')

  expect_true(selenider:::is_selenium_server(session$server)) # nolint: undesirable_operator_linter.
  expect_true(selenider:::is_selenium_client(session$driver)) # nolint: undesirable_operator_linter.

  expect_identical(session$session, 'selenium')

  withr::with_namespace('selenider', {

    elem_click(s('#toggle_div'))
    elem_expect(s('.toggleable'), is_visible, has_text('Hello'))
    elem_click(s('#toggle_div'))
    elem_expect(s('.toggleable'), !is_visible)

    elem_expect(
      elem_children(s('.buttons'))[[1L]],
      is_enabled
    )

    elem_expect(
      elem_children(s('.buttons'))[[2L]],
      is_disabled
    )

  })

})
