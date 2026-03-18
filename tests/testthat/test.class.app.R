test_that('Application: New', {
  app <- Application$new()

  expect_s3_class(app, c('R6', 'AppShiny', 'Application'))
  expect_s3_class(app$keypress, c('R6', 'Base', 'ModShiny', 'ModShiny', 'ModKey'))

  expect_null(app$auth)
})
