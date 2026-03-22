test_that('GpsSampler: New', {
  samp <- sampler()

  expect_s3_class(samp, c('R6', 'AppShiny', 'GpsSampler'))
  expect_s3_class(
    samp$keypress,
    c('R6', 'Base', 'ModShiny', 'ModShiny', 'ModKey')
  )

  expect_null(samp$auth)
})

test_that('GpsSampler: Application backward compatibility', {
  app <- Application$new()

  expect_s3_class(app, 'GpsSampler')
  expect_identical(Application, GpsSampler)
})
