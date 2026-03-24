test_that('logger exists and is a logger object', {
  expect_true(exists('.logger', envir = asNamespace('gpssampling')))
  expect_s3_class(
    get('.logger', envir = asNamespace('gpssampling')),
    'logger'
  )
})

test_that('log functions execute without error', {
  expect_no_error(logInfo('test info %s', 'message'))
  expect_no_error(logWarn('test warn %s', 'message'))
  expect_no_error(logError('test error %s', 'message'))
})
