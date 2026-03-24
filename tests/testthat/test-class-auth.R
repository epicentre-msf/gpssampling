test_that('ShinyAuth: New', {
  auth <- ShinyAuth$new()

  expect_s3_class(auth, c('ShinyAuth', 'R6'), exact = TRUE)
})
