test_that('Base class: initialize', {
  o <- Base$new()

  expect_s3_class(o, c('R6', 'Base'))
  expect_null(o$parent)
})

test_that('Base class: initialize function sets parent attribute correctly', {
  o <- Base$new()
  o_child <- Base$new(parent = o)

  expect_identical(o_child$parent, o)
})

test_that('Base class: parent active field returns and sets value correctly', {
  o <- Base$new()
  o_child <- Base$new()
  o_child$parent <- o

  expect_identical(o_child$parent, o)
})

test_that('Base class: parent active field throws error with invalid input', {
  f <- function() {
    o <- Base$new()
    o$parent <- 'invalid'
  }
  expect_error(f(), "Assertion on 'value' failed: Must inherit from class 'Base', but has class 'character'.")
})
