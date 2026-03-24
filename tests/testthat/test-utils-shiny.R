# devtools::test(filter = 'test-utils-shiny', stop_on_failure = TRUE)

test_that('button creates actionButton tag', {
  btn <- button('btn_id', label = 'Click')
  expect_s3_class(btn, 'shiny.tag')
  html <- as.character(btn)
  expect_true(grepl('btn_id', html))
  expect_true(grepl('Click', html))
})

test_that('button with semantic adds btn-* class', {
  semantics <- c(
    'primary', 'secondary', 'success', 'danger',
    'warning', 'info', 'light', 'dark', 'link'
  )
  for (sem in semantics) {
    btn <- button('btn_id', label = sem, semantic = sem)
    html <- as.character(btn)
    expect_true(
      grepl(paste0('btn-', sem), html) || grepl(sem, html),
      label = sprintf('semantic = "%s" should appear in HTML', sem)
    )
  }
})

test_that('button with size adds btn-sm or btn-lg class', {
  btn_sm <- button('btn_id', label = 'Small', size = 'sm')
  expect_true(grepl('btn-sm', as.character(btn_sm)))

  btn_lg <- button('btn_id', label = 'Large', size = 'lg')
  expect_true(grepl('btn-lg', as.character(btn_lg)))
})
