# devtools::test(filter = 'test.shiny', stop_on_failure = TRUE)

# cleanSessionSelenium()

startApp(
  func = function(port) {
    devtools::load_all()

    app <- Application$new()
    app$launch(open = FALSE, port = port)
  }
)

session <- getSeleniumSession()

# ........................................................................
# . Tests

test_that('Selenider', {
  skip()

  selenider::open_url(url = 'https://ashbythorpe.github.io/selenider/articles/test-site.html')

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

test_that('App: Start', {

  appInit(method = 'RS_SMP', screenshot = FALSE)

  clickElement(selector = 'id:app-steps-step_delimit-map')

  img <- screenshotElement(selector = 'css:body', name = 'overview', screenshot = TRUE)
  img <- img |>
    rectAt(selector = 'id:app-steps-tbs_steps', label = 'Tabs', color = 'red', xd = 20L, yd = 20L, width = 400L, cex = 1.5) |>
    rectAt(selector = 'id:app-steps-step_delimit-info', label = 'Guide', color = 'orange', xd = 16L, yd = 18L, wd = 2L, hd = 2L, cex = 1.5) |>
    rectAt(selector = 'css:div.sidebar-tabs', label = 'Sidebar', color = 'green', xd = 18L, yd = 18L, wd = 0L, hd = 0L, cex = 1.5, vertical = TRUE, align = 'bottom')

  magick::image_write(img, path = getFileSeleniumScreenshot(name = 'overview'), format = 'webp')
})

test_that('App: Walk random sampling', {

  appInit(method = 'RS_SMP')

  # Delimit
  appSelectStep(step = 'delimit', method = 'rs', screenshot = TRUE)
  appFindLocation(screenshot = TRUE)
  appDrawPolygon(screenshot = TRUE)
  appSelectPolygon(step = 'delimit')
  appSave(step = 'delimit')
  appDelimitUpload(screenshot = TRUE)
  appSelectPolygon(step = 'delimit')
  appSave(step = 'delimit', screenshot = TRUE)

  # Identify
  appSelectStep(step = 'identify', method = 'rs', screenshot = TRUE)
  appDrawRoofs(screenshot = TRUE)
  appSave(step = 'identify')
  appIdentifyRoofsByGoogle()
  appSave(step = 'identify', screenshot = TRUE)

  # Sample
  appSelectStep(step = 'sample', method = 'rs', screenshot = TRUE)
  appSelectPolygon(step = 'sample')
  appGenerate(method = 'rs', screenshot = TRUE)
  appSave(step = 'sample', screenshot = TRUE)

  # Result
  appSelectStep(step = 'result', method = 'rs')
  appSelectPolygon(step = 'result')
  appSelectStep(step = 'result', method = 'rs', screenshot = TRUE)
  appResultData(method = 'rs', screenshot = TRUE)
})

test_that('App: Walk spatial sampling', {

  appInit(method = 'SP_SMP')

  # Delimit
  appSelectStep(step = 'delimit', method = 'ss', screenshot = TRUE)
  appDelimitUpload(screenshot = TRUE)
  appSelectPolygon('delimit')
  appSave('delimit', screenshot = TRUE)

  # Identify
  appSelectStep(step = 'identify', method = 'ss', screenshot = TRUE)
  appShrink(screenshot = TRUE)
  appSave(step = 'identify', screenshot = TRUE)

  # Sample
  appSelectStep(step = 'sample', method = 'ss', screenshot = TRUE)
  appSelectPolygon(step = 'sample')
  appGenerate(method = 'ss', screenshot = TRUE)
  appSave(step = 'sample', screenshot = TRUE)

  # Result
  appSelectStep(step = 'result', method = 'ss')
  appSelectPolygon(step = 'result')
  appSelectStep(step = 'result', method = 'ss', screenshot = TRUE)
  appResultData(method = 'ss', screenshot = TRUE)
})

test_that('App: Walk spatial sampling supervised', {

  appInit(method = 'SP_SPV')

  # Delimit
  appSelectStep(step = 'delimit', method = 'sss', screenshot = TRUE)
  appDelimitUpload(screenshot = TRUE)
  appSelectPolygon('delimit')
  appSave('delimit', screenshot = TRUE)

  # Identify
  appSelectStep(step = 'identify', method = 'sss', screenshot = TRUE)
  appShrink(screenshot = TRUE)
  appSave(step = 'identify', screenshot = TRUE)

  set.seed(123L)

  # Sample
  appSelectStep(step = 'sample', method = 'sss', screenshot = TRUE)
  appSelectPolygon(step = 'sample')
  appGenerate(method = 'sss', screenshot = TRUE)
  appSave(step = 'sample', screenshot = TRUE)

  # Result
  appSelectStep(step = 'result', method = 'sss')
  appSelectPolygon(step = 'result')
  appSelectStep(step = 'result', method = 'sss', screenshot = TRUE)
  appResultData(method = 'sss', screenshot = TRUE)
})

withr::deferred_run()
