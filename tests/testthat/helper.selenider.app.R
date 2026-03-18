# styler: block

.tests <- new.env(parent = emptyenv())
.tests$rx <- NULL

appStart <- function() {

  if (!is.null(.tests$rx)) {
    .tests$rx$kill()
    .tests$rx <- NULL
  }

  rx <- callr::r_bg(
    func =
    function(port) {
      devtools::load_all()

      app <- Application$new()
      app$launch(open = FALSE, port = port)
    },
    args = list(port = 8000L),
    stdout = '',
    stderr = '',
    supervise = TRUE
    # cmdargs = c(
    # '--no-save',
    # '--no-restore'
    # )
  )

  cli::cli_progress_bar("Start shiny application...")
  while (!pingr::is_up(destination = '127.0.0.1', port = 8000L)) {
    cli::cli_progress_update()
#   if (!app$is_alive()) {
#     stop(app$read_all_error(), call. = FALSE)
#   }
  }
  cli::cli_progress_done()

  .tests$rx <- rx
  .tests$rx
}

# styler: block

appDelimitUpload <- function(screenshot = FALSE) {
  clickElement(selector = 'id:app-steps-step_delimit-act_upload_btn')

  screenshotElement(selector = 'css:div.modal-dialog', name = 'upload-delimit-1', screenshot = screenshot)

  setPathToElement(selector = 'id:app-steps-step_delimit-mod_1-tab_1-file', path = normalizePath(here::here('vignettes', 'data', 'psp', '2022.nairobi', 'masai-lodge.kmz')))

  screenshotElement(selector = 'css:div.modal-dialog', name = 'upload-delimit-2', screenshot = screenshot)

  clickElement(selector = 'id:app-steps-step_delimit-mod_1-act_ok', modal = TRUE)
}

appDrawPolygon <- function(screenshot = FALSE) {
  clickElement(selector = 'id:app-steps-step_delimit-act_draw_polygon_btn')

  coordinates <- list(
    c(529L, 233L),
    c(633L, 266L),
    c(663L, 432L),
    c(467L, 568L),
    c(250L, 543L),
    c(223L, 472L),
    c(402L, 350L),
    c(529L, 233L)
  )

  x <- 432L
  y <- 450L

  for (i in seq_along(coordinates)) {
    mouseClickToLocation(
      selector = 'id:app-steps-step_delimit-map',
      x = coordinates[[i]][1L] - x,
      y = coordinates[[i]][2L] - y
    )
    screenshotElement(selector = 'css:div.tabbable', name = paste0('draw-', i), screenshot = screenshot)
  }

  if (screenshot) {
    createAnimatedScreenshot('draw')
  }

}

appDrawRoofs <- function(screenshot = FALSE) {
  mouseClickToLocation(selector = 'id:app-steps-step_identify-map', x = 0L, y = 0L)

  screenshotElement(selector = 'css:div.tabbable', name = 'identify-start', screenshot = screenshot)

  coordinates <- list(
    c(329L - 452L, 398L - 383L),
    c(338L - 452L, 386L - 383L),
    c(373L - 452L, 391L - 383L),
    c(386L - 452L, 393L - 383L),
    c(401L - 452L, 388L - 383L),
    c(437L - 452L, 431L - 383L),
    c(453L - 452L, 421L - 383L),
    c(494L - 452L, 410L - 383L),
    c(507L - 452L, 315L - 383L),
    c(473L - 452L, 332L - 383L),
    c(523L - 452L, 337L - 383L),
    c(535L - 452L, 356L - 383L),
    c(530L - 452L, 388L - 383L),
    c(519L - 452L, 396L - 383L),
    c(507L - 452L, 401L - 383L),
    c(472L - 452L, 332L - 383L),
    c(408L - 452L, 309L - 383L)
  )

  for (i in seq_along(coordinates)) {
    mouseClickToLocation(
      selector = 'id:app-steps-step_identify-map',
      x = coordinates[[i]][1L],
      y = coordinates[[i]][2L]
    )
    screenshotElement(selector = 'css:div.tabbable', name = paste0('identify-', i), screenshot = screenshot)
  }

  clickElement(selector = 'id:app-steps-step_identify-act_check_btn')

  if (screenshot) {
    createAnimatedScreenshot('identify')
  }
}

appFindLocation <- function(location = 'masai lodge', screenshot = FALSE) {
  clickElement(selector = 'css:a.search-button')

  screenshotElement(selector = 'css:div.leaflet-control-search', name = 'search-2', x = -10L, y = -10L, width = 20L, height = 20L, screenshot = screenshot)

  setValueToElement(selector = 'css:input.search-input', text = location)

  screenshotElement(selector = 'css:div.leaflet-control-search', name = 'search-3', x = -10L, y = -60L, width = 400L, height = 70L, screenshot = screenshot)

  clickElement(selector = 'css:ul.search-tooltip>li:nth-child(2)')

  screenshotElement(selector = 'css:div.tabbable', name = 'search-4', screenshot = screenshot)

  clickElement(selector = 'id:app-steps-step_delimit-map')

  # Center Map
  # if (location == 'masai lodge') {
    sendKeysToElement(selector = 'id:app-steps-step_delimit-map', selenider::keys$down)
    sendKeysToElement(selector = 'id:app-steps-step_delimit-map', selenider::keys$down)
    sendKeysToElement(selector = 'id:app-steps-step_delimit-map', selenider::keys$right)
    sendKeysToElement(selector = 'id:app-steps-step_delimit-map', selenider::keys$right)
  # }

}

appGenerate <- function(method, screenshot = FALSE) {
  # if (method == 'RS_SMP') {
  #   clickSlider(id = 'app-steps-step_sample-sli_roofs_count', value = 10)
  # } else if (method == 'SP_QDR') {
  #   clickSlider(id = 'app-steps-step_sample-sli_sample_count', value = 10)
  #   clickSlider(id = 'app-steps-step_sample-sli_sample_size', value = 15)
  # } else if (method == 'SP_TSQ') {
  #   clickSlider(id = 'app-steps-step_sample-sli_sample_count', value = 10)
  # }

  if (method == 'sss') {
    clickElement(selector = 'id:app-steps-step_sample-act_sample_generate_btn')
    screenshotElement(selector = 'css:div.tabbable', name = sprintf('generate_%s_01', method), screenshot = screenshot)
    clickElement(selector = 'id:app-steps-step_sample-act_sample_save_btn')
    screenshotElement(selector = 'css:div.tabbable', name = sprintf('generate_%s_01_ok', method), screenshot = screenshot)

    if (screenshot) {
      imgs <- fs::dir_ls(path = here::here('vignettes/images'), regexp = 'generate_sss_[0-9]+')
      img_list <- lapply(imgs, magick::image_read)
      img_joined <- magick::image_join(img_list)
      img_animated <- magick::image_animate(img_joined, fps = 2)
      magick::image_write(image = img_animated, path = here::here('vignettes/images/generate_sss.webp'), format = 'webp')

      fs::file_delete(path = imgs)
    }

  } else {
    clickElement(selector = 'id:app-steps-step_sample-act_generate_modal_btn')
    clickElement(selector = 'id:app-steps-step_sample-act_generate', modal = TRUE)

    screenshotElement(selector = 'css:div.tabbable', name = sprintf('generate_%s', method), screenshot = screenshot)

    clickElement(selector = 'id:app-steps-step_sample-act_generate_ok', modal = TRUE)
  }

}

appIdentifyRoofsByGoogle <- function() {
  clickElement(selector = 'id:app-steps-step_identify-act_upload_btn')
  clickElement(selector = 'id:app-steps-step_identify-mod_2-tab_1-act_upload', modal = TRUE)
  clickElement(selector = 'id:app-steps-step_identify-mod_2-act_ok', modal = TRUE)
}

appInit <- function(method, clear = TRUE, screenshot = FALSE) {

  selenider::open_url(url = 'http://127.0.0.1:8000')

  cli::cli_progress_bar("Start shiny application...")
  while (isBusy()) {
    cli::cli_progress_update()
  # if (!app$is_alive()) {
  #   stop(app$read_all_error(), call. = FALSE)
  # }
  }


  Sys.sleep(2L)
  
  appSelectStep(step = 'delimit', method = method, screenshot = screenshot)

  if (clear) {
    clickElement(selector = 'id:app-steps-step_delimit-act_clear_btn')
    clickElement(selector = 'id:app-steps-step_delimit-act_clear_confirm', modal = TRUE)
  }

  # screenshotElement(selector = 'css:body', name = 'overview-start', screenshot = screenshot)

  clickElement(selector = sprintf('css:input[name="dlg_method-opt_method"][value="%s"]', method), modal = TRUE)
  clickElement(selector = 'id:dlg_method-act_ok', modal = TRUE)
}

appResultCalculate <- function(method, screenshot = FALSE) {
  clickTab(id = 'app-steps-step_result-tbs_results', index = 2L)
  clickElement(selector = 'id:app-steps-step_result-act_calculate')
  screenshotElement(selector = 'css:div.well > div > div.tabbable', name = sprintf('table_result_%s', method), screenshot = screenshot)
}

appResultData <- function(method, screenshot = FALSE) {
  clickTab(id = 'app-steps-step_result-tbs_results', index = 1L)
  screenshotElement(selector = 'css:div.well > div > div.tabbable', name = sprintf('table_data_%s', method), screenshot = screenshot)
}

appSave <- function(step, screenshot = FALSE) {
  clickElement(selector = sprintf('id:app-steps-step_%s-act_ok_btn', step))
  clickElement(selector = sprintf('id:app-steps-step_%s-act_ok_confirm', step), modal = TRUE)

  screenshotElement(selector = 'css:div.tabbable', name = sprintf('page_%s_saved', step), screenshot = screenshot)
}

appSelectPolygon <- function(step) {
  clickElement(selector = sprintf('css:div#app-steps-step_%s-map > div.leaflet-pane > div.leaflet-polygon_2-pane > svg > g > path:nth-child(1)', step))
}

appSelectStep <- function(step, method, screenshot = FALSE) {
  clickTab(id = 'app-steps-tbs_steps', index = match(step, c('delimit', 'identify', 'sample', 'result')))

  # screenshotElement(rdc,
  # 'css:div.tabbable', name = sprintf('page_%s_%s', step, method), screenshot = screenshot
  # modify = function(img) {
  #   img <- rectTipAt(img, 'id:app-steps-step_identify-act_upload_btn', color = '#ff00ea', padding = 4, xd = -4, label = 'Add a Roof layers...')
  #   img <- rectTipAt(img, 'id:app-steps-step_identify-act_identify_btn', color = '#ff00ea', padding = 4, xd = -4, label = 'Search and Identify')
  #   return(img)
  # }
  # )
}

appShrink <- function(screenshot = FALSE) {
  mouseClickToLocation(selector = 'id:app-steps-step_identify-map', x = 0L, y = 0L)

  screenshotElement(selector = 'css:div.tabbable', name = 'shrink-start', screenshot = screenshot)

  coordinates <- list(
    c(0L, 0L),
    c(-100L, -125L),
    c(0L, -125L),
    c(100L, -125L),
    c(-100L, 0L),
    c(0L, 0L),
    c(125L, 0L),
    c(0L, 125L),
    c(125L, 125L),
    c(0L, 200L)
  )

  for (i in seq_along(coordinates)) {
    screenshotElement(selector = 'css:div.tabbable', name = paste0('shrink-', i), screenshot = screenshot)
    mouseClickToLocation(
      selector = 'id:app-steps-step_identify-map',
      x = coordinates[[i]][1L],
      y = coordinates[[i]][2L]
    )
  }

  clickElement(selector = 'id:app-steps-step_identify-act_check_btn')

  if (screenshot) {
    createAnimatedScreenshot('shrink')
  }
}

createAnimatedScreenshot <- function(name) {
  imgs <- fs::dir_ls(path = here::here('vignettes', 'images'), regexp = paste0(name, '-[0-9]+'))
  img_list <- lapply(imgs, magick::image_read)
  img_joined <- magick::image_join(img_list)
  img_animated <- magick::image_animate(img_joined, fps = 2L)
  magick::image_write(image = img_animated, path = here::here('vignettes', 'images', paste0(name, '.webp')), format = 'webp')

  fs::file_delete(path = imgs)
}

# cleanSessionSelenium()
# rd <- seleniumDriver()
# rdc <- rd[['client']]
# app <- callr::r_bg(
#   function() {
#     devtools::load_all()
#     app <- Application$new()
#     app$launch(open = FALSE, port = 8000L)
#   },
#   stdout = '',
#   stderr = '',
#   cmdargs = c('--no-save', '--no-restore')
# )

# rdc$navigate(sprintf('http://127.0.0.1:%s', 8000L))

# withr::defer({
#     app$kill_tree()
#     rd[['server']]$stop()
#   },
#   teardown_env()
# )
