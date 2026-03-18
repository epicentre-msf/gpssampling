# ........................................................................
# . Selenium helpers
#   Code adapted from here
#     https://github.com/mrc-ide/shiny90
#     https://github.com/mrc-ide/odin.ui/


# styler: block Selenider Session

#' Selenider Session
#'
#'
#' @description
#' Function to initiate a Selenium session for web scraping using Selenider and Chromote packages.
#'
#' @param timeout Numeric value representing the timeout value for the session (default is 10 seconds).
#' @param .env Environment in which to evaluate the Selenium session (default is the calling environment).
#'
#' @details
#' This function creates a Selenium session using the Selenider package with a specified timeout value and environment for evaluation.
#' Additionally, it sets up the session using the Chromote package to interact with Chrome browser.
#'
#' @return The result of the Selenium session initiation.
#'
#' @examples
#' getSeleniumSession()
#' getSeleniumSession(timeout = 15L)
#'
getSeleniumSession <- function(timeout = 10L, .env = rlang::caller_env()) {
  session <- 'selenium'

  skip_if_offline()
  # skip_if_selenium_server_available()
  skip_if_selenium_unavailable()

  selenider::selenider_session(
    browser = 'firefox',
    options = selenider::selenium_options(
      client_options = selenider::selenium_client_options(
        capabilities =
          list(
            `moz:firefoxOptions` = list(
              args = list(
                '--width=900',
                '--height=700'
                # browser.download.dir = downloadedFiles,
                # browser.download.folderList = 2L,
                # browser.helperApps.neverAsk.saveToDisk = paste(
                #   'application/zip;text/csv;application/pdf;',
                #   'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
                #   sep = ';'
                # ),
                # pdfjs.disabled = TRUE
              )
            )
          )
      ),
      server_options = selenider::selenium_server_options(
        temp = FALSE
      )
    ),
    session = session,
    timeout = timeout,
    .env = .env
  )
}

# styler: block Selenider

clickElement <- function(selector, modal = FALSE) {

  elt <- findElement(selector)

  if (!modal) {
    s(id = 'shiny-modal-wrapper') |> selenider::elem_expect(!is_present)
  }

  s(css = 'div.waiter-overlay') |> selenider::elem_expect(!is_present)

  # while (isBusy()) {
  #   cat('.')
  #   Sys.sleep(1L)
  # }

  # if (selenider::is_present(elt)) {
    selenider::elem_click(elt)
  # }
  # while (isBusy()) {
  #   cat('.')
  #   Sys.sleep(1L)
  # }

}

if (requireNamespace('selenider', quietly = TRUE)) {
  s <- selenider::s
  is_present <- selenider::is_present
  is_visible <- selenider::is_visible
  is_displayed <- selenider::is_displayed
  is_enabled <- selenider::is_enabled
}

#' findElement function
#'
#' This function allows users to select elements based on a specified selector type and selection.
#'
#' @param selector A string specifying the selector type and selection, in the format 'type:selection'.
#' Supported selector types are 'class', 'css', 'id', 'name', and 'xpath'.
#' Example: 'css:#myDiv' selects element(s) with the CSS selector '#myDiv'.
#'
#' @return An object representing the selected element(s).
#' The returned object will have an attribute corresponding to the specified selector type,
#' with the value being the specified selection.
#'
#' @examples
#' findElement('id:myDiv')
#' findElement('css:.myClass')
#' findElement('xpath://div[@class="myClass"]')
#'
findElement <- function(selector, timeout = 2L) {

  selector_pattern <- '(css|id|xpath|class|name):(.+)'
  selector_type <- stringr::str_extract(string = selector, pattern = selector_pattern, group = 1L)
  selector_selection <- stringr::str_extract(string = selector, pattern = selector_pattern, group = 2L)
  session <- selenider::get_session(create = FALSE)

  elt <- switch(selector_type,
    class = s(class = selector_selection),
    css = s(css = selector_selection),
    id = s(id = selector_selection),
    name = s(name = selector_selection),
    xpath = s(xpath = selector_selection)
  )

  selenider::elem_expect(elt, is_present, timeout = timeout, testthat = TRUE)

  elt
}

getDirSeleniumDownload <- function() {
  tolower(here::here('vignettes', 'images', 'download'))
}

getDirSeleniumScreenshot <- function() {
  tolower(here::here('vignettes', 'images'))
}

getFileSeleniumScreenshot <- function(name = 'screenshot') {
  tolower(fs::path(getDirSeleniumScreenshot(), name, ext = 'webp'))
}

mouseClickToLocation <- function(selector, x, y) {
  elt <- findElement(selector = selector)
  elt_web <- selenider::get_actual_element(elt)
  elt_web_rect <- elt_web$get_rect()

  actions <- selenium::actions_stream(
    selenium::actions_mousemove(x = x, y = y, origin = elt_web),
    selenium::actions_mousedown()
  )

  elt$driver$perform_actions(actions)
}

mouseGrabToLocation <- function(selector, x1, y1, x2, y2) {
  elt <- findElement(selector)
  elt_web <- selenider::get_actual_element(elt)
  elt_web_rect <- elt_web$get_rect()

  actions <- selenium::actions_stream(
    selenium::actions_mousemove(x = 0L, y = 0L, origin = elt_web),
    selenium::actions_mousedown(),
    selenium::actions_mousemove(x = 100L, y = 0L, origin = elt_web),
    selenium::actions_mouseup()
  )

  elt$driver$perform_actions(actions)
  # rdc$mouseMoveToLocation(x = x1, y = y1, webElement = elt)
  # rdc$buttondown()
  # rdc$mouseMoveToLocation(x = x2, y = y2)
  # rdc$buttonup()
}

openURL <- function(url, port = NULL) {
  if (!is.null(port)) {
    url <- glue::glue('{url}:{port}')
  } else {
    port <- 80L
  }
  selenider::open_url(url)
  # while (!pingr::is_up(destination = '127.0.0.1', port = port)) {
  #   Sys.sleep(0.01)
  # }
}

screenshotElement <- function(selector = NULL, name = 'screenshot', modify = NULL, x = 0L, y = 0L, width = 0L, height = 0L, shadow = '50x10+30+30', screenshot = FALSE) {

  if (!screenshot) {
    return()
  }

  Sys.sleep(0.5)

  f_img     <- getFileSeleniumScreenshot(name = name)
  f_img_new <- getFileSeleniumScreenshot(name = glue::glue('{name}-new'))
  f_img_tmp <- fs::path_temp(tolower(name), ext = 'webp')

  if (!is.null(selector)) {
    elt <- findElement(selector = selector)
  }

  selenider::take_screenshot(file = f_img_tmp)

  graphics.off()

  img <- magick::image_read(f_img_tmp)
  img <- magick::image_draw(img)

  if (!is.null(selector)) {

    elt_web_rect <- selenider::get_actual_element(elt)$get_rect()

    elt_web_rect$x <- elt_web_rect$x + x
    elt_web_rect$y <- elt_web_rect$y + y
    elt_web_rect$width <- elt_web_rect$width + width
    elt_web_rect$height <- elt_web_rect$height + height

    if (x != 0L) {
      rect(
        elt_web_rect$x,
        elt_web_rect$y,
        elt_web_rect$x + elt_web_rect$width - 1L,
        elt_web_rect$y + elt_web_rect$height - 1L,
        border = 'lightgray', lty = 'solid', lwd = 1L
      )
    }

    # magick::image_browse(img)
    # rect(
    #   elt_web_rect$x - 3,
    #   elt_web_rect$y - 3,
    #   elt_web_rect$x + elt_web_rect$width + 2,
    #   elt_web_rect$y + elt_web_rect$height + 2,
    #   border = 'red', lty = 'solid'
    # )

    # rect(
    #   elt_web_rect$x - 2,
    #   elt_web_rect$y - 2,
    #   elt_web_rect$x + elt_web_rect$width + 1,
    #   elt_web_rect$y + elt_web_rect$height + 1,
    #   border = 'white', lty = 'solid'
    # )

    # rect(
    #   elt_web_rect$x - 1,
    #   elt_web_rect$y - 1,
    #   elt_web_rect$x + elt_web_rect$width,
    #   elt_web_rect$y + elt_web_rect$height,
    #   border = 'white', lty = 'solid'
    # )

    # rect(
    #   elt_web_rect$x,
    #   elt_web_rect$y,
    #   elt_web_rect$x + elt_web_rect$width - 1,
    #   elt_web_rect$y + elt_web_rect$height - 1,
    #   border = 'white', lty = 'solid'
    # )

    # rect(
    #   elt_web_rect$x + 1,
    #   elt_web_rect$y + 1,
    #   elt_web_rect$x + elt_web_rect$width - 2,
    #   elt_web_rect$y + elt_web_rect$height - 2,
    #   border = 'white', lty = 'solid'
    # )

    # border <- grid::roundrectGrob(
    #   x = elt_web_rect$x + elt_web_rect$width / 2,
    #   y = magick::image_info(img)$height - (elt_web_rect$y + elt_web_rect$height / 2),
    #   width = elt_web_rect$width,
    #   height = elt_web_rect$height,
    #   default.units = 'pt',
    #   gp = grid::gpar(col = 'gray', lwd = 1)
    # )
    # grid::grid.draw(border)

    img <- magick::image_crop(img, sprintf('%sx%s+%s+%s', elt_web_rect$width, elt_web_rect$height, elt_web_rect$x, elt_web_rect$y))

    # img <- img |>
    # magick::image_fill('transparent', point = sprintf('+%s+%s', 1, 1), fuzz = 0)

    # dev.off()
  }

  if (!is.null(modify)) {
    img <- modify(img)
  }

  if (!is.null(shadow)) {
    img <- magick::image_shadow(img, geometry = shadow, bg = 'white')
  }

  if (fs::file_exists(f_img)) {
    img_old  <- magick::image_read(f_img)
    img_diff <- magick::image_compare(img, img_old, metric = 'AE', fuzz = 50L)
    if (attributes(img_diff)$distortion > 10L) {
      magick::image_write(img, path = f_img, format = 'webp')
    }
  } else {
    magick::image_write(img, path = f_img, format = 'webp')
  }


  invisible(img)
}

sendKeysToElement <- function(selector, key) {
  clickElement(selector = selector)
  elt <- findElement(selector = selector)
  if (!is.null(elt)) {
    selenider::elem_send_keys(elt, key)
  }
}

setPathToElement <- function(selector, path) {
  elt <- findElement(selector = selector)
  if (!is.null(elt)) {
    elt_web <- selenider::get_actual_element(elt)
    elt_web$send_keys(path)
  }
}

setValueToElement <- function(selector, text) {
  elt <- findElement(selector = selector)
  if (!is.null(elt)) {
    selenider::elem_set_value(elt, text)
  }
}

skip_if_selenium_server_available <- function() {
  skip_if_not(selenium::selenium_server_available())
}

skip_if_selenium_unavailable <- function() {
  selenider::skip_if_selenider_unavailable('selenium')
}

startApp <- function(func, port = 8000L, .env = rlang::caller_env()) {

  app_bg <- callr::r_bg(
    func,
    args = list(port = port),
    stdout = '|',
    stderr = '|',
    supervise = TRUE
    # cmdargs = c(
    # '--no-save',
    # '--no-restore'
    # )
  )

  if (app_bg$is_alive()) {

    cli::cli_progress_bar("Start shiny application...")

    while (!pingr::is_up(destination = '127.0.0.1', port = 8000L)) {
      cli::cli_progress_update()
    }

    cli::cli_progress_done()

    cli::cli_alert_success('App is alive.')

  } else {

    cli::cli_alert_warning('App is not alive.')

  }

  app_bg
}

cleanSessionSelenium <- function() {
  r <- system('taskkill /im geckodriver.exe /f /t', intern = FALSE, ignore.stderr = TRUE, ignore.stdout = TRUE, show.output.on.console = FALSE) # nolint: nonportable_path_linter.
  r <- system('taskkill /im java.exe /f /t', intern = FALSE, ignore.stderr = TRUE, ignore.stdout = TRUE, show.output.on.console = FALSE) # nolint: nonportable_path_linter.
  r <- system('taskkill /im rscript.exe /f /t', intern = FALSE, ignore.stderr = TRUE, ignore.stdout = TRUE, show.output.on.console = FALSE) # nolint: nonportable_path_linter.
}

# styler: block Waiting For

isBusy <- function() {
  session <- selenider::get_session(create = FALSE)
  session$driver$execute_script('return $("html").hasClass("shiny-busy")')
}

waitFor <- function(predicate, timeout = 20L) {
  waited <- 0L
  while (!predicate()) {
    # if (waited == 0L) {
    #   cat('\nSearch')
    # } else {
    #   cat('.')
    # }
    Sys.sleep(0.05)
    waited <- waited + 0.05
    if (waited >= timeout) {
      # num <- numberScreenshot()
      # screenshotPath <- fs::path(screenshotsFolder, glue::glue('test{num}.png'))
      # rdc$screenshot(file = screenshotPath)
      stop(glue::glue('Timed out waiting {timeout}s for predicate to be true - screenshot {screenshotPath}'))
    }
  }
}

waitForAndTryAgain <- function(predicate, failureCallBack, maxTries = 2L, timeout = 5L) {
  waited <- 0L
  tries <- 0L
  while (!predicate()) {
    Sys.sleep(0.05)
    waited <- waited + 0.05
    if (waited >= timeout) {
      if (tries >= maxTries) {
        # num <- numberScreenshot()
        # screenshotPath <- file.path(screenshotsFolder, glue::glue('test{num}.png'))
        # rdc$screenshot(file = screenshotPath)
        stop(glue::glue('Timed out waiting {timeout}s for predicate to be true - screenshot {screenshotPath}'))
      } else {
        tries <- tries + 1L
        failureCallBack()
      }
    }
  }
}

waitForChildElement <- function(parent, selector) {
  children <- waitForThisManyChildren(parent, selector, 1L)
  children[[1L]]
}

waitForElement <- function(selector, timeout = 10L) {
  waitFor(rdc,
    function() {
      r <- try(findElement(selector = selector), silent = TRUE)
      class(r) != 'try-error'
    },
    timeout = timeout
  )
}

waitForElements <- function(selector) {
  elements <- waitForThisManyElements(selector, 1L)
  elements[[1L]]
}

waitForShinyToNotBeBusy <- function(timeout = 20L) {
  waitFor(rdc,
    function() {
      cat('.')
      !isBusy(rdc)
    },
    timeout = timeout
  )
}

waitForThisManyChildren <- function(parent, selector, expectedNumber) {
  waitFor(function() {
    length(parent$findChildElements('css', selector)) == expectedNumber
  })
  parent$findChildElements('css', selector)
}

waitForThisManyElements <- function(selector, expectedNumber) {
  waitFor(function() {
    length(rdc$findElements('css', selector)) == expectedNumber
  })
  rdc$findElements('css', selector)
}

waitForVisible <- function(element) {
  waitFor(function() {
    isVisible(element)
  })
}
