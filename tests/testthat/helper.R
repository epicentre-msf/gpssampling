
# styler: block

# Function to click on a slider element and set its value
# Arguments:
#   - id: ID of the slider element
#   - value: Value to be set on the slider
clickSlider <- function(id, value) {
  # Find the slider element
  elt <- findElement(selector = sprintf('xpath://*[@id="%s"]', id))

  # Extract min and max values from the slider element attributes
  el_min <- as.numeric(elt$getElementAttribute('data-min'))
  el_max <- as.numeric(elt$getElementAttribute('data-max'))

  # Find the slider line element and get its dimensions
  el_line <- findElement(selector = sprintf('xpath://*[@id="%s"]/../span/span/span', id))
  el_line_dim <- el_line$getElementSize()

  # Adjust the width of the slider line element
  el_line_dim$width <- el_line_dim$width - 16L

  # Calculate the scale for mapping slider values to pixel positions
  el_line_scale <- el_line_dim$width / el_max

  # Move the mouse to the slider line element
  rdc$mouseMoveToLocation(webElement = el_line)

  # Move the mouse to the appropriate position based on the slider value
  rdc$mouseMoveToLocation(x = -(el_line_dim$width / 2L), y = 0L)
  rdc$mouseMoveToLocation(x = value * el_line_scale, y = 0L)

  # Click on the slider at the calculated position
  rdc$click()

  # Pause for 1 second to allow the action to take effect
  Sys.sleep(1L)
}

clickTab <- function(id, index = 1L) {
  clickElement(selector = sprintf('css:ul#%s > li:nth-child(%s) > a', id, index))
}

enterText <- function(element, text, clear = FALSE) {
  element$clearElement()

  element$click
  element$sendKeysToElement(list(text))
}

errorMessage <- function(rdc) {
  stringr::str_split_fixed(rdc$errorDetails()$localizedMessage, '\n', simplify = TRUE)[1L, 1L]
}

expectElementPresent <- function(selector) {
  elements <- rdc$findElements('css', selector)
  expect(
    ok = length(elements) > 0L,
    failure_message = glue::glue('Expected an element in the page to match {selector}')
  )
}

expectElementPresent <- function(selector) {
  elements <- findElement(selector)
  expect(ok = length(elements) > 0L, failure_message = glue::glue('Expected an element in the page to match {selector}'))
}

expectTextEqual <- function(expected, element) {
  expect_identical(getText(element), expected)
}

expectTextToContain <- function(expectedText, element) {
  actualText <- getText(element)
  expect(
    ok = grepl(expectedText, actualText, fixed = TRUE),
    failure_message = glue::glue("Expected to find string '{expectedText}' in '{actualText}'")
  )
}

getDirTests <- function() {
  here::here('tests', 'testthat')
}

getDirTestsShiny <- function() {
  fs::path(getDirTests(), 'app') |>
    fs::path_norm()
}

getElementIfPresent <- function(selector) {
  elements <- rdc$findElements('css', selector)
  if (length(elements) > 0L) {
    elements[[1L]]
  } else {
    NULL
  }
}

getFileLog <- function() {
  fs::path(getDirTestsShiny(), 'app.log')
}

getFilePid <- function() {
  fs::path(getDirTestsShiny(), 'app.pids')
}

getTabLink <- function(tabText) {
  selector <- glue::glue("a[data-toggle=tab][data-value='{tabText}']")
  link <- rdc$findElement('css', selector)
}

getText <- function(element) {
  texts <- element$getElementText()
  if (length(texts) > 1L) {
    fail(message = 'More than one element matched the expression')
  }
  texts[[1L]]
}

inActivePane <- function(selector) {
  glue::glue('.tab-pane.active {selector}')
}

isTabEnabled <- function(tabText) {
  getTabLink(tabText)$getElementAttribute('class') != 'disabled'
}

isVisible <- function(element) {
  element$isElementDisplayed() == 'TRUE'
}

navigate <- function(x1 = NA_integer_, y1 = NA_integer_, selector1 = NULL, x2 = NA_integer_, y2 = NA_integer_, selector2 = NULL) {
  el1 <- findElement(selector = selector1)
  rdc$mouseMoveToLocation(x = x1, y = y1, webElement = el1)
  rdc$buttondown()
  rdc$mouseMoveToLocation(x = x2, y = y2)
  rdc$buttonup()
}

rectAt <- function(img, selector, padding = 0L, xd = 0L, yd = 0L, wd = 0L, hd = 0L, color = 'blue', label = NULL, cex = 1L, width = NULL, height = NULL, align = 'right', vertical = FALSE) {
  elt <- findElement(selector = selector)
  elt_rect <- selenider::get_actual_element(elt)$get_rect()

  x <- elt_rect$x + xd
  y <- elt_rect$y + yd
  w <- elt_rect$width + wd
  h <- elt_rect$height + hd

  if (!is.null(width)) {
    w <- width
  }

  if (!is.null(height)) {
    h <- height
  }

  img <- magick::image_draw(img)

  rect(
    x - padding - 1L,
    y - padding - 1L,
    x + padding + w,
    y + padding + h,
    border = color,
    col = scales::alpha(color, alpha = 0.1),
    lty = 'solid', lwd = 2L
  )

  if (!is.null(label)) {
    th <- graphics::strheight(label, cex = cex)
    tw <- graphics::strwidth(label, cex = cex)
    # if (FALSE) {
    #   tx <- x + w / 2L
    #   ty <- y + h / 2L - 2L
    #   pad <- th * 0.2
    #   rect(
    #     xleft = tx - tw / 2L + pad,
    #     xright = tx + tw / 2L - pad,
    #     ytop = ty + th / 2L + pad,
    #     ybottom = ty - th / 2L - pad,
    #     col = scales::alpha('white', 0.5),
    #     border = NA
    #   )
    #   text(x + w / 2L, y + h / 2L - 2L, label, cex = cex, col = color)
    # } else {
      if (align == 'right') {
        rect(
          x + padding + w - 1L,
          y - padding + 1L,
          x + padding + w - tw * 2L,
          y + padding + h - 1L,
          col = scales::alpha(color, alpha = 0.75),
          border = NA
        )
        text(x + padding + w - tw, y + padding + h / 2L, label, cex = cex, col = 'white')
      } else if (align == 'bottom') {
        rect(
          x + padding + 1L,
          y - padding + h - 1L,
          x + padding + w - -1L,
          y + padding + h - tw * 2L,
          col = scales::alpha(color, alpha = 0.75),
          border = NA
        )
        text(x + padding + w / 2L, y + padding + h - tw, label, cex = cex, col = 'white', srt = 90L)
      }
    # }
  }
  dev.off()

  img
}

rectTipAt <- function(img, selector, padding = 0L, xd = 0L, yd = 0L, color = 'blue', label = NULL, cex = 1L, width = NULL, height = NULL, align = 'right', vertical = FALSE) {
  elt <- findElement(selector = selector)
  elt_rect <- selenider::get_actual_element(elt)$get_rect()

  x <- elt_rect$x + xd
  y <- elt_rect$y + yd
  w <- elt_rect$width
  h <- elt_rect$height

  if (!is.null(width)) {
    w <- width
  }

  if (!is.null(height)) {
    h <- height
  }

  img <- magick::image_draw(img)

  rect(
    x - padding,
    y - padding,
    x + w + 1L,
    y + h + 1L,
    border = color,
    col = scales::alpha(color, alpha = 0.1),
    lty = 'solid', lwd = 2L
  )

  if (!is.null(label)) {
    th <- graphics::strheight(label, cex = cex)
    tw <- graphics::strwidth(label, cex = cex)
    # if (FALSE) {
    #   tx <- x + w / 2L
    #   ty <- y + h / 2L - 2L
    #   pad <- th * 0.2
    #   rect(
    #     xleft = tx - tw / 2L + pad,
    #     xright = tx + tw / 2L - pad,
    #     ytop = ty + th / 2L + pad,
    #     ybottom = ty - th / 2L - pad,
    #     col = scales::alpha('white', 0.5),
    #     border = NA
    #   )
    #   text(x + w / 2L, y + h / 2L - 2L, label, cex = cex, col = color)
    # } else {
      if (align == 'right') {
        rect(
          x + w * 2L - tw / 2L - 5L,
          y + h / 2L + th - 1L,
          x + w * 2L + tw / 2L + 5L,
          y + h / 2L - th - 1L,
          col = scales::alpha(color, alpha = 0.75),
          border = NA
        )
        text(x + w * 2L, y + h / 2L, label, cex = cex, col = 'white')
      } else if (align == 'bottom') {
        rect(
          x + padding + 1L,
          y - padding + h - 1L,
          x + padding + w - -1L,
          y + padding + h - tw * 2L,
          col = scales::alpha(color, alpha = 0.75),
          border = NA
        )
        text(x + padding + w / 2L, y + padding + h - tw, label, cex = cex, col = 'white', srt = 90L)
      }
    # }
  }
  dev.off()

  img
}

save_png <- function(code, width = 400L, height = 400L) {
  path <- fs::file_temp(ext = 'png')
  png(path, width = width, height = height)
  on.exit(dev.off())
  code
  path
}

switchTab <- function(tabText, timeout = 10L) {
  link <- getTabLink(tabText)
  waitFor(function() {
    link$getElementAttribute('class') != 'disabled'
  }, timeout)

  waitFor(function() {
    getTabLink(tabText)$clickElement()
    title <- rdc$findElement('css', inActivePane('.panelTitle'))
    tabText == getText(title)
  })
}

.selenium <- new.env(parent = emptyenv())
# @blocksort asc
numberScreenshot <- local({
  counter <- 0L
  function() {
    counter <<- counter + 1L
    counter
  }
})
# findElement <- function(selector = NULL) {
#   if (is.null(selector)) {
#     return(elt)
#   }
#   selector_parts <- stringr::str_split_fixed(selector, pattern = ':', n = 2L, simplify = TRUE)
#   selector_using <- selector_parts[1L, 1L]
#   selector_value <- selector_parts[1L, 2L]
#   if (selector_using == 'css') {
#     selector_using <- 'css selector'
#   }
#   elt <- tryCatch(
#     {
#       suppressMessages({
#         rdc$findElement(using = selector_using, value = selector_value)
#       })
#     },
#     error = function(e) {
#       return(NULL)
#     }
#   )
#   expect(ok = !is.null(elt), failure_message = glue::glue('Expected an element in the page to match {selector}.'))
#   return(elt)
# }
## Generic retry function - evaluate the function 'f' with no
## arguments with a break of 'poll' between evaluations until it
## returns TRUE, or until 'timeout' seconds, in which case throw an
## error (using the string 'description' to make it informative.
## Typically we will use wrappers around this.
## Evaluate the function 'f' until it does not throw an error.
## Try and read from the shiny url until it becomes responsive.
