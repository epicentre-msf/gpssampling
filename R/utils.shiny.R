# styler: block Button

#' Create a button widget
#'
#' This function creates a button with specified characteristics.
#'
#' @param inputId The input ID of the button
#' @param label The text to display on the button
#' @param icon An optional icon to display alongside the label
#' @param class An optional class for styling the button
#' @param loading Logical indicating if the button is in a loading state
#' @param loading_label The label to display when the button is in a loading state
#' @param ... Additional arguments to be passed to the button function
#'
#' @return A button widget with specified characteristics
#'
#' @examples
#' button('button_id', 'Click me', icon = 'check')
#' button('submit', 'Submit', loading = TRUE)
#'
button <- function(..., loading = FALSE, loading_label = .('Loading...')) {
  args <- rlang::list2(...)

  if (is.character(args$icon)) {
    args$icon <- icon(args$icon, size = NULL)
  }

  if (loading) {
    # if (is.null(class)) class <- 'btn btn-default'
    # shinyFeedback::loadingButton(inputId = inputId, label = shiny::HTML(as.character(label)), class = class, loadingLabel = 'Calculating')
    tag <- do.call(shinyFeedback::loadingButton, args)
  } else {
    tag <- do.call(shiny::actionButton, args)
  }

  tag_qr <- htmltools::tagQuery(tag)

  if (!is.null(args$size)) {
    checkmate::assertChoice(args$size, choices = c('sm', 'lg'))

    tag_qr <- tag_qr$addClass(glue::glue('btn-{args$size}'))
  }

  tag_qr$allTags()
}

buttonDanger <- function(class = NULL, ...) {
  if (is.null(class)) class <- 'btn btn-danger'
  button(class = class, ...)
}

buttonDownload <- function(
  outputId,
  label = .('Download'),
  icon = 'download',
  class = NULL,
  ...
) {
  if (is.defined(icon)) icon <- icon(icon, size = NULL)
  shiny::downloadButton(
    outputId = outputId,
    label = label,
    icon = icon,
    class = class,
    ...
  )
}

#' Create a button with optional loading state and loading label
#'
#' @inheritDotParams shinyFeedback::loadingButton
#'
#' @return HTML button with optional loading behavior
#'
#' @examples
#' ui <- tagList(
#'   buttonLoading('act_submit', label = 'Submit'),
#'   buttonLoading('act_submit', label = 'Submit', semantic = 'danger')
#' )
#'
#' shinyAppMinimal(ui = ui)
#'
buttonLoading <- function(
  ...,
  margin = 2L,
  semantic = 'default',
  size = NULL,
  hide = FALSE,
  outline = FALSE
) {
  args <- rlang::list2(...)

  checkmate::assertChoice(
    semantic,
    choices = c(
      'default',
      'primary',
      'secondary',
      'success',
      'danger',
      'warning',
      'info',
      'light',
      'dark',
      'link'
    )
  )
  checkmate::assertChoice(size, null.ok = TRUE, choices = c('lg', 'sm', 'xs'))
  checkmate::assertInt(margin, null.ok = TRUE, lower = 1L, upper = 7L)
  checkmate::assertFlag(outline)
  checkmate::assertFlag(hide)

  args$label <- shiny::HTML(as.character(args$label))

  tag <- do.call(shinyFeedback::loadingButton, args)

  tag_qr <- htmltools::tagQuery(tag)

  tag_qr <- tag_qr$find('button')

  tag_qr <- tag_qr$removeClass('btn-primary')

  if (outline) {
    tag_qr <- tag_qr$addClass(glue::glue('btn-outline-{semantic}'))
  } else {
    tag_qr <- tag_qr$addClass(glue::glue('btn-{semantic}'))
  }

  if (!is.null(args$size)) {
    tag_qr <- tag_qr$addClass(glue::glue('btn-{size}'))
  }

  if (!is.null(margin)) {
    tag_qr <- tag_qr$addClass(glue::glue('m-{margin}'))
  }

  hidden(tag_qr$allTags(), hide = hide)
}

buttonPrimary <- function(class = NULL, ...) {
  if (is.null(class)) class <- 'btn btn-primary'
  button(class = class, ...)
}

# styler: block Layout

row <- function(..., flex = TRUE, align_self = NULL) {
  checkmate::checkFlag(flex)
  checkmate::checkChoice(
    align_self,
    choices = c('start', 'end', 'center', 'baseline', 'stretch'),
    null.ok = TRUE
  )

  tag <- shiny::div(...)

  tag_qr <- htmltools::tagQuery(tag)

  if (flex) {
    tag_qr$addClass('d-flex flex-row')
  }

  if (!is.null(align_self)) {
    tag_qr$children()$addClass(glue::glue('align-self-{align_self}'))
  }

  tag_qr$allTags()
}

# styler: block Modal Dialog

#' @noRd
modalDanger <- function(
  inputIdOk,
  icon = 'alert',
  msg = 'Are you sure.',
  msg_info = NULL,
  msg_bold = TRUE,
  label_cancel = 'Cancel',
  label_ok = 'Confirm'
) {
  if (msg_bold) {
    msg <- sprintf('<b>%s</b>', msg)
  }
  shiny::showModal(
    shiny::modalDialog(
      shiny::fluidPage(
        shiny::fluidRow(
          style = 'width: 400px',
          shiny::column(2L, style = 'color: #c9302c;', icon(icon, size = 'l')),
          shiny::column(
            10L,
            shiny::p(shiny::HTML(msg)),
            shiny::p(shiny::HTML(msg_info))
          )
        )
      ),
      size = 's',
      footer = shiny::tagList(
        shiny::modalButton(label = label_cancel),
        buttonDanger(inputId = inputIdOk, label = label_ok)
      )
    )
  )
}

#' Modal Dialog
#'
#' This function creates a modal dialog using the \code{modalDialog} function from the \code{shiny} package.
#'
#' @param ... arguments to be passed to the \code{modalDialog} function
#'
#' @examples
#' shiny::shinyApp(
#'   ui = bslib::page_fillable(
#'     theme = theme(),
#'     shiny::actionButton('show', 'Show modal dialog')
#'   ),
#'   server = function(input, output) {
#'     shiny::observeEvent(input$show, {
#'       shiny::showModal(modalDialog(
#'         title = 'Important message',
#'         icon = 'alert'
#'       ))
#'     })
#'   }
#' )
#'
modalDialog <- function(...) {
  args <- rlang::list2(...)

  if (is.character(args$icon)) {
    args$icon <- icon(args$icon, size = NULL)
  }

  if (is.null(args$size)) {
    args$size <- 's'
  }

  tag <- do.call(shiny::modalDialog, args)
  tag_qr <- htmltools::tagQuery(tag)

  if (length(args$icon)) {
    tag_qr$find('.modal-header')$prepend(
      shiny::column(2L, style = 'color: #337ab7;', icon(args$icon, size = 'l'))
    )
    tag_title <- tag_qr$find('.modal-title')$selectedTags()
    tag_qr$find('.modal-title')$replaceWith(
      shiny::column(10L, tag_title)
    )
  }

  # if (is.character(args$title)) {
  #   tag_qr$find('.modal-content')$prepend(div('oiu'))
  # }

  # if (!missing(size)) {
  #   checkmate::assertChoice(size, choices = c('sm', 'lg'))

  #   tag_qr <- tag_qr$addClass(glue::glue('btn-{size}'))
  # }

  # if (loading) {
  #   if (is.null(class)) class <- 'btn btn-default'
  #   shinyFeedback::loadingButton(inputId = inputId, label = shiny::HTML(as.character(label)), class = class, loadingLabel = 'Calculating')
  # } else {
  #   shiny::actionButton(..., class = class)
  # }

  tag_qr$allTags()
}

modalInfo <- function(
  inputIdOk,
  icon = 'information',
  msg = .('Hello.'),
  msg_info = NULL,
  msg_bold = TRUE,
  label_ok = .('Ok'),
  img_src = NULL
) {
  if (msg_bold) {
    msg <- sprintf('<b>%s</b>', msg)
  }
  if (is.defined(img_src)) {
    img <- shiny::img(src = img_src, width = '100%')
  } else {
    img <- NULL
  }
  shiny::showModal(
    shiny::modalDialog(
      shiny::fluidPage(
        shiny::fluidRow(
          style = 'width: 400px',
          shiny::column(2L, icon(name = icon, size = 'l', color = '#337ab7')),
          shiny::column(
            10L,
            shiny::p(shiny::HTML(msg)),
            shiny::p(shiny::HTML(msg_info))
          )
        ),
        img
      ),
      size = 's',
      footer = shiny::tagList(
        shiny::modalButton(label = label_ok)
      )
    )
  )
}

modalProgress <- function(session, min = 0L, max = 1L) {
  shiny::modalDialog(
    title = 'Upload surveillance data...',
    shiny::fluidPage(
      shiny::fluidRow(shiny::column(
        12L,
        shinyWidgets::progressBar(id = 'pgb', value = 0L, title = 'p')
      ))
    )
  )
}

modalWarning <- function(
  inputIdOk,
  icon = 'alert',
  msg = .('Are you sure.'),
  msg_info = NULL,
  msg_bold = TRUE,
  label_cancel = .('Cancel'),
  label_ok = .('Confirm')
) {
  if (msg_bold) {
    msg <- sprintf('<b>%s</b>', msg)
  }
  shiny::showModal(
    shiny::modalDialog(
      shiny::fluidPage(
        shiny::fluidRow(
          style = 'width: 400px',
          shiny::column(2L, style = 'color: #337ab7;', icon(icon, size = 'l')),
          shiny::column(
            10L,
            shiny::p(shiny::HTML(msg)),
            shiny::p(shiny::HTML(msg_info))
          )
        )
      ),
      size = 's',
      footer = shiny::tagList(
        shiny::modalButton(label = label_cancel),
        shinyFeedback::loadingButton(
          inputId = inputIdOk,
          label = label_ok,
          loadingLabel = sprintf(..('%s (Please wait...)'), label_ok),
          class = 'btn btn-primary'
        )
      )
    )
  )
}

# styler: block Shiny

css <- function(...) {
  do.call(htmltools::css, purrr::flatten(rlang::list2(...)))
}

disableTabPanel <- function(value) {
  shinyjs::disable(selector = sprintf('li:has(a[data-value=%s])', value))
  shinyjs::disable(selector = sprintf('li a[data-value=%s]', value))
}

disabledConditional <- function(condition, ui) {
  if (condition) {
    shinyjs::disabled(ui)
  } else {
    ui
  }
}

divInline <- function(..., width = NULL, style = 'vertical-align: top;') {
  args <- list(...)
  internal.style <- 'display: inline-block;'
  if (!is.null(width))
    internal.style <- sprintf('%s width: %spx;', internal.style, width)
  if (!is.null(style)) internal.style <- sprintf('%s %s', internal.style, style)
  div(style = internal.style, ...)
}

divStyled <- function(color, border, ...) {
  shiny::div(
    style = paste0(
      'align-items: center;',
      'background: ',
      color,
      '; ',
      'box-shadow: inset 0px 0px 0px 5px ',
      border,
      ';',
      # "min-height: 100px;",
      'display: flex;',
      'height: 100%;',
      'justify-content: center;'
    ),
    ...
  )
}

divTest <- function(...) {
  divStyled('#6babff', '#a6cdff', ...)
}

downloadLinkFile <- function(inputId, label = 'Download', class = NULL) {
  aTag <- shiny::tags$a(
    id = inputId,
    class = paste('btn btn-default shiny-download-link', class),
    href = '',
    target = '_blank',
    download = NA,
    icon('download'),
    label
  )
}

dropdownMenu <- function(label = NULL, icon = NULL, menu = NULL) {
  ul <- lapply(names(menu), function(id) {
    if (is.character(menu[[id]])) {
      shiny::tags$li(actionLink(id, menu[[id]]))
    } else {
      args <- menu[[id]]
      args$inputId <- id
      shiny::tags$li(do.call(actionLink, args))
    }
  })
  ul$class <- 'dropdown-menu'
  shiny::tags$div(
    class = 'dropdown',
    shiny::tags$button(
      class = 'btn btn-default dropdown-toggle',
      type = 'button',
      `data-toggle` = 'dropdown',
      label,
      `if`(!is.null(icon), icon, shiny::tags$span(class = 'caret'))
    ),
    do.call(shiny::tags$ul, ul)
  )
}

enableTabPanel <- function(value) {
  shinyjs::enable(selector = sprintf('li:has(a[data-value=%s])', value))
  shinyjs::enable(selector = sprintf('li a[data-value=%s]', value))
}

errorFunc <- function(err, buttonId) {
  errEl <- sprintf('[data-for-btn=%s] .btn-err', buttonId)
  errElMsg <- sprintf('[data-for-btn=%s] .btn-err-msg', buttonId)
  errMessage <- gsub('^ddpcr: (.*)', '\\1', err$message)
  shinyjs::html(html = errMessage, selector = errElMsg)
  shinyjs::show(selector = errEl, anim = TRUE, animType = 'fade')
}

fillModalDialog <- function(
  ...,
  title = NULL,
  footer = shiny::modalButton('Dismiss'),
  size = c('m', 's', 'l'),
  easyClose = FALSE,
  fade = TRUE
) {
  shiny::modalDialog(
    title = title,
    footer = footer,
    size = size,
    easyClose = easyClose,
    fade = fade,
    shiny::div(
      style = 'margin: -13px -15px -15px -15px',
      ...
    )
  )
}

fillPanel <- function(title, id, icon = NULL, ...) {
  shiny::tabPanel(
    title = title,
    value = id,
    icon = icon,
    shiny::div(
      class = 'outer',
      shiny::fillPage(
        shinyWidgets::chooseSliderSkin('Flat', color = '#112446'),
        ...
      )
    )
  )
}

fillTabPanel <- function(
  title,
  ...,
  value = title,
  icon = NULL,
  height = 100L,
  overflow = FALSE,
  style = NULL,
  id = NULL
) {
  if (is.defined(height)) {
    style <- paste0(
      style,
      sprintf('height: calc(100vh - %spx) !important;', height)
    )
  }
  if (overflow) {
    style <- paste0(style, 'overflow-y: auto;')
  }
  if (is.defined(style)) {
    div_content <- shiny::div(id = id, style = style, ...)
  } else {
    div_content <- shiny::div(id = id, ...)
  }
  shiny::tabPanel(
    title = title,
    value = value,
    icon = icon,
    shiny::div(
      class = 'inner',
      div_content
    )
  )
}

fillTabset <- function(id, ..., background_color = '#f0f0f0', padding = 5L) {
  style <- 'overflow: hidden;'
  if (is.defined(background_color)) {
    style <- paste0(style, sprintf('background-color: %s;', background_color))
  }
  if (is.defined(padding)) {
    style <- paste0(style, sprintf('padding: %spx;', padding))
  }
  shiny::div(
    style = style,
    shiny::tabsetPanel(id = id, type = 'tabs', ...)
  )
}

filterLayout <- function(filters, main) {
  verticalSplitLayout(
    left = shiny::tags$div(
      class = 'well',
      style = 'margin: 30px',
      filters
    ),
    right = main,
    lwidth = '370px'
  )
}

filterPanel <- function(...) {
  shiny::div(
    class = 'filter-bar',
    style = 'background-color: #fafafa; border: 1px solid silver;padding: 4px;white-space: nowrap;',
    divInline(
      htmltools::span(
        style = 'color: #DDDDDD;',
        shiny::icon('filter', 'fa-2x')
      ),
      style = 'margin-top: 12px; margin-right: 5px;'
    ),
    ...,
    shiny::img(
      src = 'assets/img/index.jpg',
      style = 'height: 46px; float: right',
      class = 'hide-on-800'
    )
  )
}

getURL <- function(session, url, file, progress = NULL) {
  r <- httr::GET(
    url = url,
    config = httr::config(noprogress = 0L, progressfunction = progress)
  )
  r.bin <- httr::content(r, 'raw')
  writeBin(r.bin, file)
}

guideSteps <- function(steps, session, ns) {
  shinyjs::delay(
    500L,
    rintrojs::introjs(
      session,
      options = list(
        steps = steps,
        doneLabel = ..('Done'),
        prevLabel = ..('Back'),
        nextLabel = ..('Next'),
        # skipLabel = ..('Skip'),
        exitOnOverlayClick = FALSE,
        overlayOpacity = 0L
      ),
      events = list(
        oncomplete = I(sprintf(
          'Shiny.onInputChange("%s", [Math.random()])',
          ns('intro_complete')
        )),
        onchange = I(sprintf(
          'Shiny.onInputChange("%s", [this._currentStep])',
          ns('intro_change')
        )),
        onbeforechange = I(sprintf(
          'Shiny.onInputChange("%s", [this._currentStep])',
          ns('intro_before_change')
        )),
        onafterchange = I(sprintf(
          'Shiny.onInputChange("%s", [this._currentStep])',
          ns('intro_after_change')
        ))
      )
    )
  )
}

icon <- function(
  name,
  class = NULL,
  size = 'xs',
  color = NULL,
  lib = 'mdi',
  ...
) {
  if (stringr::str_detect(name, '^[a-z]+:')) {
    m <- stringr::str_match_all(name, '^(.+):(.+)$')
    name <- m[[1L]][1L, 3L]
    lib <- m[[1L]][1L, 2L]
    lib <- switch(lib, fa = 'font-awesome', mdi = 'mdi')
  }
  if (lib == 'mdi') {
    icon_mdi(name = name, class = class, size = size, color = color)
  } else {
    if (name == 'home') {
      name <- 'house'
    }
    shiny::icon(name = name, class = class, lib = lib, ...)
  }
}

inline <- function(...) {
  shiny::div(class = 'form-inline', ...)
}

invalidate <- function(val) {
  val_new <- shiny::isolate(val()) + 1L
  val(val_new)
}

isEdge <- function(session) {
  val <- get0('HTTP_USER_AGENT', session$request, ifnotfound = '')
  grepl('edge', val, ignore.case = TRUE)
}

labelMandatory <- function(label) {
  shiny::tagList(
    label,
    shiny::span('*', class = 'mandatory_star')
  )
}

# div <- function(style = list(), bg = '#bcfbff', fill = FALSE) {
#   checkmate::assertList(style, unique = TRUE)
#   if (fill) {
#     style <- utils::modifyList(style, list(height = '100%'))
#   }
#   if (!missing(bg)) {
#     style <- utils::modifyList(style, list('background-color' = bg))
#   }
#   shiny::div(style = css(style))
# }
# styler: block NavBar

nav <- function(
  title,
  ...,
  value = title,
  icon = NULL,
  h100 = getOption('epi.h100', TRUE)
) {
  tag <- bslib::nav(title = title, ..., value = value, icon = icon)
  tag_qr <- htmltools::tagQuery(tag)
  if (h100) {
    tag_qr <- tag_qr$addClass('h-100')
  }
  tag_qr$allTags()
}

navItemLink <- function(href, text, icon_name, external = TRUE) {
  tag <- bslib::nav_item()
  tag_qr <- htmltools::tagQuery(tag)
  tag_qr$append(tags$a(href = href, text))
  if (external) {
    tag_qr$find('a')$addAttr(target = '_blank')
  }
  if (!missing(icon_name)) {
    tag_qr$find('a')$prepend(shiny::icon(icon_name))
  }
  tag_qr$allTags()
}

navItemSeparator <- function(href, text, icon_name, external = TRUE) {
  tag <- bslib::nav_item()
  tag_qr <- htmltools::tagQuery(tag)
  tag_qr$append(tags$div(class = 'dropdown-divider'))
  tag_qr$allTags()
}

navMenu <- function(...) {
  bslib::nav_menu(...)
}

navPanel <- function(..., fill = FALSE) {
  tag <- bslib::nav_panel(...)
  tag_qr <- htmltools::tagQuery(tag)
  if (fill) {
    tag_qr <- tag_qr$addClass('h-100')
  }
  tag_qr$allTags()
}

navs_tab_card <- function(
  ...,
  id = NULL,
  selected = NULL,
  header = NULL,
  footer = NULL,
  h100 = getOption('epi.h100', TRUE)
) {
  tag <- bslib::navs_tab_card(
    ...,
    id = id,
    selected = selected,
    header = header,
    footer = footer
  )
  tag_qr <- htmltools::tagQuery(tag)
  if (h100) {
    tag_qr <- tag_qr$addClass('h-100')
    tag_qr <- tag_qr$addClass('border-0')
    tag_qr <- tag_qr$children('.card-header')
    tag_qr <- tag_qr$addAttrs(style = 'background-color: var(--bs-gray-200)')
    tag_qr <- tag_qr$parent()
    tag_qr <- tag_qr$children('.card-body')
    tag_qr <- tag_qr$addClass('border-end')
    tag_qr <- tag_qr$addClass('border-start')
    tag_qr <- tag_qr$addClass('border-bottom')
    tag_qr <- tag_qr$find('.tab-content')
    tag_qr <- tag_qr$addClass('h-100')
  }
  tag_qr$allTags()
}

navsetCardPill <- function(..., fill = FALSE, header = TRUE) {
  tag <- bslib::navset_card_pill(...)
  tag_qr <- htmltools::tagQuery(tag)
  if (fill) {
    tag_qr <- tag_qr$addClass('h-100')
  }
  if (!header) {
    tag_qr <- tag_qr$find('div.card-header')
    tag_qr <- tag_qr$addAttrs(style = 'display: none')
    tag_qr <- tag_qr$parent()
    tag_qr <- tag_qr$find('div.tab-content')
    tag_qr <- tag_qr$addClass('border-0')
    tag_qr <- tag_qr$parent()
  }
  # if (h100) {
  #   tag_qr <- tag_qr$addClass('h-100')
  #   tag_qr <- tag_qr$addClass('border-0')
  #   tag_qr <- tag_qr$children('.card-header')
  #   tag_qr <- tag_qr$addAttrs(style = 'background-color: var(--bs-gray-200)')
  #   tag_qr <- tag_qr$parent()
  #   tag_qr <- tag_qr$children('.card-body')
  #   tag_qr <- tag_qr$addClass('border-end')
  #   tag_qr <- tag_qr$addClass('border-start')
  #   tag_qr <- tag_qr$addClass('border-bottom')
  #   tag_qr <- tag_qr$find('.tab-content')
  #   tag_qr <- tag_qr$addClass('h-100')
  # }
  tag_qr$allTags()
}

navsetCardTab <- function(..., h100 = getOption('epi.h100', TRUE)) {
  tag <- bslib::navset_card_tab(...)
  tag_qr <- htmltools::tagQuery(tag)
  # if (h100) {
  #   tag_qr <- tag_qr$addClass('h-100')
  #   tag_qr <- tag_qr$addClass('border-0')
  #   tag_qr <- tag_qr$children('.card-header')
  #   tag_qr <- tag_qr$addAttrs(style = 'background-color: var(--bs-gray-200)')
  #   tag_qr <- tag_qr$parent()
  #   tag_qr <- tag_qr$children('.card-body')
  #   tag_qr <- tag_qr$addClass('border-end')
  #   tag_qr <- tag_qr$addClass('border-start')
  #   tag_qr <- tag_qr$addClass('border-bottom')
  #   tag_qr <- tag_qr$find('.tab-content')
  #   tag_qr <- tag_qr$addClass('h-100')
  # }
  tag_qr$allTags()
}

navsetTab <- function(..., fill = FALSE) {
  tag <- bslib::navset_tab(...)
  tag_qr <- htmltools::tagQuery(tag)
  if (fill) {
    tag_qr <- tag_qr$addClass('h-100')
  }
  tag_qr$allTags()
}

#' Page Navbar
#'
#' Create a Bootstrap navbar for a page.
#'
#' @inheritDotParams bslib::page_navbar
#'
#' @return A `tag` object representing the page navbar.
#'
#' @examples
#' pageNavbar(title = 'My Page', bg = 'light')
#'
pageNavbar <- function(..., shadow = TRUE) {
  tag_page <- bslib::page_navbar(
    collapsible = TRUE,
    fillable = TRUE,
    fillable_mobile = TRUE,
    inverse = FALSE,
    # lang = NULL,
    # gap = 0
    # padding = 1L,
    position = 'static-top',
    theme = theme(),
    ...
  )
  # tag <- bslib::page_navbar(...,
  #   title = title, id = id, selected = selected, position = position,
  #   header = header, footer = footer, bg = bg, inverse = inverse, collapsible = collapsible,
  #   fluid = fluid, theme = theme, window_title = window_title, lang = lang
  # )
  # if (h100) {
  #   tag <- bslib::page_fill(tag)
  # }
  tag_qr <- htmltools::tagQuery(tag_page)
  tag_qr <- tag_qr$find('nav')
  # tag_qr <- tag_qr$addClass('py-0')
  if (shadow) {
    tag_qr <- tag_qr$addClass('shadow-sm')
  }
  # if (h100) {
  #   tag_qr <- tag_qr$siblings('.container-fluid')
  #   tag_qr <- tag_qr$addClass('h-100')
  #   tag_qr <- tag_qr$addClass('g-0')
  #   tag_qr <- tag_qr$addAttrs(style = 'padding-top: 43px')
  #   tag_qr <- tag_qr$children('.tab-content')
  #   tag_qr <- tag_qr$addClass('h-100')
  #   tag_qr <- tag_qr$addClass('p-5')
  #   tag_qr <- tag_qr$addAttrs(style = 'background-color: var(--bs-gray-200)')
  # }
  # tag_qr$allTags()
  #   tag_qr <- tag_qr$addClass('shadow-sm')
  # }
  # if (h100) {
  #   tag_qr <- tag_qr$siblings('.container-fluid')
  #   tag_qr <- tag_qr$addClass('h-100')
  #   tag_qr <- tag_qr$addClass('g-0')
  #   tag_qr <- tag_qr$addAttrs(style = 'padding-top: 43px')
  #   tag_qr <- tag_qr$children('.tab-content')
  #   tag_qr <- tag_qr$addClass('h-100')
  #   tag_qr <- tag_qr$addClass('p-5')
  #   tag_qr <- tag_qr$addAttrs(style = 'background-color: var(--bs-gray-200)')
  # }

  tag_qr$allTags()
}

#' Page Navbar
#'
#' Create a Bootstrap navbar for a page.
#'
#' @inheritDotParams bslib::page_navbar
#'
#' @return A `tag` object representing the page navbar.
#'
#' @examples
#' pageNavbar(title = 'My Page', bg = 'light')
#'
page_navbar <- function(
  ...,
  title = NULL,
  id = NULL,
  selected = NULL,
  position = c('static-top', 'fixed-top', 'fixed-bottom'),
  header = NULL,
  footer = NULL,
  bg = NULL,
  inverse = 'auto',
  collapsible = TRUE,
  fluid = TRUE,
  theme = bslib::bs_theme(),
  window_title = NA,
  lang = NULL,
  h100 = getOption('epi.h100', TRUE),
  shadow = TRUE
) {
  tag <- bslib::page_navbar(
    ...,
    title = title,
    id = id,
    selected = selected,
    position = position,
    header = header,
    footer = footer,
    bg = bg,
    inverse = inverse,
    collapsible = collapsible,
    fluid = fluid,
    theme = theme,
    window_title = window_title,
    lang = lang
  )
  if (h100) {
    tag <- bslib::page_fill(tag)
  }
  tag_qr <- htmltools::tagQuery(tag)
  tag_qr <- tag_qr$siblings('.navbar')
  tag_qr <- tag_qr$addClass('py-0')
  if (shadow) {
    tag_qr <- tag_qr$addClass('shadow-sm')
  }
  if (h100) {
    tag_qr <- tag_qr$siblings('.container-fluid')
    tag_qr <- tag_qr$addClass('h-100')
    tag_qr <- tag_qr$addClass('g-0')
    tag_qr <- tag_qr$addAttrs(style = 'padding-top: 43px')
    tag_qr <- tag_qr$children('.tab-content')
    tag_qr <- tag_qr$addClass('h-100')
    tag_qr <- tag_qr$addClass('p-5')
    tag_qr <- tag_qr$addAttrs(style = 'background-color: var(--bs-gray-200)')
  }
  tag_qr$allTags()
}

# div <- function(style = list(), bg = '#bcfbff', fill = FALSE) {
#   checkmate::assertList(style, unique = TRUE)
#   if (fill) {
#     style <- utils::modifyList(style, list(height = '100%'))
#   }
#   if (!missing(bg)) {
#     style <- utils::modifyList(style, list('background-color' = bg))
#   }
#   shiny::div(style = css(style))
# }

# styler: block Misc

hidden <- function(tag, hide = TRUE) {
  if (hide) {
    tag <- shinyjs::hidden(tag)
  }
  tag
}

inputGroup <- function(...) {
  shiny::div(class = 'input-group', ...)
}

observeResize <- function(session, project, mod_id, callback) {
  # shiny::observe({
  #   tab <- project$view
  #   if (is.null(tab)) {
  #     return()
  #   }
  #   if (tab != mod_id) {
  #     return()
  #   }
  #   callback(session)
  # })
}

page_fill <- function(
  ...,
  padding = 0L,
  title = NULL,
  theme = bslib::bs_theme(),
  lang = NULL
) {
  bslib::page_fill(
    ...,
    padding = padding,
    title = title,
    theme = theme,
    lang = lang
  )
}

panel <- function(..., style_ex = NULL) {
  style <- 'background-color: #fafafa; width: 100%; height: 100%; border-right: #c9c9c9 1px solid;'
  if (is.defined(style_ex)) {
    style <- paste(style, style_ex, collapse = ';')
  }
  shiny::div(
    style = style,
    ...
  )
}

panelLeaflet <- function(
  ...,
  fixed = TRUE,
  draggable = TRUE,
  top = 104L,
  left = 'auto',
  right = 5L,
  bottom = 'auto',
  width = 330L,
  height = 'auto'
) {
  shiny::absolutePanel(
    class = 'leaflet-panel',
    fixed = fixed,
    draggable = draggable,
    top = top,
    left = left,
    right = right,
    bottom = bottom,
    width = width,
    height = height,
    ...
  )
}

panelLoading <- function(id) {
  shiny::absolutePanel(
    id = id,
    top = '45%',
    bottom = '45%',
    left = '45%',
    right = '45%',
    fixed = FALSE,
    style = 'opacity: 1; z-index: 10;',
    shiny::img(src = 'assets/img/ajax-loader.gif')
  )
}

reactiveTrigger <- function() {
  counter <- shiny::reactiveVal(0L)
  list(
    depend = function() {
      if (!is.null(shiny::getDefaultReactiveDomain())) {
        counter()
      }
      invisible()
    },
    trigger = function() {
      counter(shiny::isolate(counter()) + 1L)
    }
  )
}

splash <- function(img = 1L, ...) {
  shiny::absolutePanel(
    class = 'splash',
    style = sprintf('background-image: url("assets/img/splash-%s.jpg");', img),
    ...
  )
}

toggle <- function(..., visible = TRUE) {
  if (visible) {
    shinyjs::show(...)
  } else {
    shinyjs::hide(...)
  }
}

useEnter <- function(ns) {
  shiny::tags$script(sprintf(
    '$(document).on("keyup", function(e) {if(e.keyCode == 13) {Shiny.onInputChange("%s", Math.random());}});',
    ns('keyPressed')
  ))
}

#' Add JS code Straight to your app (instead of a dependency)
#'
#' This function can be used in your UI to insert directly the JavaScript
#' functions
#'
#' @details These functions are meant to be used with `session$sendCustomMessage`
#'     from the server side.
#'
#' \describe{
#'   \item{showid}{Show an element with the id provided.}
#'   \item{hideid}{Hide an element with the id provided.}
#'   \item{showclass}{Same as showid, but with class.}
#'   \item{hideclass}{Same as hideid, but with class.}
#'   \item{showhref}{Same as showid, but with `a[href*=`}
#'   \item{hidehref}{Same as hideid, but with `a[href*=`}
#'   \item{clickon}{Click on an element. The full Jquery selector has to be used.}
#' }
#'
#' @return A script
#'
useEvents <- function() {
  htmltools::includeScript(
    fs::path_package(package = utils::packageName(), 'assets/js/event.js')
  )
}

useIntro <- function() {
  # bug with introjs (sub replacement utf 8)
  # rintrojs::introjsUI()
  shiny::tags$head(
    shiny::singleton(
      shiny::tagList(
        shiny::HTML(
          '
          <script src="https://cdn.jsdelivr.net/npm/intro.js@7.2.0/minified/intro.min.js"></script>
          <link  href="https://cdn.jsdelivr.net/npm/intro.js@7.2.0/minified/introjs.min.css" rel="stylesheet"/>'
        ),
        shiny::includeScript(system.file(
          fs::path('javascript', 'rintro.js'),
          package = 'rintrojs'
        ))
      )
    )
  )
}

useKeys <- function() {
  keys::useKeys()
}

useShinyFeedback <- function() {
  shinyFeedback::useShinyFeedback()
}

useShinyjs <- function() {
  shinyjs::useShinyjs()
}

#' Use Waiter and Waitress in Shiny app
#'
#' This function integrates Waiter and Waitress into a Shiny application.
#'
#' @return A tag list containing Waiter and Waitress components for a Shiny app.
#'
#' @examples
#' useWaiter()
#'
useWaiter <- function(particle = FALSE) {
  if (particle) {
    # shiny::tagList(
    #   waiter::useHostess(),
    #   waiter::useWaiter(),
    #   waiter::useWaitress(),
    #   waiter::waiterPreloader(
    #     html = tags$div(
    #       tags$div(
    #         id = 'particles-target',
    #         style = 'position: absolute; top: 0; bottom: 0; right: 0; left: 0;',
    #         div(style = 'max-width: fit-content; margin-left: auto; margin-right: auto; background-color: red')
    #       ),
    #       div(
    #         style = '
    #           max-width: fit-content;
    #           margin-left: auto;
    #           margin-right: auto;
    #           background-color: #777777;
    #           opacity: 0.1;
    #           padding: 10px;
    #           border-radius: 10px;',
    #         shiny::h3('Please wait...')
    #       )
    #       # waiter::spin_loaders(7L, color = '#ffffff'),
    #     ),
    #     color = 'white',
    #     image = '',
    #     fadeout = FALSE,
    #     logo = ''
    #   ),
    #   particlesjs::particles(
    #     config = particlesjs::particles_config(
    #       particles.color.value = '#0096ff',
    #       particles.shape.type = 'circle',
    #       particles.size.value = 3L,
    #       particles.size.random = TRUE,
    #       particles.line_linked.color = '#0096ff',
    #       interactivity.events.onhover.mode = 'bubble',
    #       interactivity.events.onclick.mode = 'repulse',
    #       interactivity.modes.bubble.distance = 100L,
    #       interactivity.modes.bubble.size = 1L,
    #       interactivity.modes.bubble.duration = 2L,
    #       interactivity.modes.bubble.opacity = 0L,
    #       interactivity.modes.bubble.speed = 3L
    #     ),
    #     target_id = 'particles-target',
    #     element_id = 'particles'
    #   )
    # )
  } else {
    shiny::tagList(
      waiter::useWaiter(),
      waiter::useWaitress(),
      waiter::waiterShowOnLoad(
        html = shiny::tagList(
          waiter::spin_loaders(7L, color = '#ffffff'),
          shiny::h3('Please wait...')
        ),
        color = 'white',
        logo = ''
      )
    )
  }
}

verticalSplitLayout <- function(left, right, lwidth = '370px') {
  shiny::bootstrapPage(
    shiny::tags$head(shiny::tags$style(
      type = 'text/css',
      'html, body {width: 100%; height: 100%; overflow: hidden}'
    )),
    shiny::tags$div(
      style = 'position: absolute; top: 50px; left: 0; right: 0; bottom: 0',
      shiny::tags$div(
        style = paste(
          'position: absolute; top: 0; left: 0; width:',
          lwidth,
          '; bottom: 0; overflow: auto'
        ),
        left
      ),
      shiny::tags$div(
        style = paste(
          'position: absolute; top: 0; left:',
          lwidth,
          '; right: 0; bottom: 0; overflow: auto'
        ),
        right
      )
    )
  )
}

wellPanel <- function(..., h100 = getOption('epi.h100', TRUE)) {
  tag <- shiny::wellPanel(...)
  tag_qr <- htmltools::tagQuery(tag)
  if (h100) {
    tag_qr <- tag_qr$addClass('h-100')
  }
  tag_qr$allTags()
}

wellPanelFill <- function(...) {
  shiny::wellPanel(style = 'height: 100%; padding: 2px', ...)
}

wellPanelInfo <- function(..., title = 'Title', color = '#D6E8F7') {
  shiny::tagList(
    shiny::wellPanel(
      style = sprintf(
        '
        color: #3F3F3F;
        background-color: %s;
        border-top: 2px solid transparent;
        border-radius: 5px;
        padding: 10px;
        font-size: smaller;',
        color
      ),
      shiny::tags$p(class = 'intro', title, style = 'font-size: 14px'),
      ...
    )
  )
}

withBusyIndicatorServer <- function(buttonId, expr) {
  # UX stuff: show the 'busy' message, hide the other messages, disable the button
  loadingEl <- sprintf('[data-for-btn=%s] .btn-loading-indicator', buttonId)
  doneEl <- sprintf('[data-for-btn=%s] .btn-done-indicator', buttonId)
  errEl <- sprintf('[data-for-btn=%s] .btn-err', buttonId)
  shinyjs::disable(buttonId)
  shinyjs::show(selector = loadingEl)
  shinyjs::hide(selector = doneEl)
  shinyjs::hide(selector = errEl)
  on.exit({
    shinyjs::enable(buttonId)
    shinyjs::hide(selector = loadingEl)
  })

  # Try to run the code when the button is clicked and show an error message if
  # an error occurs or a success message if it completes
  tryCatch(
    {
      value <- expr
      shinyjs::show(selector = doneEl)
      shinyjs::delay(
        2000L,
        shinyjs::hide(
          selector = doneEl,
          anim = TRUE,
          animType = 'fade',
          time = 0.5
        )
      )
      value
    },
    error = function(err) {
      errorFunc(err, buttonId)
    }
  )
}

withBusyIndicatorUI <- function(button) {
  id <- button[['attribs']][['id']]
  shiny::div(
    `data-for-btn` = id,
    button,
    shiny::span(
      class = 'btn-loading-container',
      shinyjs::hidden(
        icon('spinner', class = 'btn-loading-indicator fa-spin'),
        icon('check', class = 'btn-done-indicator')
      )
    ),
    shinyjs::hidden(
      shiny::div(
        class = 'btn-err',
        shiny::div(
          icon('exclamation-circle'),
          shiny::tags$b('Error: '),
          shiny::span(class = 'btn-err-msg')
        )
      )
    )
  )
}

withProgressDialog <- function(
  expr,
  min = 0L,
  max = 1L,
  value = min + (max - min) * 0.1,
  message = NULL,
  detail = NULL,
  session = shiny::getDefaultReactiveDomain(),
  env = parent.frame(),
  quoted = FALSE
) {
  if (!quoted) {
    expr <- substitute(expr)
  }
  if (is.null(session$progressStack)) {
    stop('"session" is not a ShinySession object.')
  }
  p <- modalProgress(session, min = min, max = max)
  shiny::showModal(p)
  session$progressStack$push(p)
  on.exit({
    session$progressStack$pop()
    shiny::removeModal()
  })
  # p$set(value, message, detail)
  eval(expr, env)
}
