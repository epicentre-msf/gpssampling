# styler: block Theme

#' shinyAppMinimal
#'
#' @param ui A shiny.tag.list object representing the UI elements of the Shiny app.
#' @param server A function representing the server logic of the Shiny app.
#' @param theme A character string specifying the theme of the app. Choices include 'bootstrap_3', 'bootstrap_5', and 'epicentre'.
#' @param port An integer specifying the port number for running the Shiny app.
#'
#' @return NULL
#'
shinyAppMinimal <- function(ui, server = NULL, theme = 'epicentre', port = 8000L) {
  checkmate::assertClass(ui, classes = 'shiny.tag.list')
  checkmate::assertFunction(server, nargs = 0L, null.ok = TRUE)
  checkmate::assertChoice(theme, choices = c('bootstrap_3', 'bootstrap_5', 'epicentre'))

  ui <- switch(theme,
    bootstrap_3 = shiny::fillPage(useWaiter(), ui, theme = bslib::bs_theme(version = 3L)),
    bootstrap_5 = bslib::page_fillable(useWaiter(), ui, theme = bslib::bs_theme(version = 5L)),
    epicentre = bslib::page_fillable(useWaiter(), ui, theme = theme())
  )

  if (is.null(server)) {
    server <- function() {}
  }

  shiny::shinyApp(
    ui = ui,
    server = function(input, output, session) {
      session$onSessionEnded(
        function() {
          shiny::stopApp()
        }
      )

      bslib::bs_themer(gfonts = TRUE, gfonts_update = FALSE)

      server()

      waiter::waiter_hide()
    },
    options = list(port = port)
  )
}

#' Theme function for customizing Bootstrap themes
#'
#' This function allows customization of Bootstrap themes by adjusting various elements such as fonts, colors, buttons, and cards.
#'
#' @param preview Logical indicating whether to preview the theme changes (default is FALSE)
#'
#' @return A customized Bootstrap theme object
#'
#' @examples
#' theme()
#'
theme <- function(preview = FALSE) {
  #   <link rel="preconnect" href="https://fonts.googleapis.com">
  # <link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
  # <link href="https://fonts.googleapis.com/css2?family=Fira+Sans:ital,wght@0,100;0,200;0,300;0,400;0,500;0,600;0,700;0,800;0,900;1,100;1,200;1,300;1,400;1,500;1,600;1,700;1,800;1,900&display=swap" rel="stylesheet">
  theme_font <- bslib::font_collection(bslib::font_google('Fira+Sans', wght = 1:9 * 100L, ital = 0:1), 'Roboto', 'Helvetica', 'Arial', 'sans-serif')

  theme <- bslib::bs_theme(
    preset = 'bootstrap',
    spacer = '0.5rem',
    # 'modal-padding' = '0rem',
    # 'modal-header-padding' = '0rem',
    # 'modal-header-padding-y' = '0rem',
    # 'navbar-padding-y' = '0.25rem',

    # Typography
    base_font = theme_font,
    code_font = theme_font,
    heading_font = theme_font,
    'font-size-base' = '0.9rem',
    'font-weight-normal' = '400',

    # # Controls the default grayscale palette
    bg = '#FFFFFF',
    fg = '#474747',
    light = '#FFFFFF',

    # # Controls the accent (e.g., hyperlink, button, etc) colors
    primary = '#dbe5f0',
    # secondary = '#c03939',
    # Can also add lower-level customization
    # 'input-border-color' = '#EA80FC'
    # 'bs-body-color' = 'rgb(119, 119, 119)'

    # Button
    # `btn-color` = '#f2f2f2',
    # `btn-link-hover-color` = '#e7e7e7',
    `btn-white-space` = 'nowrap',

    # Options
    `enable-gradients` = TRUE,
    `enable-shadows` = TRUE,
    `link-color` = 'rgb(51, 122, 183)',
    `modal-sm` = '400px'
  )

  # Lower-level bs_add_*() functions allow you to work more
  # directly with the underlying Sass code
  # theme <- bslib::bs_add_variables(theme, 'spacer' = '0rem', .where = "declarations")
  # theme <- bslib::bs_add_variables(theme, 'navbar-padding-y' = '$spacer / 4', .where = "declarations") # $spacer / 2

  theme <- bslib::bs_add_rules(theme, 'body {background-color: #FAFAFA}')

  theme <- bslib::bs_add_rules(theme, '.form-label-sm {margin-bottom: 0.2rem !important;}')

  # theme <- bslib::bs_add_rules(theme, '.vscomp-dropbox-container {z-index: 10000 !important;}')
  theme <- bslib::bs_add_rules(theme, '
    .vscomp-toggle-button {
      border: 1px solid var(--bs-border-color) !important;
      border-radius: var(--bs-border-radius);
    }')
  theme <- bslib::bs_add_rules(theme, '
    .vscomp-wrapper.focused .vscomp-toggle-button, .vscomp-wrapper:focus .vscomp-toggle-button {
      border-color: #86b7fe !important;
      box-shadow: inset 0 1px 2px rgba(71, 71, 71, 0.075), 0 0 0 0.25rem rgba(13, 110, 253, 0.25) !important;
    }')

  # Button rules
  theme <- bslib::bs_add_rules(theme, '
    .btn-default {
      --bs-btn-bg: #f2f2f2;
      --bs-btn-hover-bg: #e7e7e7;
      --bs-btn-active-bg: #dbe5f0;
      --bs-btn-color: #5b5b5b;
      --bs-btn-hover-color: #5b5b5b;
      --bs-btn-active-color: #1d3d63;
      border: none !important;
      box-shadow: none !important;
    }

    .btn-xs {
      --bs-btn-padding-y: .15rem;
      --bs-btn-padding-x: .25rem;
      --bs-btn-font-size: .675rem;
      --bs-btn-border-radius: var(--bs-border-radius-sm)
    }
  ')

  # Card rules
  theme <- bslib::bs_add_rules(theme, '
    div.card {
      border-radius: 0px;
    }

    div.card:has(div.card-header > ul.nav) {
      --bs-card-border-color: none;
    }

    div.card-header:has(ul.nav) {
      --bs-card-border-color: var(--bs-border-color);
    }

    div.card:has(div.card-header > ul.nav) > div.tab-content {
      border: solid var(--bs-border-width) var(--bs-border-color);
      border-top: none;
    }

    .nav {
      --bs-nav-link-padding-x: 0.5rem;
      --bs-nav-link-padding-y: 0.25rem;
    }
  ')

  if (preview) {
    bslib::bs_theme_preview(theme)
  }

  theme
}

themeGallery <- function() {
  tags <- shinyWidgets::pickerInput(
    inputId = 'groups',
    label = 'Select one from each group below:',
    choices = list(
      Group1 = c('1', '2', '3', '4'),
      Group2 = c('A', 'B', 'C', 'D')
    ),
    multiple = TRUE,
    options = list('max-options-group' = 1L)
  )
  themePreview(tags)
}

themePreview <- function(..., reset = TRUE) {
  if (reset) {
    path_cache <- sass::sass_cache_context_dir()
    if (fs::dir_exists(path_cache)) {
      fs::dir_delete(path_cache)
    }
  }
  dots <- rlang::list2(...)
  if (length(dots)) {
    shiny::shinyApp(
      ui = bslib::page_fillable(
        theme = theme(),
        shiny::wellPanel(
          ...
        )
      ),
      server = function(input, output) {}
    )
  } else {
    bslib::bs_theme_preview(theme(), with_themer = FALSE)
  }
}

# themePreview(actionButton("largeButton", "Large Primary Button", class = "btn-primary btn-lg"))
# themePreview(actionButton("largeButton", "Large Primary Button", class = "btn-primary btn-light"))

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

buttonDownload <- function(outputId, label = .('Download'), icon = 'download', class = NULL, ...) {
  if (is.defined(icon)) icon <- icon(icon, size = NULL)
  shiny::downloadButton(outputId = outputId, label = label, icon = icon, class = class, ...)
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
buttonLoading <- function(..., margin = 2L, semantic = 'default', size = NULL, hide = FALSE, outline = FALSE) {
  args <- rlang::list2(...)

  checkmate::assertChoice(semantic, choices = c('default', 'primary', 'secondary', 'success', 'danger', 'warning', 'info', 'light', 'dark', 'link'))
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
  checkmate::checkChoice(align_self, choices = c('start', 'end', 'center', 'baseline', 'stretch'), null.ok = TRUE)

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

#' @examples
#' shiny::shinyApp(
#'   ui = shiny::basicPage(
#'     shiny::actionButton('show', 'Show modal dialog')
#'   ),
#'   server = function(input, output) {
#'     shiny::observeEvent(input$show, {
#'       shiny::showModal(modalDanger('rt'))
#'     })
#'   }
#' )
#'
modalDanger <- function(inputIdOk,
                        icon = 'alert',
                        msg = 'Are you sure.',
                        msg_info = NULL,
                        msg_bold = TRUE,
                        label_cancel = 'Cancel',
                        label_ok = 'Confirm') {
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

modalInfo <- function(inputIdOk,
                      icon = 'information',
                      msg = .('Hello.'),
                      msg_info = NULL,
                      msg_bold = TRUE,
                      label_ok = .('Ok'),
                      img_src = NULL) {
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
      shiny::fluidRow(shiny::column(12L, shinyWidgets::progressBar(id = 'pgb', value = 0L, title = 'p')))
    )
  )
}

modalWarning <- function(inputIdOk,
                         icon = 'alert',
                         msg = .('Are you sure.'),
                         msg_info = NULL,
                         msg_bold = TRUE,
                         label_cancel = .('Cancel'),
                         label_ok = .('Confirm')) {
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
        shinyFeedback::loadingButton(inputId = inputIdOk, label = label_ok, loadingLabel = sprintf(..('%s (Please wait...)'), label_ok), class = 'btn btn-primary')
      )
    )
  )
}

# styler: block pickerInput

observeEventPickerInputExSpin <- function(session, input, vars, inputId, choices) {
  shiny::observeEvent(input[[inputId]], {
    vars[[inputId]] <- input[[inputId]]
  })

  shiny::observeEvent(input[[paste0(inputId, '_previous')]], {
    choices <- unlist(choices)
    choice.previous.idx <- max(1L, match(vars[[inputId]], choices) - 1L)
    choice.previous <- choices[choice.previous.idx]
    updatePickerInputEx(session, inputId = inputId, selected = choice.previous)
  })

  shiny::observeEvent(input[[paste0(inputId, '_next')]], {
    choices <- unlist(choices)
    choice.next.idx <- min(length(choices), match(vars[[inputId]], choices) + 1L)
    choice.next <- choices[choice.next.idx]
    updatePickerInputEx(session, inputId = inputId, selected = choice.next)
  })

  observe({
    choices <- as.character(unlist(choices))
    shinyjs::toggleState(id = paste0(inputId, '_previous'), condition = vars[[inputId]] != choices[1L])
    shinyjs::toggleState(id = paste0(inputId, '_next'), condition = vars[[inputId]] != choices[length(choices)])
  })
}

pickerInput <- function(..., buttons = NULL, size = NULL, hide = FALSE) {
  args <- rlang::list2(...)

  checkmate::assertList(buttons, null.ok = TRUE)
  checkmate::assertChoice(size, null.ok = TRUE, choices = c('lg', 'sm', 'xs'))
  checkmate::assertFlag(hide)

  tag <- shinyWidgets::pickerInput(...)
  # tag <- shiny::div(class = 'rounded', tag)
  tag_qr <- htmltools::tagQuery(tag)

  if (!is.null(size)) {
    tag_qr <- tag_qr$find('select')
    tag_qr <- tag_qr$addClass(glue::glue('form-control-{size}'))
    tag_qr <- tag_qr$parent()
    if (size == 'sm') {
      tag_qr <- tag_qr$find('label')
      tag_qr$replaceWith(tags$small(tag_qr$selectedTags()))
      tag_qr <- tag_qr$parent()
    }
  } else {
    size <- NULL
  }

  if (!missing(buttons)) {
    buttons <- lapply(
      buttons,
      function(btn) {
        button(inputId = paste0(args$inputId, '_', btn$id), size = size, icon = btn$icon, label = NULL)
      }
    )
    tag_qr <- tag_qr$find('select')
    tag_qr$replaceWith(tags$div(class = 'input-group', buttons, tag_qr$selectedTags()))
  }

  hidden(tag_qr$allTags(), hide = hide)
}

pickerInputEx <- function(inputId, label, choices, selected = NULL, multiple = FALSE,
                          options = list(), choicesOpt = NULL, width = NULL, inline = FALSE,
                          size = 'normal', buttons = list()) {
  size <-
    match.arg(
      arg = size,
      choices = c('xs', 'sm', 'normal', 'lg')
    )
  choices <- shinyWidgets:::choicesWithNames(choices) # nolint: undesirable_operator_linter.
  selected <- restoreInput(id = inputId, default = selected)
  if (!is.null(options) && length(options) > 0L) {
    names(options) <- paste('data', names(options), sep = '-')
  }
  if (!is.null(width)) {
    options <- c(options, list('data-width' = width))
  }
  if (!is.null(width) && width %in% c('fit')) {
    width <- NULL
  }
  options <- lapply(options, function(x) {
    if (identical(x, TRUE)) {
      'true'
    } else if (identical(x, FALSE)) {
      'false'
    } else {
      x
    }
  })
  maxOptGroup <- options[['data-max-options-group']]

  if (size == 'normal') {
    divInputGroupClass <- 'input-group'
  } else {
    divInputGroupClass <- paste0('input-group input-group-', size)
  }

  selectTag <- tag('select', dropNulls(options))
  selectTag <- tagAppendAttributes(
    tag = selectTag,
    id = inputId,
    class = 'selectpicker form-control'
  )
  selectTag <- tagAppendChildren(
    tag = selectTag, pickerSelectOptions(choices, selected, choicesOpt, maxOptGroup)
  )

  if (length(buttons)) {
    buttons <- lapply(buttons, function(btn) {
      htmltools::tags$button(
        id = paste0(inputId, '_', btn$id), type = 'button', class = 'btn btn-default action-button btn-input',
        htmltools::tags$i(class = paste0('fa fa-', btn$icon))
      )
    })

    selectTag <-
      htmltools::tags$div(
        class = divInputGroupClass,
        htmltools::tags$div(class = 'input-group-btn', buttons),
        selectTag
      )
  }

  if (multiple) {
    selectTag$attribs$multiple <- 'multiple'
  }

  if (size == 'normal') {
    divClass <- 'shiny-input-container form-group'
  } else {
    divClass <- paste0('shiny-input-container form-group form-group-', size)
  }

  if (inline) {
    divClass <- paste(divClass, 'shiny-input-container-inline')
  } else {
    divClass <- paste(divClass, 'shiny-input-container')
  }


  labelClass <- 'control-label'

  if (inline) {
    divClass <- paste(divClass, 'form-horizontal')
    selectTag <- tags$div(class = 'col-sm-10', selectTag)
    labelClass <- paste(labelClass, 'col-sm-2')
  }

  labelTag <- htmltools::tags$label(class = labelClass, `for` = inputId, label)

  if (size == 'sm') {
    labelTag <- htmltools::tags$small(labelTag)
  }

  pickerTag <- tags$div(
    class = divClass,
    style = if (!is.null(width)) paste0('width: ', validateCssUnit(width), ';'),
    if (!is.null(label)) {
      labelTag
    },
    if (!is.null(label) & !inline) {
      htmltools::tags$br()
    },
    selectTag
  )
  if (inline) {
    pickerTag <- htmltools::tags$form(class = 'form-horizontal', pickerTag)
  }
  # Deps
  shinyWidgets:::attachShinyWidgetsDep(pickerTag, 'picker') # nolint: undesirable_operator_linter.
}

pickerInputExPalette <- function(...) {
  args <- list(...)
  pickerInputEx(...,
    choices    = getChoicePalettes(),
    choicesOpt = list(content = sprintf('shiny::div(style="width:100%%;padding:2px;border-radius:4px;background:%s">&nbsp;</div>', unname(getColorPalettes())))
  )
}

pickerInputExSpin <- function(...) {
  pickerInputEx(..., buttons = list(
    list(id = 'previous', icon = 'caret-left'),
    list(id = 'next', icon = 'caret-right')
  ))
}

pickerInputExSpinRight <- function(...) {
  args <- list(...)
  div(
    class = 'input-group', id = paste0(args$inputId, '_div'),
    pickerInputEx(...),
    span(
      class = 'input-group-btn',
      actionButton(inputId = paste0(args$inputId, '_previous'), label = '', icon('caret-left'), width = 25L, class = 'btn-input'),
      actionButton(inputId = paste0(args$inputId, '_next'), label = '', icon('caret-right'), width = 25L, class = 'btn-input')
    )
  )
}

pickerSelectOptions <- function(choices, selected = NULL, choicesOpt = NULL, maxOptGroup = NULL) {
  if (is.null(choicesOpt) & is.null(maxOptGroup)) {
    return(selectOptions(choices, selected))
  }
  if (is.null(choicesOpt)) {
    choicesOpt <- list()
  }
  l <- sapply(choices, length)
  if (!is.null(maxOptGroup)) {
    maxOptGroup <- rep_len(x = maxOptGroup, length.out = sum(l))
  }
  m <- matrix(data = c(c(1L, cumsum(l)[-length(l)] + 1L), cumsum(l)), ncol = 2L)
  html <- lapply(seq_along(choices), FUN = function(i) {
    label <- names(choices)[i]
    choice <- choices[[i]]
    if (is.list(choice)) {
      tags$optgroup(
        label = htmltools::htmlEscape(label, TRUE),
        `data-max-options` = if (!is.null(maxOptGroup)) maxOptGroup[i],
        pickerSelectOptions(
          choice, selected,
          choicesOpt = lapply(
            X = choicesOpt,
            FUN = function(j) {
              j[m[i, 1L]:m[i, 2L]]
            }
          )
        )
      )
    } else {
      tags$option(
        value = choice,
        shiny::HTML(htmltools::htmlEscape(label)),
        style = choicesOpt$style[i],
        `data-icon` = choicesOpt$icon[i],
        `data-subtext` = choicesOpt$subtext[i],
        `data-content` = choicesOpt$content[i],
        `data-tokens` = choicesOpt$tokens[i],
        disabled = if (!is.null(choicesOpt$disabled[i]) && choicesOpt$disabled[i]) 'disabled',
        selected = if (choice %in% selected) 'selected' else NULL
      )
    }
  })
  tagList(html)
}

selectOptions <- function(choices, selected = NULL) {
  html <- mapply(choices, names(choices), FUN = function(choice, label) {
    if (is.list(choice)) {
      sprintf(
        '<optgroup label="%s">\n%s\n</optgroup>',
        htmltools::htmlEscape(label, TRUE),
        selectOptions(choice, selected)
      )
    } else {
      sprintf(
        '<option value="%s"%s>%s</option>',
        htmltools::htmlEscape(choice, TRUE),
        if (choice %in% selected) ' selected' else '',
        htmltools::htmlEscape(label)
      )
    }
  })
  shiny::HTML(paste(html, collapse = '\n'))
}

updatePickerInputEx <- function(session, inputId, label = NULL,
                                selected = NULL, choices = NULL,
                                choicesOpt = NULL) {
  choices <- if (!is.null(choices)) {
    shinyWidgets:::choicesWithNames(choices)
  } # nolint: undesirable_operator_linter.
  if (!is.null(selected)) {
    selected <- shinyWidgets:::validateSelected(selected, choices, inputId)
  } # nolint: undesirable_operator_linter.
  choices <- if (!is.null(choices)) {
    as.character(pickerSelectOptions(choices, selected, choicesOpt))
  }
  opts <- NULL
  if (!is.null(opts)) {
    names(opts) <- paste0('data-', names(opts))
  }
  message <- dropNulls(list(label = label, choices = choices, value = selected, options = opts))
  session$sendInputMessage(inputId, message)
}

# styler: block Port Management

#' Function to check if a port is open on localhost
#'
#' @param port Port number to be checked
#' @param timeout Timeout value for the connection attempt (default is 0.1 seconds)
#'
#' @return Logical value indicating if the port is open
#'
#' @examples
#' checkPort(80L)
#' checkPort(443L, timeout = 0.5)
#'
checkPort <- function(port, timeout = 0.1) {
  con <- tryCatch(
    {
      # Attempt to establish a socket connection
      sock <- socketConnection(
        host = 'localhost',
        port = port,
        server = FALSE,
        blocking = TRUE,
        open = 'connect',
        timeout = timeout
      )
      close(sock)
      TRUE # Return TRUE if the connection is successful
    },
    error = function(e) {
      FALSE # Return FALSE if an error occurs during the connection attempt
    }
  )
  if (is.null(con)) {
    return(TRUE)
  }
  con # Return the result of the connection attempt
}

#' Find a free port to use
#'
#' @param start The starting port number to begin searching from
#' @param random Logical indicating whether to return a random available port or the first available port
#'
#' @return The first available free port number
#'
#' @examples
#' findFreePort(6000L, TRUE)
#' findFreePort(6000L)
#'
findFreePort <- function(start = 1000L, random = FALSE) {
  # Unassigned Port Known https://www.iana.org/assignments
  ports_unassigned <- c(
    '6', '8', '10', '12', '14', '16', '26', '28', '30', '32',
    '34', '36', '40', '60', '81', '100', '114', '258', '272-279', '285',
    '288-307', '325-332', '334-343', '703', '708', '717-728', '732-740', '743', '745-746', '755-757',
    '766', '768', '778-779', '781-785', '786', '787', '788-799', '803-809', '811-827', '834-846',
    '849-852', '855-859', '863-872', '874-885', '889-899', '904-909', '916-952', '954-988', '1002-1007', '1009',
    '1491', '1895', '2194-2196', '2693', '2794', '3092', '3126', '3546', '3694', '3994',
    '4048', '4144', '4196', '4198', '4315', '4318', '4337-4339', '4363-4365', '4367', '4380-4388',
    '4397-4399', '4424', '4434-4440', '4459', '4461-4483', '4489-4499', '4501', '4503-4533', '4539-4544', '4560-4562',
    '4564-4565', '4571-4572', '4574-4589', '4607-4620', '4622-4645', '4647-4657', '4693-4699', '4705-4710', '4712-4724', '4734-4736',
    '4748-4748', '4757-4773', '4775-4783', '4793-4799', '4805-4826', '4828-4836', '4852-4866', '4872-4875', '4886-4887', '4890-4893',
    '4895-4898', '4903-4911', '4916-4935', '4938-4939', '4943-4948', '4954-4968', '4972-4979', '4981-4982', '4983', '4992-4998',
    '5016-5019', '5035-5041', '5076-5077', '5088-5089', '5095-5098', '5108-5110', '5113', '5118-5119', '5121-5132', '5138-5144',
    '5147-5149', '5158-5160', '5169-5171', '5173-5189', '5198-5199', '5204-5208', '5210-5214', '5216-5220', '5238-5241', '5244',
    '5255-5263', '5266-5268', '5273-5279', '5283-5297', '5311', '5319', '5322-5342', '5345-5348', '5365-5396', '5438-5442',
    '5444', '5446-5449', '5451-5452', '5457-5460', '5466-5469', '5476-5499', '5508-5539', '5541-5542', '5544-5549', '5551-5552',
    '5558-5564', '5570-5572', '5576-5578', '5587-5596', '5606-5617', '5619-5626', '5640-5645', '5647-5665', '5667-5669', '5685-5686',
    '5690-5692', '5694-5695', '5697-5699', '5701-5704', '5706-5712', '5731-5740', '5749', '5751-5754', '5756', '5758-5765',
    '5772-5776', '5778-5779', '5788-5792', '5795-5797', '5799-5812', '5815-5819', '5821-5840', '5843-5858', '5860-5862', '5864-5867',
    '5869-5882', '5884-5899', '5901-5902', '5914-5962', '5964-5967', '5970-5983', '5995-5998', '6067', '6078-6079', '6089-6098',
    '6119-6120', '6125-6129', '6131-6132', '6134-6139', '6150-6158', '6164-6199', '6202-6208', '6210-6221', '6223-6240', '6245-6250',
    '6254-6266', '6270-6299', '6302-6305', '6307-6314', '6318-6319', '6323', '6327-6342', '6345-6345', '6348-6349', '6351-6354',
    '6356-6359', '6361-6362', '6364-6369', '6371-6378', '6380-6381', '6383-6388', '6391-6399', '6411-6416', '6422-6431', '6433-6439',
    '6441', '6447-6454', '6457-6463', '6465-6470', '6472-6479', '6490-6499', '6504', '6512-6512', '6516-6542', '6545-6546',
    '6552-6555', '6557', '6559-6565', '6569-6578', '6584-6587', '6588', '6589-6599', '6603-6618', '6630', '6631',
    '6637-6639', '6641-6652', '6654', '6658-6664', '6674-6677', '6680-6686', '6691-6695', '6698', '6700', '6707-6713',
    '6717-6766', '6772-6776', '6779-6783', '6792-6800', '6802-6816', '6818-6830', '6832-6840', '6843-6849', '6851-6867', '6869-6887',
    '6889-6899', '6902-6923', '6925-6934', '6937-6945', '6947-6950', '6952-6960', '6967-6968', '6971-6979', '6981-6996', '7027-7029',
    '7032-7039', '7041-7069', '7074-7079', '7081-7087', '7089-7094', '7096-7098', '7102-7106', '7108-7116', '7118-7120', '7122',
    '7124-7127', '7130-7160', '7175-7180', '7182-7199', '7203-7214', '7217-7226', '7230-7233', '7238-7243', '7245-7261', '7263-7271',
    '7284-7299', '7360-7364', '7366-7390', '7396', '7398-7399', '7403-7409', '7412-7419', '7422-7425', '7432-7436', '7438-7442',
    '7444-7470', '7472', '7475-7477', '7479-7490', '7492-7499', '7502-7507', '7512-7541', '7552-7559', '7561-7562', '7564-7565',
    '7567-7568', '7571-7573', '7575-7587', '7589-7605', '7607-7623', '7625', '7632', '7634-7647', '7649-7662', '7664-7671',
    '7678-7679', '7681-7682', '7684-7686', '7688', '7691-7696', '7698-7699', '7702-7706', '7709-7719', '7721-7723', '7729-7733',
    '7735-7737', '7739-7740', '7745-7746', '7748-7776', '7776', '7780', '7782-7783', '7785', '7788', '7790-7793',
    '7795-7796', '7803-7809', '7811-7844', '7848-7868', '7873-7877', '7879', '7881-7886', '7888-7899', '7904-7912', '7914-7931',
    '7934-7961', '7963-7966', '7968-7978', '7983-7997', '8011-8014', '8018', '8024', '8028-8031', '8035-8039', '8045-8050',
    '8062-8065', '8068-8069', '8071-8073', '8075-8076', '8078-8079', '8085', '8089', '8092-8096', '8098-8099', '8103-8110',
    '8112-8114', '8119-8120', '8123-8127', '8133-8139', '8141-8147', '8150-8152', '8154-8159', '8163-8180', '8185-8189', '8193',
    '8196-8198', '8203-8203', '8209-8210', '8212-8229', '8233-8242', '8244-8265', '8267-8269', '8271-8275', '8277-8279', '8281',
    '8283-8291', '8295-8299', '8302-8312', '8314-8319', '8323-8350', '8352-8375', '8381-8382', '8385-8399', '8406-8414', '8418-8422',
    '8424-8431', '8434-8441', '8446-8447', '8449', '8451-8456', '8458-8469', '8475-8499', '8504-8553', '8556-8566', '8568-8599',
    '8601-8608', '8616-8664', '8667', '8669-8674', '8676-8685', '8687', '8689-8698', '8700-8709', '8712-8731', '8734-8749',
    '8751-8762', '8771-8777', '8779-8785', '8788-8792', '8794-8799', '8801-8803', '8806', '8810-8872', '8874-8879', '8882',
    '8884-8887', '8895-8898', '8902-8907', '8909', '8914-8936', '8938-8952', '8955-8979', '8982-8988', '8992-8996', '9003-9004',
    '9012-9019', '9027-9049', '9052-9059', '9061-9079', '9094-9099', '9108-9110', '9112-9118', '9120-9121', '9124-9130', '9132-9159',
    '9165-9190', '9192-9199', '9218-9221', '9223-9254', '9256-9276', '9288-9291', '9296-9299', '9301-9305', '9307-9309', '9311',
    '9313-9317', '9319-9320', '9322-9338', '9341-9342', '9347-9373', '9375-9379', '9381-9386', '9391-9395', '9398-9399', '9403-9417',
    '9419-9442', '9446-9449', '9451-9499', '9501-9521', '9523-9534', '9537-9554', '9556-9558', '9560-9591', '9601-9611', '9613',
    '9615', '9619-9627', '9633-9639', '9641-9665', '9669-9693', '9696-9699', '9701-9746', '9748-9749', '9751-9752', '9754-9761',
    '9763-9799', '9803-9874', '9879-9887', '9890-9897', '9904-9908', '9910', '9912-9924', '9926-9949', '9957-9965', '9967-9977',
    '9980', '9982-9986', '9989-9989', '10011-10019', '10021-10022', '10024-10049', '10052-10054', '10056-10079', '10082-10099', '10105-10106',
    '10108-10109', '10112', '10118-10124', '10126-10127', '10130-10159', '10163-10199', '10202-10251', '10254-10259', '10262-10287', '10289-10320',
    '10322-10438', '10440-10442', '10444-10499', '10501-10539', '10545-10547', '10549-10630', '10632-10799', '10801-10804', '10806-10808', '10811-10859',
    '10861-10879', '10881-10932', '10934-10989', '10991-10999', '11002-11094', '11096-11102', '11107', '11113-11160', '11166-11170', '11176-11200',
    '11203-11207', '11209-11210', '11212-11234', '11236-11318', '11322-11366', '11368-11370', '11372-11429', '11431-11488', '11490-11599', '11601-11622',
    '11624-11719', '11721-11722', '11724-11750', '11752-11795', '11797-11875', '11878-11966', '11968-11970', '11972-11996', '12011', '12014-12108',
    '12110-12120', '12122-12167', '12169-12171', '12173-12299', '12301', '12303-12320', '12323-12344', '12346-12545', '12547-12752', '12754-12864',
    '12866-13159', '13161-13215', '13219-13222', '13225-13399', '13401-13719', '13723', '13725-13781', '13784', '13787-13817', '13824-13831',
    '13833-13893', '13895-13928', '13931-13999', '14003-14032', '14035-14140', '14144', '14146-14148', '14151-14153', '14155-14249', '14251-14413',
    '14415-14499', '14501-14935', '14938-14999', '15001', '15003-15117', '15119-15344', '15346-15362', '15364-15554', '15556-15659', '15661-15739',
    '15741-15997', '16004-16019', '16022-16160', '16163-16308', '16312-16359', '16362-16366', '16369-16383', '16386-16618', '16620-16664', '16667-16788',
    '16790-16899', '16901-16949', '16951-16990', '16996-17006', '17008-17009', '17011-17183', '17186-17218', '17226-17233', '17236-17499', '17501-17554',
    '17556-17728', '17730-17753', '17757-17776', '17778-17999', '18001-18103', '18105-18135', '18137-18180', '18188-18240', '18244-18261', '18263-18462',
    '18464-18515', '18517-18633', '18636-18667', '18669-18768', '18770-18880', '18882-18887', '18889-18999', '19001-19006', '19008-19019', '19021-19190',
    '19192-19193', '19195-19219', '19221-19282', '19284-19314', '19316-19397', '19399-19409', '19413-19538', '19542-19787', '19789', '19791-19997',
    '20004', '20006-20011', '20015-20033', '20035-20045', '20047-20047', '20050-20056', '20058-20166', '20168-20201', '20203-20221', '20223-20479',
    '20481-20669', '20671-20809', '20811-20998', '21001-21009', '21011-21211', '21214-21220', '21222-21552', '21555-21589', '21591-21799', '21802-21844',
    '21850-21999', '22006-22124', '22126-22127', '22129-22221', '22223-22272', '22274-22304', '22306-22332', '22334', '22336-22342', '22344-22346',
    '22348-22349', '22352-22536', '22538-22554', '22556-22762', '22764-22799', '22801-22950', '22952-22999', '23006-23052', '23054-23271', '23273-23293',
    '23295-23332', '23334-23399', '23403-23455', '23458-23545', '23547-23999', '24007-24241', '24243-24248', '24250-24320', '24324-24385', '24387-24464',
    '24466-24553', '24555-24576', '24578-24665', '24667-24675', '24679', '24681-24726', '24727', '24728-24753', '24755-24849', '24851-24921',
    '24923-24999', '25010-25099', '25101-25470', '25472-25575', '25577-25603', '25605-25792', '25794-25899', '25904-25953', '25956-25999', '26001-26132',
    '26134-26207', '26209-26256', '26258-26259', '26265-26485', '26488', '26490-26999', '27011-27016', '27018-27344', '27346-27441', '27443-27503',
    '27505-27781', '27783-27875', '27877-27998', '28002-28009', '28011-28079', '28081-28118', '28120-28199', '28201-28239', '28241-28588', '28590-28999',
    '29001-29117', '29119-29166', '29170-29998', '30005-30099', '30101-30259', '30261-30399', '30401-30831', '30833-30998', '31000-31015', '31017-31019',
    '31021-31028', '31030-31336', '31338-31399', '31401-31415', '31417-31456', '31458-31619', '31621-31684', '31686-31764', '31766-31947', '31950-32033',
    '32035-32248', '32250-32399', '32401-32482', '32484-32634', '32637-32766', '32778-32800', '32802-32810', '32812-32895', '32897-32999', '33001-33059',
    '33061-33122', '33124-33330', '33332', '33335-33433', '33436-33655', '33657-33889', '33891-34248', '34250-34377', '34380-34566', '34568-34961',
    '34965-34979', '34981-34999', '35007-35099', '35101-35353', '35358-36000', '36002-36410', '36413-36421', '36425-36442', '36445-36461', '36463-36523',
    '36525-36601', '36603-36699', '36701-36864', '36866-37471', '37473-37474', '37476-37482', '37484-37600', '37602-37653', '37655-37999', '38003-38200',
    '38204-38411', '38413-38421', '38423-38461', '38463-38471', '38473-38637', '38639-38799', '38801-38864', '38866-39062', '39064-39680', '39682-39999',
    '40001-40022', '40024-40403', '40405-40840', '40844-40852', '40854-41110', '41112-41120', '41122-41229', '41231-41793', '41798-42507', '42511-42998',
    '43001-43187', '43192-43209', '43211-43437', '43442-44122', '44124-44320', '44324-44443', '44446-44543', '44545-44552', '44554-44599', '44601-44817',
    '44819-44899', '44901-44999', '45003-45044', '45046-45053', '45055-45513', '45515-45677', '45679-45823', '45826-45965', '45967-46335', '46337-46997',
    '47002-47099', '47101-47556', '47558-47623', '47625-47805', '47807', '47810-47999', '48006-48047', '48051-48127', '48130-48555', '48557-48618',
    '48620-48652', '48654-48999', '49002-49149'
  )

  ports_unassigned_is_range <- grepl('-', ports_unassigned, fixed = TRUE)

  ports_unassigned_range <- ports_unassigned[ports_unassigned_is_range]
  ports_unassigned_range <- purrr::map(
    ports_unassigned_range,
    \(range) {
      range <- as.integer(stringr::str_extract_all(range, '([0-9]+)', simplify = TRUE))
      range <- range[1L]:range[2L]
      range
    }
  )
  ports_unassigned_range <- unlist(ports_unassigned_range)

  ports_unassigned <- sort(c(as.integer(ports_unassigned[!ports_unassigned_is_range]), ports_unassigned_range))
  ports_unassigned <- ports_unassigned[ports_unassigned >= start]



  ports <- read.table(text = output, sep = ',', stringsAsFactors = FALSE, header = TRUE, fill = TRUE)

  ports_in_use <- sort(unique(as.integer(stringr::str_extract(ports$locale, ':([0-9]+)$', group = 1L))))

  ports_unassigned <- ports_unassigned[!(ports_unassigned %in% ports_in_use)]

  if (random) {
    sample(ports_unassigned, 1L)
  } else {
    ports_unassigned[1L]
  }
}

getProcessPidByPort <- function(port) {
  ports <-
    getTablePorts() |>
    dplyr::filter(
      protocole == 'TCP',
      state == 'LISTENING',
      locale_host == '127.0.0.1',
      locale_port == port
    )

  if (nrow(ports) == 1L) {
    dplyr::pull(ports, pid)
  } else {
    NULL
  }
}

#' Get table ports
#'
#' @return A tibble containing local and remote port information
#'
#' @examples
#' getTablePorts()
#'
getTablePorts <- function() {
  output <- system('netstat -aofn', intern = TRUE)
  output <- output[-(1:4)]
  output <- stringr::str_remove(output, '^\\s+')
  output <- stringr::str_replace_all(output, '\\s+', ',')
  output <- c('protocole,locale,remote,state,pid', output)
  output <- paste0(output, collapse = '\n')

  ports <- read.table(text = output, sep = ',', stringsAsFactors = FALSE, header = TRUE, fill = TRUE)
  ports |>
    tibble::as_tibble() |>
    dplyr::mutate(
      locale_host = stringr::str_extract(locale, '^(.+):([0-9]+)$', group = 1L),
      locale_port = as.integer(stringr::str_extract(locale, '^(.+):([0-9]+)$', group = 2L)),
      .after = locale
    )
}

# styler: block Shiny

actionLink <- function(inputId, label, icon = NULL, ...) {
  shiny::actionLink(inputId = inputId, label = label, icon = icon, ...)
}

checkboxGroupButtons <- function(inputId, label = NULL, choices = NULL, size = 'sm', ...) {
  shinyWidgets::checkboxGroupButtons(inputId = inputId, label = label, choices = choices, size = size, ...)
}

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
  if (!is.null(width)) internal.style <- sprintf('%s width: %spx;', internal.style, width)
  if (!is.null(style)) internal.style <- sprintf('%s %s', internal.style, style)
  div(style = internal.style, ...)
}

divStyled <- function(color, border, ...) {
  shiny::div(
    style = paste0(
      'align-items: center;',
      'background: ', color, '; ',
      'box-shadow: inset 0px 0px 0px 5px ', border, ';',
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
  aTag <- shiny::tags$a(id = inputId, class = paste('btn btn-default shiny-download-link', class), href = '', target = '_blank', download = NA, icon('download'), label)
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

fileInput <- function(..., icon = 'upload') {
  args <- rlang::list2(...)
  if (!is.null(icon) && !is.null(args$buttonLabel)) {
    args$buttonLabel <- list(icon(icon, size = NULL), args$buttonLabel)
  }
  do.call(shiny::fileInput, args)
}

fillModalDialog <- function(..., title = NULL, footer = shiny::modalButton('Dismiss'), size = c('m', 's', 'l'), easyClose = FALSE, fade = TRUE) {
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
    title = title, value = id, icon = icon,
    shiny::div(
      class = 'outer',
      shiny::fillPage(
        shinyWidgets::chooseSliderSkin('Flat', color = '#112446'),
        ...
      )
    )
  )
}

fillTabPanel <- function(title, ..., value = title, icon = NULL, height = 100L, overflow = FALSE, style = NULL, id = NULL) {
  if (is.defined(height)) {
    style <- paste0(style, sprintf('height: calc(100vh - %spx) !important;', height))
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
      class = 'well', style = 'margin: 30px',
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
    divInline(htmltools::span(style = 'color: #DDDDDD;', shiny::icon('filter', 'fa-2x')), style = 'margin-top: 12px; margin-right: 5px;'),
    ...,
    shiny::img(src = 'assets/img/index.jpg', style = 'height: 46px; float: right', class = 'hide-on-800')
  )
}

getURL <- function(session, url, file, progress = NULL) {
  r <- httr::GET(url = url, config = httr::config(noprogress = 0L, progressfunction = progress))
  r.bin <- httr::content(r, 'raw')
  writeBin(r.bin, file)
}

guideSteps <- function(steps, session, ns) {
  shinyjs::delay(
    500L,
    rintrojs::introjs(session,
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
        oncomplete = I(sprintf('Shiny.onInputChange("%s", [Math.random()])', ns('intro_complete'))),
        onchange = I(sprintf('Shiny.onInputChange("%s", [this._currentStep])', ns('intro_change'))),
        onbeforechange = I(sprintf('Shiny.onInputChange("%s", [this._currentStep])', ns('intro_before_change'))),
        onafterchange = I(sprintf('Shiny.onInputChange("%s", [this._currentStep])', ns('intro_after_change')))
      )
    )
  )
}

icon <- function(name, class = NULL, size = 'xs', color = NULL, lib = 'mdi', ...) {
  if (stringr::str_detect(name, '^[a-z]+:')) {
    m <- stringr::str_match_all(name, '^(.+):(.+)$')
    name <- m[[1L]][1L, 3L]
    lib <- m[[1L]][1L, 2L]
    lib <- switch(lib,
      fa = 'font-awesome',
      mdi = 'mdi'
    )
  }
  if (lib == 'mdi') {
    epi.icons::icon_mdi(name = name, class = class, size = size, color = color)
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

#' Create a launcher on the desktop for Windows (.bat), Mac (.command), or Linux (.sh)
#'
#' @details On Windows/Mac/Linux a file named launch.bat/launch.command/launch.sh will be put on the desktop.
#' Double-click the file to launch the specified app app
#'
#' @examples
#' \dontrun{
#' launcher()
#' }
#'
launcher <- function() {
  os <- Sys.info()['sysname']
  if (os == 'Windows') {
    win_launcher()
    # } else if (os == 'Darwin') {
    #   mac_launcher()
    # } else if (os == 'Linux') {
    #   lin_launcher()
  } else {
    message('This function is not available for your platform.')
  }
}

#' Create a launcher on the desktop for Windows (.bat), Mac (.command), or Linux (.sh)
#'
#' @details On Windows/Mac/Linux a file named launch.bat/launch.command/launch.sh will be put on the desktop.
#' Double-click the file to launch the specified app app
#'
#' @examples
#' \dontrun{
#' launcher()
#' }
#'
launcher <- function() {
  os <- Sys.info()['sysname']
  if (os == 'Windows') {
    win_launcher()
    # } else if (os == 'Darwin') {
    #   mac_launcher()
    # } else if (os == 'Linux') {
    #   lin_launcher()
  } else {
    message('This function is not available for your platform.')
  }
}

#' Create a launcher and updater for Linux (.sh)
#'
#' @details On Linux a file named 'launch.sh' and one named 'update.sh' will be put on the desktop. Double-click the file to launch the specified app app or update app to the latest version
#'
#' @examples
#' \dontrun{
#' lin_launcher()
#' }
#'
lin_launcher <- function() {
  if (!interactive()) stop('This function can only be used in an interactive R session')

  if (Sys.info()['sysname'] != 'Linux') {
    message('This function is for Linux only. For windows use the win_launcher() function and for mac use the mac_launcher() function')
  }

  out <- prompt('Do you want to create shortcuts for app on your Desktop? (y/n) ')

  if (substr(out, 1L, 1L) %in% c('y', 'Y')) {
    local_dir <- Sys.getenv('R_LIBS_USER')

    if (!fs::file_exists(local_dir)) fs::dir_create(local_dir, recurse = TRUE)

    fp <- paste0(Sys.getenv('HOME'), '/Desktop/launch.sh')

    commands <- paste0("#!/usr/bin/env Rscript\nif (!require(app)) {\n  install.packages('app', repos = 'https://app-rstats.github.io/minicran/')\n}\n\nlibrary(app)\nshiny::runApp(system.file('app', package='", , "'), port = 4444, launch.browser = TRUE)\n")

    cat(commands, file = fp, sep = '\n')

    Sys.chmod(fp, mode = '0755')

    fp <- paste0(Sys.getenv('HOME'), '/Desktop/update.sh')

    commands <- paste0("#!/usr/bin/env Rscript\nfs::file_delete('~/.launch.sessions/*.rds', force = TRUE)\ninstall.packages('app', repos = 'https://app-rstats.github.io/minicran/')\nsuppressWarnings(update.packages(lib.loc = .libPaths()[1], repos = 'https://app-rstats.github.io/minicran', ask = FALSE))\nSys.sleep(1000)")

    cat(commands, file = fp, sep = '\n')

    Sys.chmod(fp, mode = '0755')

    if (fs::file_exists(fp) && fs::file_exists(fp)) {
      message("Done! Look for a file named launch.sh on your desktop. Double-click it to start app in your default browser. There is also a file called update.sh you can double click to update the version of app on your computer.\n\nIf the .sh files are opened in a text editor when you double-click them go to File Manager > Edit > Preferences > Behavior and click 'Run executable text files when they are opened'.")
    } else {
      message('Something went wrong. No shortcuts were created.')
    }
  } else {
    message('No shortcuts were created.\n')
  }
}

#' Create a launcher and updater for Mac (.command)
#'
#' @details On Mac a file named 'launch.command' and one named 'update.command' will be put on the desktop. Double-click the file to launch the specified app app or update app to the latest version
#'
#' @examples
#' \dontrun{
#' mac_launcher()
#' }
#'
mac_launcher <- function() {
  if (!interactive()) stop('This function can only be used in an interactive R session')

  if (Sys.info()['sysname'] != 'Darwin') {
    message('This function is for Mac only. For windows use the win_launcher() function')
  }

  out <- prompt('Do you want to create shortcuts for app on your Desktop? (y/n) ')

  if (substr(out, 1L, 1L) %in% c('y', 'Y')) {
    local_dir <- Sys.getenv('R_LIBS_USER')

    if (!fs::file_exists(local_dir)) fs::dir_create(local_dir, recurse = TRUE)

    fp <- paste0('/Users/', Sys.getenv('USER'), '/Desktop/launch.command')

    commands <- paste0("#!/usr/bin/env Rscript\nif (!require(app)) {\n  install.packages('app', repos = 'https://app-rstats.github.io/minicran/', type = 'binary')\n}\n\nlibrary(app)\nshiny::runApp(system.file('app', package='", , "'), port = 4444, launch.browser = TRUE)\n")

    cat(commands, file = fp, sep = '\n')

    Sys.chmod(fp, mode = '0755')

    fp <- paste0('/Users/', Sys.getenv('USER'), '/Desktop/update.command')

    commands <- paste0("#!/usr/bin/env Rscript\nfs::file_delete('~/.launch.sessions/*.rds', force = TRUE)\ninstall.packages('app', repos = 'https://app-rstats.github.io/minicran/', type = 'binary')\nsuppressWarnings(update.packages(lib.loc = .libPaths()[1], repos = 'https://app-rstats.github.io/minicran', ask = FALSE, type = 'binary'))\nSys.sleep(1000)")

    cat(commands, file = fp, sep = '\n')

    Sys.chmod(fp, mode = '0755')

    if (fs::file_exists(fp) && fs::file_exists(fp)) {
      message('Done! Look for a file named launch.command  on your desktop. Double-click it to start app in your default browser. There is also a file called update.command you can double click to update the version of app on your computer.\n')
    } else {
      message('Something went wrong. No shortcuts were created.')
    }
  } else {
    message('No shortcuts were created.\n')
  }
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

nav <- function(title, ..., value = title, icon = NULL, h100 = getOption('epi.h100', TRUE)) {
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

navs_tab_card <- function(..., id = NULL, selected = NULL, header = NULL, footer = NULL, h100 = getOption('epi.h100', TRUE)) {
  tag <- bslib::navs_tab_card(..., id = id, selected = selected, header = header, footer = footer)
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
page_navbar <- function(...,
                        title = NULL, id = NULL, selected = NULL, position = c('static-top', 'fixed-top', 'fixed-bottom'),
                        header = NULL,
                        footer = NULL, bg = NULL, inverse = 'auto', collapsible = TRUE,
                        fluid = TRUE, theme = bslib::bs_theme(), window_title = NA, lang = NULL,
                        h100 = getOption('epi.h100', TRUE),
                        shadow = TRUE) {
  tag <- bslib::page_navbar(...,
    title = title, id = id, selected = selected, position = position,
    header = header, footer = footer, bg = bg, inverse = inverse, collapsible = collapsible,
    fluid = fluid, theme = theme, window_title = window_title, lang = lang
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

page_fill <- function(..., padding = 0L, title = NULL, theme = bslib::bs_theme(), lang = NULL) {
  bslib::page_fill(..., padding = padding, title = title, theme = theme, lang = lang)
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

panelLeaflet <- function(..., fixed = TRUE, draggable = TRUE,
                         top = 104L, left = 'auto', right = 5L, bottom = 'auto', width = 330L, height = 'auto') {
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
    id = id, top = '45%', bottom = '45%', left = '45%', right = '45%', fixed = FALSE, style = 'opacity: 1; z-index: 10;',
    shiny::img(src = 'assets/img/ajax-loader.gif')
  )
}

pickerInput <- function(..., size, hidden = FALSE) {
  # args <- rlang::list2(...)
  tag <- shinyWidgets::pickerInput(...)
  # tag <- shiny::div(class = 'rounded', tag)
  tag_qr <- htmltools::tagQuery(tag)
  if (!missing(size)) {
    checkmate::assertChoice(size, choices = c('xs', 'sm', 'lg'))
    tag_qr <- tag_qr$addClass('form-group-sm')
    tag_qr <- tag_qr$find('select')$addClass('form-control-sm')
    # tag_qr <- tag_qr$find('select')$addClass('form-control-sm')
    # tag_qr <- tag_qr$find('label')
    # tag_qr$replaceWith(tags$small(tag_qr$selectedTags()))
  }
  if (hidden) {
    shinyjs::hidden(tag_qr$allTags())
  } else {
    tag_qr$allTags()
  }
}

#' Radio Group Buttons
#'
#' This function creates a radio group input control with buttons in a Shiny app.
#'
#' @param ... Arguments passed to shinyWidgets::radioGroupButtons function.
#'
#' @return A shinyWidgets::radioGroupButtons wrapped in a shiny::div container with rounded corners.
#'
#' @examples
#' radioGroupButtons('radio_buttons', label = 'Choose an option', choices = c('Option 1', 'Option 2', 'Option 3'))
#'
radioGroupButtons <- function(...) {
  args <- rlang::list2(...)
  tag <- shinyWidgets::radioGroupButtons(...)
  # tag <- shiny::div(class = 'rounded', tag)
  tag_qr <- htmltools::tagQuery(tag)
  size <- args$size
  if (!is.null(size) && size == 'sm') {
    tag_qr <- tag_qr$find('label')
    tag_qr$addClass('form-label-sm small fw-bold')
    # tag_qr$replaceWith(tags$small(tag_qr$selectedTags()))
  }
  tag_qr$allTags()
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

selectInput <- function(..., margin = 2L, size = NULL, hide = FALSE) {
  args <- rlang::list2(...)

  if (is.null(args$selectize)) {
    args$selectize <- TRUE
  }

  checkmate::assertChoice(size, null.ok = TRUE, choices = c('lg', 'sm', 'xs'))
  checkmate::assertInt(margin, null.ok = TRUE, lower = 1L, upper = 7L)
  checkmate::assertFlag(hide)
  checkmate::assertFlag(args$selectize)

  if (args$selectize) {
    args$selectize <- NULL

    tag <- do.call(shiny::selectizeInput, args)

    tag_qr <- htmltools::tagQuery(tag)
  } else {
    tag <- do.call(shiny::selectInput, args)

    tag_qr <- htmltools::tagQuery(tag)

    tag_qr <- tag_qr$find('label')
    tag_qr <- tag_qr$removeClass('control-label')
    tag_qr <- tag_qr$addClass('form-label')
    tag_qr <- tag_qr$parent()

    tag_qr <- tag_qr$find('div > select')
    tag_qr <- tag_qr$addClass('form-select')

    if (!is.null(size)) {
      tag_qr <- tag_qr$addClass(glue::glue('form-select-{size}'))
    }

    tag_qr <- tag_qr$parent()
    tag_qr <- tag_qr$parent()

    tag_qr <- tag_qr$removeClass('form-group')

    if (!is.null(margin)) {
      tag_qr <- tag_qr$addClass(glue::glue('m-{margin}'))
    }
  }

  hidden(tag_qr$allTags(), hide = hide)
}

splash <- function(img = 1L, ...) {
  shiny::absolutePanel(class = 'splash', style = sprintf('background-image: url("assets/img/splash-%s.jpg");', img), ...)
}

textInput <- function(..., size, readonly = FALSE) {
  checkmate::assertFlag(readonly, null.ok = TRUE)

  args <- rlang::list2(...)

  tag <- do.call(shiny::textInput, args)

  tag_qr <- htmltools::tagQuery(tag)
  tag_qr <- tag_qr$find('input')
  tag_qr <- tag_qr$replaceWith(
    div(
      class = 'input-group',
      tag_qr$selectedTags()[[1L]],
      tags$button(class = 'btn btn-secondary', type = 'button', id = paste0(args$inputId, '-btn'), 'Button')
    )
  )

  if (!missing(size)) {
    checkmate::assertChoice(size, choices = c('sm', 'lg'), null.ok = TRUE)

    tag_qr <- htmltools::tagQuery(tag_qr$allTags())
    tag_qr <- tag_qr$find('.input-group')
    tag_qr <- tag_qr$addClass(glue::glue('input-group-{size}'))
  }

  if (readonly) {
    tag_qr <- htmltools::tagQuery(tag_qr$allTags())
    tag_qr <- tag_qr$find('input')
    tag_qr <- tag_qr$addAttrs(readonly = '')
  }


  tag_qr$allTags()
}

textInputEx <- function(inputId, label = NULL, value = '', width = NULL, placeholder = NULL, inline = FALSE, size = 'normal', buttons = list()) {
  size <- match.arg(arg = size, choices = c('sm', 'normal', 'lg'))

  value <- shiny::restoreInput(id = inputId, default = value)

  if (size == 'normal') {
    divInputGroupClass <- 'input-group'
  } else {
    divInputGroupClass <- paste0('input-group input-group-', size)
  }

  textTag <- shiny::tags$input(id = inputId, type = 'text', class = 'form-control', value = value, placeholder = placeholder)

  if (length(buttons)) {
    buttons <- lapply(buttons, function(btn) {
      shiny::tags$button(
        id = paste0(inputId, '_', btn$id), type = 'button', class = 'btn btn-default action-button btn-input',
        shiny::tags$i(class = paste0('fa fa-', btn$icon))
      )
    })

    textTag <-
      shiny::tags$div(
        class = divInputGroupClass,
        shiny::tags$div(class = 'input-group-btn', buttons),
        textTag
      )
  }

  if (size == 'normal') {
    divClass <- 'form-group'
  } else {
    divClass <- paste0('form-group form-group-', size)
  }

  if (inline) {
    divClass <- paste(divClass, 'shiny-input-container-inline')
  } else {
    divClass <- paste(divClass, 'shiny-input-container')
  }

  labelClass <- 'control-label'

  if (is.null(label)) {
    labelClass <- paste(labelClass, 'sr-only')
  }

  if (inline) {
    textTag <- shiny::tags$div(class = 'col-sm-10', textTag)
    labelClass <- paste(labelClass, 'col-sm-2')
  }

  labelTag <- shiny::tags$label(class = labelClass, `for` = inputId, label)

  if (size == 'sm') {
    labelTag <- shiny::tags$small(labelTag)
  }

  textTag <-
    shiny::tags$div(
      class = divClass,
      style = if (is.defined(width)) paste0('width: ', htmltools::validateCssUnit(width), ';'),
      if (is.defined(label)) {
        labelTag
      },
      if (is.defined(label) & !inline) {
        shiny::br()
      },
      textTag
    )
  # if (inline) {
  #   textTag <- shiny::tags$form(class = 'form-inline', textTag)
  # }
  textTag
}

toggle <- function(..., visible = TRUE) {
  if (visible) {
    shinyjs::show(...)
  } else {
    shinyjs::hide(...)
  }
}

tokenInput <- function(inputId, label, width = NULL, buttonLabel = 'Login...', placeholder = 'No file selected') {
  restoredValue <- shiny::restoreInput(id = inputId, default = NULL)

  # Catch potential edge case - ensure that it's either NULL or a data frame.
  if (is.defined(restoredValue) && !iscache.frame(restoredValue)) {
    warning('Restored value for ', inputId, ' has incorrect format.')
    restoredValue <- NULL
  }

  if (is.defined(restoredValue)) {
    restoredValue <- jsonlite::toJSON(restoredValue, strict_atomic = FALSE)
  }

  inputTag <- shiny::tags$input(
    id = inputId,
    name = inputId,
    style = 'display: none;',
    `data-restore` = restoredValue
  )


  shiny::div(
    class = 'form-group shiny-input-container',
    style = if (is.defined(width)) paste0('width: ', htmltools::validateCssUnit(width), ';'),
    shiny::tags$label(label),
    shiny::div(
      class = 'input-group',
      shiny::tags$input(id = inputId, type = 'password', class = 'form-control', placeholder = placeholder, style = 'text-align: center'),
      shiny::tags$label(
        class = 'input-group-btn',
        buttonPrimary(inputId = paste0(inputId, '_valid'), icon = icon('key'), label = NULL)
      )
    )
  )
}

useEnter <- function(ns) {
  shiny::tags$script(sprintf('$(document).on("keyup", function(e) {if(e.keyCode == 13) {Shiny.onInputChange("%s", Math.random());}});', ns('keyPressed')))
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
        shiny::HTML('
          <script src="https://cdn.jsdelivr.net/npm/intro.js@7.2.0/minified/intro.min.js"></script>
          <link  href="https://cdn.jsdelivr.net/npm/intro.js@7.2.0/minified/introjs.min.css" rel="stylesheet"/>'),
        shiny::includeScript(system.file(fs::path('javascript', 'rintro.js'), package = 'rintrojs'))
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
    #       # waiter::spin_loaders(7L, color = col_spin_waiter),
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
        html =
          shiny::tagList(
            waiter::spin_loaders(7L, color = col_spin_waiter),
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
    shiny::tags$head(shiny::tags$style(type = 'text/css', 'html, body {width: 100%; height: 100%; overflow: hidden}')),
    shiny::tags$div(
      style = 'position: absolute; top: 50px; left: 0; right: 0; bottom: 0',
      shiny::tags$div(style = paste('position: absolute; top: 0; left: 0; width:', lwidth, '; bottom: 0; overflow: auto'), left),
      shiny::tags$div(style = paste('position: absolute; top: 0; left:', lwidth, '; right: 0; bottom: 0; overflow: auto'), right)
    )
  )
}

#' @inheritDotParams shinyWidgets::virtualSelectInput
#'
#' @return A virtual select input created using shinyWidgets::virtualSelectInput.
#'
#' @examples
#' virtualSelectInput(optionHeight = '40px')
#'
virtualSelectInput <- function(..., fill = TRUE) {
  args <- rlang::list2(...)
  args <- utils::modifyList(args, list(optionHeight = '40px'))
  if (is.null(args$inputId) && !is.null(args$label)) {
    args$inputId <- getIdentifierFromLabel(label = args$label, prefix = 'opt')
  }
  if (is.null(args$focusSelectedOptionOnOpen)) {
    args$focusSelectedOptionOnOpen <- FALSE
  }
  # args$zIndex <- 1000L
  # args$additionalClasses <- 'h-100 w-100'
  if (fill) {
    args$width <- '100%'
  }
  do.call(shinyWidgets::virtualSelectInput, args)
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
      style = sprintf('
        color: #3F3F3F;
        background-color: %s;
        border-top: 2px solid transparent;
        border-radius: 5px;
        padding: 10px;
        font-size: smaller;', color),
      shiny::tags$p(class = 'intro', title, style = 'font-size: 14px'),
      ...
    )
  )
}

#' Create a launcher and updater for Windows (.bat)
#'
#' @details On Windows a file named 'launch.bat' and one named 'update.bat' will be put on the desktop. Double-click the file to launch the specified app app or update app to the latest version
#'
#' @examples
#' \dontrun{
#' win_launcher()
#' }
#'
win_launcher <- function() {
  if (!interactive()) {
    stop('This function can only be used in an interactive R session')
  }

  if (Sys.info()['sysname'] != 'Windows') {
    message('This function is for Windows only. For Mac use the mac_launcher() function')
  }

  out <- prompt(sprintf('Do you want to create shortcuts for %s on your Desktop? (y/n) ', getPackageDescription()$Title))

  if (substr(out, 1L, 1L) %in% c('y', 'Y')) {
    local_dir <- Sys.getenv('R_LIBS_USER')

    if (!fs::file_exists(local_dir)) {
      fs::dir_create(local_dir, recurse = TRUE)
    }

    pt <- fs::path(Sys.getenv('HOME'), 'Desktop')
    if (!fs::file_exists(pt)) {
      pt <- fs::path(Sys.getenv('USERPROFILE'), 'Desktop', fsep = '\\')
    }

    if (!fs::file_exists(pt)) {
      pt <- Sys.getenv('HOME')
      message(paste0('
      The launcher function was unable to find your Desktop. The launcher and
      update files/icons will be put in the directory: ', pt))
    }

    pt <- fs::path_norm(pt)

    fp <- fs::path(pt, 'launch.bat')

    commands <- sprintf(
      '"%s" -e "
      if (!require(%s)) {
        install.packages(\'%s\', type = \'binary\',
          repos = c(
            \'http://cran.msf.net\',
            \'http://cloud.r-project.org\'
          )
        )
      };

      library(%s);

      app <- Application$new();
      app$launch()"',
      Sys.which('R'),
      getPackageDescription()$Package,
      getPackageDescription()$Package,
      getPackageDescription()$Package
    )

    cat(gsub('\n *', '', commands), file = fp, sep = '\n')

    Sys.chmod(fp, mode = '0755')

    fp <- fs::path(pt, 'update.bat')

    commands <- sprintf(
      '"%s" -e "
      install.packages(\'%s\', type = \'binary\',
        repos = c(
          \'http://cran.msf.net\',
          \'http://cloud.r-project.org\'
        )
      );
      suppressWarnings(
        update.packages(\'%s\', ask = FALSE, type = \'binary\',
          lib.loc = .libPaths()[1],
          repos = c(
            \'http://cran.msf.net\',
            \'http://cloud.r-project.org\'
          )
        )
      );
      "',
      Sys.which('R'),
      getPackageDescription()$Package,
      getPackageDescription()$Package
    )

    cat(gsub('\n *', '', commands), file = fp, sep = '\n')

    Sys.chmod(fp, mode = '0755')

    if (fs::file_exists(fp) && fs::file_exists(fp)) {
      message('Done! Look for a file named launch.bat on your desktop. Double-click it to start app in your default browser. There is also a file called update.bat you can double click to update the version of app on your computer.\n')
    } else {
      message('Something went wrong. No shortcuts were created.')
    }
  } else {
    message('No shortcuts were created.\n')
  }
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
      shinyjs::delay(2000L, shinyjs::hide(
        selector = doneEl, anim = TRUE, animType = 'fade',
        time = 0.5
      ))
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

withProgressDialog <- function(expr,
                               min = 0L, max = 1L, value = min + (max - min) * 0.1,
                               message = NULL, detail = NULL,
                               session = shiny::getDefaultReactiveDomain(),
                               env = parent.frame(), quoted = FALSE) {
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

# styler: block Process

runBackgroundApp <- function(
    app = NULL,
    func = NULL,
    port = getOption('shiny.port', 8000L),
    host = getOption('shiny.host', '127.0.0.1'),
    stdout = '',
    stderr = '',
    force = FALSE,
    load_all = TRUE) {
  runApp <- function(app, func, host, port, load_all) {
    if (load_all) {
      devtools::load_all()
    }

    if (!is.null(func) && is.null(app)) {
      app <- func(port = port)
    } else {
      shiny::runApp(appDir = app, host = host, port = port)
    }
  }

  pid <- getProcessPidByPort(port = port)

  if (!is.null(pid)) {
    if (force) {
      tools::pskill(pid)
    } else {
      cli::cli_abort('Port {port} is already used by a process (pid: {pid}).')
    }
  }

  args <- list(app = app, func = func, host = host, port = port, load_all = load_all)

  app <- callr::r_bg(
    func = runApp,
    args = args,
    stdout = stdout,
    stderr = stderr
  )

  cli::cli_progress_bar('Start shiny application...')
  while (!pingr::is_up(destination = '127.0.0.1', port = port, check_online = FALSE, timeout = 0.1)) {
    cli::cli_progress_update()
  }

  invisible(app)
}
