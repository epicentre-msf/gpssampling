# Shiny input component utilities

# styler: block pickerInput

observeEventPickerInputExSpin <- function(
  session,
  input,
  vars,
  inputId,
  choices
) {
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
    choice.next.idx <- min(
      length(choices),
      match(vars[[inputId]], choices) + 1L
    )
    choice.next <- choices[choice.next.idx]
    updatePickerInputEx(session, inputId = inputId, selected = choice.next)
  })

  observe({
    choices <- as.character(unlist(choices))
    shinyjs::toggleState(
      id = paste0(inputId, '_previous'),
      condition = vars[[inputId]] != choices[1L]
    )
    shinyjs::toggleState(
      id = paste0(inputId, '_next'),
      condition = vars[[inputId]] != choices[length(choices)]
    )
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
        button(
          inputId = paste0(args$inputId, '_', btn$id),
          size = size,
          icon = btn$icon,
          label = NULL
        )
      }
    )
    tag_qr <- tag_qr$find('select')
    tag_qr$replaceWith(tags$div(
      class = 'input-group',
      buttons,
      tag_qr$selectedTags()
    ))
  }

  hidden(tag_qr$allTags(), hide = hide)
}

pickerInputEx <- function(
  inputId,
  label,
  choices,
  selected = NULL,
  multiple = FALSE,
  options = list(),
  choicesOpt = NULL,
  width = NULL,
  inline = FALSE,
  size = 'normal',
  buttons = list()
) {
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
    tag = selectTag,
    pickerSelectOptions(choices, selected, choicesOpt, maxOptGroup)
  )

  if (length(buttons)) {
    buttons <- lapply(buttons, function(btn) {
      htmltools::tags$button(
        id = paste0(inputId, '_', btn$id),
        type = 'button',
        class = 'btn btn-default action-button btn-input',
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
  pickerInputEx(
    ...,
    choices = getChoicePalettes(),
    choicesOpt = list(
      content = sprintf(
        'shiny::div(style="width:100%%;padding:2px;border-radius:4px;background:%s">&nbsp;</div>',
        unname(getColorPalettes())
      )
    )
  )
}

pickerInputExSpin <- function(...) {
  pickerInputEx(
    ...,
    buttons = list(
      list(id = 'previous', icon = 'caret-left'),
      list(id = 'next', icon = 'caret-right')
    )
  )
}

pickerInputExSpinRight <- function(...) {
  args <- list(...)
  div(
    class = 'input-group',
    id = paste0(args$inputId, '_div'),
    pickerInputEx(...),
    span(
      class = 'input-group-btn',
      actionButton(
        inputId = paste0(args$inputId, '_previous'),
        label = '',
        icon('caret-left'),
        width = 25L,
        class = 'btn-input'
      ),
      actionButton(
        inputId = paste0(args$inputId, '_next'),
        label = '',
        icon('caret-right'),
        width = 25L,
        class = 'btn-input'
      )
    )
  )
}

pickerSelectOptions <- function(
  choices,
  selected = NULL,
  choicesOpt = NULL,
  maxOptGroup = NULL
) {
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
          choice,
          selected,
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
        disabled = if (
          !is.null(choicesOpt$disabled[i]) && choicesOpt$disabled[i]
        )
          'disabled',
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

updatePickerInputEx <- function(
  session,
  inputId,
  label = NULL,
  selected = NULL,
  choices = NULL,
  choicesOpt = NULL
) {
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
  message <- dropNulls(list(
    label = label,
    choices = choices,
    value = selected,
    options = opts
  ))
  session$sendInputMessage(inputId, message)
}

# styler: block Shiny

actionLink <- function(inputId, label, icon = NULL, ...) {
  shiny::actionLink(inputId = inputId, label = label, icon = icon, ...)
}

checkboxGroupButtons <- function(
  inputId,
  label = NULL,
  choices = NULL,
  size = 'sm',
  ...
) {
  shinyWidgets::checkboxGroupButtons(
    inputId = inputId,
    label = label,
    choices = choices,
    size = size,
    ...
  )
}

fileInput <- function(..., icon = 'upload') {
  args <- rlang::list2(...)
  if (!is.null(icon) && !is.null(args$buttonLabel)) {
    args$buttonLabel <- list(icon(icon, size = NULL), args$buttonLabel)
  }
  do.call(shiny::fileInput, args)
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
      tags$button(
        class = 'btn btn-secondary',
        type = 'button',
        id = paste0(args$inputId, '-btn'),
        'Button'
      )
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

textInputEx <- function(
  inputId,
  label = NULL,
  value = '',
  width = NULL,
  placeholder = NULL,
  inline = FALSE,
  size = 'normal',
  buttons = list()
) {
  size <- match.arg(arg = size, choices = c('sm', 'normal', 'lg'))

  value <- shiny::restoreInput(id = inputId, default = value)

  if (size == 'normal') {
    divInputGroupClass <- 'input-group'
  } else {
    divInputGroupClass <- paste0('input-group input-group-', size)
  }

  textTag <- shiny::tags$input(
    id = inputId,
    type = 'text',
    class = 'form-control',
    value = value,
    placeholder = placeholder
  )

  if (length(buttons)) {
    buttons <- lapply(buttons, function(btn) {
      shiny::tags$button(
        id = paste0(inputId, '_', btn$id),
        type = 'button',
        class = 'btn btn-default action-button btn-input',
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
      style = if (is.defined(width))
        paste0('width: ', htmltools::validateCssUnit(width), ';'),
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

tokenInput <- function(
  inputId,
  label,
  width = NULL,
  buttonLabel = 'Login...',
  placeholder = 'No file selected'
) {
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
    style = if (is.defined(width))
      paste0('width: ', htmltools::validateCssUnit(width), ';'),
    shiny::tags$label(label),
    shiny::div(
      class = 'input-group',
      shiny::tags$input(
        id = inputId,
        type = 'password',
        class = 'form-control',
        placeholder = placeholder,
        style = 'text-align: center'
      ),
      shiny::tags$label(
        class = 'input-group-btn',
        buttonPrimary(
          inputId = paste0(inputId, '_valid'),
          icon = icon('key'),
          label = NULL
        )
      )
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
