# styler: block

getChange <- function(data) {
  changes <- data$changes$changes
  if (is.null(changes)) {
    changes <- data.frame(
      recs = integer(),
      vars = character(),
      values_old = character(),
      values_new = character(),
      stringsAsFactors = FALSE
    )
  } else {
    changes <- base::t(matrix(unlist(changes, FALSE), 4L, length(changes)))
    changes <- as.data.frame(changes)
    names(changes) <- c('recs', 'vars', 'values_old', 'values_new')
    changes$recs <- as.integer(changes$recs) + 1L
    changes$vars <- unlist(data$params$rColnames)[as.integer(changes$vars) + 1L]
  }
  changes
}

getMergeCells <- function(tbl, cols) {
  l <- list()
  for (i in seq_along(cols)) {
    r <- tbl |>
      dplyr::group_by_at(1:i) |>
      dplyr::summarize(n = dplyr::n()) |>
      dplyr::pull(n)
    rc <- cumsum(r)
    l <- append(
      l,
      purrr::map2(
        (rc - r),
        r,
        ~ list(row = .x, col = cols[i] - 1L, rowspan = .y, colspan = 1L)
      )
    )
  }
  l
}

rhandsontable <- function(data, nestedHeaders = NULL, ...) {
  rht <-
    rhandsontable::rhandsontable(
      allowInsertColumn = FALSE,
      allowInvalid = FALSE,
      data = data,
      enterBeginsEditingBoolean = FALSE,
      enterMoves = list(row = 0L, col = 1L),
      licenseKey = 'non-commercial-and-evaluation',
      filters = TRUE,
      manualColumnResize = TRUE,
      manualRowResize = FALSE,
      multiColumnSorting = TRUE,
      nestedHeaders = nestedHeaders,
      overflow = 'visible',
      search = TRUE,
      selectCallback = TRUE,
      stretchH = 'last',
      trimDropdown = FALSE,
      width = '100%',
      height = '100%',
      ...
    ) |>
    rhandsontable::hot_table(
      highlightCol = TRUE,
      highlightRow = TRUE
    )
  rht
}

rhandsontableOutput <- function(..., fill = TRUE) {
  if (fill) {
    args <- rlang::list2(...)
    args$width <- '100%'
    args$height <- 'auto'
    tag <-
      bslib::card(
        # div(style = 'height: 100%; width: 100%; background-color:red;padding: 15px',
        do.call(rhandsontable::rHandsontableOutput, args)
        # )
      )
  } else {
    tag <- rhandsontable::rHandsontableOutput(...)
  }
  tag
}

rhandsontableSetData <- function(id, data, session) {
  data <- data |>
    dplyr::mutate(dplyr::across(where(is.numeric), as.character))

  data[is.na(data)] <- ''

  hot_load_data(id = id, data = data, session = session)
  hot_render(id = id, session = session)
}

#' Push new data to an existing Handsontable widget
#' @noRd
hot_load_data <- function(
  id,
  data,
  session = shiny::getDefaultReactiveDomain()
) {
  session$sendCustomMessage(
    "hot-load-data",
    list(id = id, data = jsonlite::toJSON(data, dataframe = "rows"))
  )
}

#' Force a Handsontable widget to re-render
#' @noRd
hot_render <- function(id, session = shiny::getDefaultReactiveDomain()) {
  session$sendCustomMessage("hot-render", list(id = id))
}

#' Scroll to and highlight a specific cell
#' @noRd
hot_select_cell <- function(
  id,
  row,
  col,
  session = shiny::getDefaultReactiveDomain()
) {
  session$sendCustomMessage(
    "hot-select-cell",
    list(id = id, row = row, col = col)
  )
}

#' Apply a text filter to a column
#' @noRd
hot_filter <- function(
  id,
  column,
  filter,
  session = shiny::getDefaultReactiveDomain()
) {
  session$sendCustomMessage(
    "hot-filter",
    list(id = id, column = column, filter = filter)
  )
}
