# styler: block 

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#'
#' @param name PARAM_DESCRIPTION
#' @param default PARAM_DESCRIPTION, Default: NULL
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#'
#' @noRd
#'
Option <- function(name, default = NULL) {
  # Make sure to use named (not numeric) indexing
  name <- as.character(name)

  if (name %in% names(.globals$options)) {
    .globals$options[[name]]
  } else {
    default
  }
} 

#' @noRd
#'
setOption <- function(name, value) {
  .globals$options[[name]] <- value
} 

#' @noRd
#'
setOptions <- function(...) {

  newOpts <- list(...)

  if (length(newOpts) > 0L) {
    .globals$options <- c(.globals$options, newOpts)
    invisible(.globals$options)
  } else {
    .globals$options
  }
} 

# = pkg-options
# A scope where we can put mutable global state
.globals <- new.env(parent = emptyenv())
.globals$options <- list()
