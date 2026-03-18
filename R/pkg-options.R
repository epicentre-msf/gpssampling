# styler: block 

#' Retrieve a package-level option from the global options store.
#'
#' @param name Character name of the option to retrieve.
#' @param default Value to return if the option is not set. Default: NULL.
#'
#' @return The stored option value, or `default` if not found.
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
