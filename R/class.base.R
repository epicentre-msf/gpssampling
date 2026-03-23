#' @title Base R6 Class
#'
#' @description
#' Root R6 class for the gpssampling class hierarchy. Provides optional
#' method tracing via the `gpssampling.trace` option or the `.trace` parameter.
#'
#' When tracing is enabled, all public and private methods are wrapped with
#' entry/exit logging and elapsed-time measurement via `logDecorate()`.
#'
#' @return An R6 class object.
#'
#' @keywords internal
Base <- R6::R6Class(
  classname = "Base",
  portable = FALSE,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param parent (`R6Class`)\cr
    #'   Identifier of the parent object.
    #' @param .trace (`logical(1)`)\cr
    #'   Enable method tracing for this instance. Defaults to
    #'   `getOption("gpssampling.trace", FALSE)`.
    #'
    initialize = function(parent = NULL, .trace = NULL) {
      self$parent <- parent

      trace <- .trace %||% getOption("gpssampling.trace", FALSE)
      if (isTRUE(trace)) {
        decorate_methods(self, private)
      }
    }
  ),
  active = list(
    #' @field parent (`R6Class`)\cr
    #' Parent object of the object.
    parent = function(value) {
      if (!missing(value)) {
        if (!is.null(value)) {
          checkmate::assert_class(value, classes = c("Base", "R6"))
        }
        private$.parent <- value
      }
      private$.parent
    }
  ),
  private = list(
    .parent = NULL
  )
)
