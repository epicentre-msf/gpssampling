#' @title Base R6 Class
#'
#' @description
#' This function defines an R6 class called "Base". The class
#' has a single public method called "initialize", which creates
#' a new instance of the class and sets its "parent" attribute to
#' the value passed in as an argument (or NULL if no argument is provided).
#'
#' If a global option called "epi.log" is set to TRUE, all functions defined in the
#' class are decorated with logging functionality. The class also has a single
#' active field called "parent", which can be used to get or set the value of
#' the "parent" attribute.
#'
#' @return An R6 class object.
#'
#' @examples
#' # Create a new instance of the Base class with no parent
#' my_base <- Base$new()
#'
#' # Create a new instance of the Base class with a parent
#' my_child_base <- Base$new(parent = my_base)
#'
#' # Get the value of the "parent" attribute for an instance of the Base class
#' my_base$parent()
#'
#' # Set the value of the "parent" attribute for an instance of the Base class
#' my_base$parent(my_child_base)
#'
Base <- R6::R6Class(
  classname = 'Base',
  portable = FALSE,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param parent (`R6Class`)\cr
    #'   Identifier of the parent object.
    #'
    initialize = function(parent = NULL) {
      # Set the "parent" attribute of the object to the value passed in as an argument (or NULL if no argument is provided)
      self$parent <- parent

      # If a global option called "epi.log" is set to TRUE, decorate all functions defined in the class with logging functionality
      if (getOption('epi.log', FALSE)) {
        # Identify all the functions defined in the class that are not part of the active or private components
        funcs_active <- names(self$.__active__)
        funcs <- names(self)[-(1:2)]
        funcs <- funcs[!(funcs %in% c('clone', 'finalize', 'initialize', 'self', 'super', 'private'))]
        funcs <- funcs[!(funcs %in% funcs_active)]

        # For each function identified, check if it is actually a function and decorate it with logging functionality if it is
        for (func in funcs) {
          if (class(self[[func]])[1L] == 'function') {
            logDebug('Decorate %s', func)
            unlockBinding(func, self)
            self[[func]] <- logDecorate(class(self)[1L], func, self[[func]])
            lockBinding(func, self)
          }
        }

        # Repeat the above process for all functions defined in the private component of the class, except for two specific functions
        funcs <- names(self$private)[-(1:2)]
        funcs <- funcs[!(funcs %in% c('getServer', 'console_out'))]
        for (func in funcs) {
          if (class(self$private[[func]])[1L] == 'function') {
            logDebug('Decorate %s', func)
            unlockBinding(func, self$private)
            self$private[[func]] <- logDecorate(class(self)[1L], func, self$private[[func]])
            lockBinding(func, self$private)
          }
        }

      }

    }
  ),
  active = list(
    #' @field parent (`R6Class`)\cr
    #' Parent object of the object.
    parent = function(value) {
      if (!missing(value)) {
        if (!is.null(value)) {
          checkmate::assert_class(value, classes = c('Base', 'R6'))
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
