
Setting <- R6::R6Class(
  classname = 'Setting',
  active = list(
    value = function(value) {
      if (missing(value)) {
        private$.value_trigger$depend()
        return(private$.value)
      } else {
        if (!identical(private$.value, value)) {
          private$.value <- value
          private$.value_trigger$trigger()
        }
      }
    }
  ),
  public = list(
    initialize = function(value = NULL) {
      private$.value <- value
      private$.value_trigger <- reactiveTrigger()
    }
  ),
  private = list(
    .value = NULL,
    .value_trigger = NULL
  )
)

Settings <- R6::R6Class(
  classname = 'Settings',
  public = list(
    initialize = function(value = NULL) {
      private$.settings <- list()
      private$.settings_trigger <- reactiveTrigger()
    },
    addSetting = function(setting, value) {
      private$.settings[[setting]] <- Setting$new(value)
    },
    getValue = function(setting) {
      private$.settings[[setting]]$value
    },
    setValue = function(setting, value) {
      private$.settings[[setting]]$value <- value
    }
  ),
  private = list(
    .settings = NULL,
    .settings_trigger = NULL
  )
)
