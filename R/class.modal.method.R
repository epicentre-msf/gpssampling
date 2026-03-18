ModalDialogMethod <- R6::R6Class(
  classname = 'ModalDialogMethod',
  inherit = ModalDialog,
  portable = FALSE,
  active = list(
    method = function() {
      private$.method
    }
  ),
  public = list(
    initialize = function(id, parent = NULL) {
      super$initialize(id, lbl_title = .('Sample Method'), parent = parent)

      private$.method <- 'RS_SMP'
    },
    getUIDialog = function() {
      shiny::div(
        style = 'padding: 10px',
        shiny::radioButtons(
          inputId = ns('opt_method'),
          label = .('Choose Method:'),
          choices = getChoices(df_sampling_method),
          selected = private$.method
        )
      )
    },
    getUIFooter = function() {
      button(inputId = ns('act_ok'), label = private$.lbl_ok, semantic = 'primary')
    }
  ),
  private = list(
    .method = NULL,
    getServer = function(input, output, session, vars) {
      super$getServer(input, output, session)

      shiny::observeEvent(input$opt_method, ignoreInit = TRUE, {
        private$.method <- input$opt_method
      })
    }
  )
)
