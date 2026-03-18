TabPanel <- R6::R6Class(
  'TabPanel',
  inherit = ApplicationModule,
  portable = FALSE,
  active = list(
    dialog = function() {
      parent
    }
  ),
  public = list(
    initialize = function(id, lbl_title, parent = NULL) {
      super$initialize(id = id, parent = parent)
      private$.lbl_title <- lbl_title
    },
    getUI = function(ns = shiny::NS(NULL)) {
      super$getUI(ns)
      fillTabPanel(
        value = ns('tab'), title = private$.lbl_title, height = NULL,
        shiny::fillPage(
          self$getUIPanel()
        )
      )
    },
    getUIPanel = function() {
    }
  ),
  private = list(
    .lbl_title = NULL
  )
)
