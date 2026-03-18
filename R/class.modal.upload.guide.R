
ModalDialogTabUploadGuide <- R6::R6Class(
  'ModalDialogTabUploadGuide',
  inherit = ModalDialogTab,
  portable = FALSE,
  public = list(
    initialize = function(id, lbl_title = '', lbl_ok = .('OK'), lbl_cancel = .('Cancel'), type = 'polygon', parent = NULL) {
      super$initialize(id, lbl_title = lbl_title, lbl_ok = lbl_ok, lbl_cancel = lbl_cancel, parent = parent)
      if (type == 'polygon') {
        private$.tab_1 <- TabPanelLeafletFilePolygon$new(id = 'tab_1', lbl_title = .('From File'), parent = self)
      } else {
        private$.tab_1 <- TabPanelLeafletFilePoint$new(id = 'tab_1', lbl_title = .('From File'), parent = self)
      }
    },
    getUITabs = function() {
      list(
        private$.tab_1$getUI(ns = ns)
      )
    }
  ),
  private = list(
    .tab_1 = NULL,
    getServer = function(input, output, session) {
      super$getServer(input, output, session)

      private$.tab_1$bind()
    }
  )
)
