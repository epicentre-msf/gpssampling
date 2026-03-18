
ModalDialogTabOffline <- R6::R6Class(
  'ModalDialogTabOffline',
  inherit = ModalDialogTab,
  portable = FALSE,
  public = list(
    initialize = function(id, lbl_title = '', lbl_ok = .('OK'), lbl_cancel = .('Cancel'), parent = NULL) {
      super$initialize(id, lbl_title = lbl_title, lbl_ok = lbl_ok, lbl_cancel = lbl_cancel, parent = parent)
      private$.tab_1 <- TabPanelLeafletOffline$new(id = 'tab_google', lbl_title = .('From Google Map'), parent = self)
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
