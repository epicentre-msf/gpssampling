
ModalDialogTabUploadIdentify <- R6::R6Class(
  'ModalDialogTabUploadIdentify',
  inherit = ModalDialogTab,
  portable = FALSE,
  public = list(
    initialize = function(id, lbl_title = '', lbl_ok = .('OK'), lbl_cancel = .('Cancel'), parent = NULL) {
      super$initialize(id, lbl_title = lbl_title, lbl_ok = lbl_ok, lbl_cancel = lbl_cancel, parent = parent)
      private$.tab_1 <- TabPanelLeafletGoogle$new(id = 'tab_1', lbl_title = .('From Google Map'), parent = self)
      # private$.tab_2 <- TabPanelLeafletOSM$new(id = 'tab_2', lbl_title = .('From OpenStreetMap'), parent = self)
      private$.tab_3 <- TabPanelLeafletFilePoint$new(id = 'tab_3', lbl_title = .('From File'), parent = self)
    },
    getUITabs = function() {
      list(
        private$.tab_1$getUI(ns = ns),
        # private$.tab_2$getUI(ns = ns),
        private$.tab_3$getUI(ns = ns)
      )
    }
  ),
  private = list(
    .tab_1 = NULL,
    # .tab_2 = NULL,
    .tab_3 = NULL,
    getServer = function(input, output, session) {
      super$getServer(input, output, session)

      private$.tab_1$bind()
      # private$.tab_2$bind()
      private$.tab_3$bind()
    }
  )
)
