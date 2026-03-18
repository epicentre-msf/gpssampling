ModalDialogQRCode <- R6::R6Class(
  'ModalDialogQRCode',
  inherit = ModalDialog,
  portable = FALSE,
  public = list(
    initialize = function(id, parent = NULL) {
      super$initialize(id, lbl_title = .('Download'), parent = parent)
    },
    show = function(ns = shiny::NS(NULL), qrcode) {
      private$.qrcode <- qrcode
      super$show(ns = ns)
    },
    getUIDialog = function() {
      shiny::div(
        style = 'padding: 10px',
        shiny::plotOutput(ns('plot'), height = 300L, width = 300L)
      )
    },
    getUIFooter = function() {
      shiny::tagList(
        button(inputId = ns('act_ok'), label = private$.lbl_ok, semantic = 'primary')
      )
    }
  ),
  private = list(
    .qrcode = NULL,
    getServer = function(input, output, session, vars) {
      super$getServer(input, output, session, vars)

      output$plot <- shiny::renderPlot({
        x <- qrcode::qr_code(private$.qrcode)
        plot(x)
      })
    }
  )
)
