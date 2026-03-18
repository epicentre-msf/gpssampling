TabPanelLeafletOSM <- R6::R6Class(
  'TabPanelLeafletOSM',
  inherit = TabPanelLeaflet,
  portable = FALSE,
  public = list(
    getUIPanel = function() {
      shiny::fillCol(
        flex = c(40L, 25L, 280L), height = 350L, width = 600L,
        button(inputId = ns('act_upload'), label = .('Get Building Datas...'), style = 'width: 100%', class = 'btn'),
        shinyWidgets::progressBar(id = ns('pb_upload'), value = 0L),
        super$getUIPanel()
      )
    }
  ),
  private = list(
    getServer = function(input, output, session) {
      super$getServer(input, output, session)

      shiny::observeEvent(input$act_upload, {

        progress <- function(value) {
          shinyWidgets::updateProgressBar(session = application$session, id = 'pb_upload', value = value)
        }

        disableTabPanel(value = self$ns_parent('tab_upload_file'))
        disableTabPanel(value = self$ns_parent('tab_upload_google'))

        sf_file <- data$roofsAddOSM(map = map, progress = progress)

        enableTabPanel(value = self$ns_parent('tab_upload_google'))
        enableTabPanel(value = self$ns_parent('tab_upload_file'))

        # shinyFeedback::resetLoadingButton(inputId = 'act_upload')

        shinyjs::enable(id = self$ns_parent('act_ok'), asis = TRUE)
      })
    },
    renderMap = function() {
      data$displayPolygons(private$.map, only_selected = TRUE)
    }
  )
)
