ModalDialogImportExport <- R6::R6Class(
  'ModalDialogImportExport',
  inherit = ModalDialog,
  portable = FALSE,
  public = list(
    initialize = function(id, parent = NULL) {
      super$initialize(id, lbl_title = .('Import / Export'), parent = parent)
    },
    show = function(ns = shiny::NS(NULL)) {
      super$show(ns = ns)
    },
    getUIDialog = function() {
      shiny::div(
        style = 'padding: 10px',
        shiny::fillRow(
          flex = c(30L, 1L, 10L), height = 40L, width = 400L,
          # fillTabset(
          #   id = ns('tbs_import_export'),
          #   fillTabPanel(
          #     title = 'Export', value = ns('tab_export'), icon = icon('export'),
          shiny::fileInput(
            inputId = ns('act_import'), label = NULL, buttonLabel = ..('Import...'),
            placeholder = ..('Select project file...'), width = '100%',
            accept = '.zip'
          ),
          shiny::div(),
          shiny::downloadButton(outputId = ns('act_export'), label = 'Export...')
          # )
          # )
        )
      )
    },
    getUIFooter = function() {
      buttonLoading(inputId = ns('act_ok'), label = private$.lbl_ok, loadingLabel = 'OK', semantic = 'primary')
    }
  ),
  private = list(
    getServer = function(input, output, session, vars) {
      super$getServer(input, output, session, vars)

      output$act_export <- shiny::downloadHandler(
        filename = function() {
          'project.zip'
        },
        content = function(file) {
          export_path <- self$parent$data$export()
          fs::file_copy(path = export_path, new_path = file)
        }
      )
    }
  )
)
