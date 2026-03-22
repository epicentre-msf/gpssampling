#' GpsSampler R6 Class
#'
#' Main entry point for the GPS sampling Shiny application.
#' Use the [sampler()] factory function for idiomatic construction.
#'
#' @param ... Additional arguments passed to the parent class.
#' @param open Logical; open the app in a browser on launch.
#' @param port Optional numeric port number.
#' @param options Named list of Shiny options.
#' @param test Logical; run in test mode.
#' @param test_record Logical; enable shinytest2 recording.
#' @param authentification Logical; enable OAuth2 authentication.
#' @param method Character; sampling method code (e.g. `"RS_SMP"`, `"SP_SMP"`).
#' @param identify Logical; run in identify-only mode.
#' @param bbox Numeric vector of length 4; default bounding box.
#'
#' @export
#'
GpsSampler <- R6::R6Class(
  classname = 'GpsSampler',
  inherit = AppShiny,
  portable = FALSE,
  active = list(
    #' @field data The application's UserData store.
    data = function() {
      self$domain$userData$data
    },
    #' @field dlg_method The method selection dialog.
    dlg_method = function() {
      self$domain$userData$dlg_method
    },
    #' @field keypress The keyboard shortcut handler.
    keypress = function() {
      private$.keypress
    }
  ),
  public = list(
    #' @description Create a new GpsSampler instance.
    initialize = function(...) {
      super$initialize(...)

      private$.keypress <-
        ModKey$new(
          keys = c(
            '-',
            'delete',
            'enter',
            paste0('alt+', 0:9),
            'space',
            0:9
          )
        )
    },

    #' @description Launch the Shiny application.
    #' @return A `shiny.appobj` object.
    launch = function(
      open = TRUE,
      port = NULL,
      options = list(),
      test = FALSE,
      test_record = FALSE,
      authentification = FALSE,
      method = NULL,
      identify = FALSE,
      bbox = c(13.02915, 11.76533, 13.24897, 11.89713)
    ) {
      private$.bbox <- bbox

      if (identify == 'identify') {
        authentification <- FALSE
      }

      app <- super$launch(
        open,
        port,
        options,
        test,
        test_record,
        authentification
      ) # nolint: undesirable_function_linter.

      if (!is.null(method)) {
        data$project_method <- method
      }

      app
    }
  ),
  private = list(
    .bbox = NULL,
    .keypress = NULL,
    getTitle = function() {
      getPackageTitle()
    },
    getUIHeader = function() {
      shiny::tagList(
        keypress$getUI(),
        super$getUIHeader()
      )
    },
    getUIMenu = function() {
      data$steps$getUI(ns = shiny::NS('app'))
    },
    # getUIMenuSecond = function() {
    #   shiny::navbarMenu(
    #     title = .('Project'), icon = icon('file-document-outline'),
    #     shiny::tabPanel(actionLink(inputId = 'act_import_export', label = .('Import / Export')))
    #   )
    # },
    getServerInternal = function(input, output, session) {
      super$getServerInternal(input, output, session)

      private$.keypress$bind()

      addModule(
        'dlg_method',
        ModalDialogMethod$new(id = 'dlg_method', parent = self)
      )

      if (!is.null(isolate(self$url_params$method))) {
        data$project_method <- method
      }

      addModule('data', UserData$new(application = self))

      logDebug(getDirApp())
      logDebug(data$cache)

      shiny::observeEvent(input$opt_project_current, ignoreInit = TRUE, {
        data$projectSelect(name = input$opt_project_current)
        shiny::updateTabsetPanel(
          session,
          'app-steps-tbs_steps',
          selected = 'Delimit'
        )
      })

      shiny::observeEvent(data$project_name, {
        shinyjs::delay(
          250L,
          updatePickerInputEx(
            session,
            'opt_project_current',
            choices = data$projects,
            selected = data$project_name
          )
        )
      })

      shiny::observeEvent(dlg_method$ok, ignoreInit = TRUE, {
        data$project_method <- dlg_method$method
      })

      shiny::observeEvent(input$act_import_export, {
        dlg <- ModalDialogImportExport$new(id = 'import_export', parent = self)
        dlg$bind()
        dlg$show()
      })
    },
    onChangeLanguage = function() {
      super$onChangeLanguage()
      data$steps$step_delimit$invalidateState()
    },
    onSessionEnded = function(session) {
      shiny::isolate({
        .globals$session$tokens <- .globals$session$tokens[
          .globals$session$tokens != session$token
        ]
      })
      super$onSessionEnded(session)
    },
    start = function() {
      super$start()

      if (data$project_method == 'UNK') {
        dlg_method$show(ns = shiny::NS(NULL))
      }
    }
  )
)

#' @noRd
GpsSamplerModule <- R6::R6Class(
  classname = 'GpsSamplerModule',
  inherit = ModShiny,
  portable = FALSE,
  active = list(
    application = function() {
      private$.application
    },
    data = function() {
      application$data
    }
  ),
  public = list(
    initialize = function(id = 'mod', parent = NULL) {
      super$initialize(id = id, parent = parent)

      private$.application <- parent
      while (!inherits(private$.application, 'GpsSampler')) {
        private$.application <- private$.application$parent
      }
    }
  ),
  private = list(
    .application = NULL
  )
)

#' @export
#' @noRd
ApplicationModule <- GpsSamplerModule

#' Create a GPS sampler instance
#'
#' Factory function for creating a [GpsSampler] object.
#' This is the recommended way to instantiate the sampling application.
#'
#' @param ... Additional arguments passed to [GpsSampler]`$new()`.
#' @return A [GpsSampler] R6 object.
#'
#' @examples
#' \dontrun{
#' samp <- sampler()
#' samp$launch()
#' }
#'
#' @export
sampler <- function(...) {
  GpsSampler$new(...)
}

#' @export
#' @noRd
Application <- GpsSampler
