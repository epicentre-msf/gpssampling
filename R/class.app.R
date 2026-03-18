#' Application R6 class
#'
#' This class represents an application with various methods and functionalities.
#'
#' @format A list of additional arguments
#'
#' @field data (`character(1)`)
#'   A function to retrieve the application's data
#'
#' @field dlg_method (`character(1)`)
#'   A function to retrieve the dialog method
#'
#' @field keypress (`character(1)`)
#'   A function to retrieve the keypress
#'
#' @section Public:
#'
#' @describeIn launch Launch a project
#' @param open logical indicating whether or not to open the project on launch
#' @param port Optional numeric port number for the project
#' @param method An optional character string specifying a specific method for the project
#' @param options An optional list of named attributes that are passed as options to the project launch
#' @param authentification Logical indicating whether or not to use authentication
#' @param identify Logical indicating whether or not to use identify part only
#' @return The launch application
#'
#' @describeIn initialize Initializes the object
#' @param ... A list of additional arguments
#'
#' @section Private:
#'
#' @describeIn getTitle
#'   A function to get the package title
#'
#' @describeIn getUIMenu
#'   A function to get the UI menu
#'
#' @describeIn getServerInternal
#'   A function to get the server internal
#'
#' @describeIn onChangeLanguage
#'   A function to handle language change
#'
#' @describeIn onSessionEnded
#'   A function to handle session end
#'
#' @describeIn start
#'   A function to start the application
#'
#' @export
#'
Application <- R6::R6Class(
  classname = 'Application',
  inherit = AppShiny,
  portable = FALSE,
  active = list(
    #' @field data (`character(1)`)\cr
    data = function() {
      self$domain$userData$data
    },
    #' @field dlg_method (`character(1)`)\cr
    dlg_method = function() {
      self$domain$userData$dlg_method
    },
    #' @field keypress (`character(1)`)\cr
    keypress = function() {
      private$.keypress
    }
  ),
  public = list(

    #' Initializes the object
    #'
    #' This function initializes the object with given arguments.
    #'
    #' @param ... A list of additional arguments
    #'
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

    #' Launch a project
    #'
    #' This function launches a project with given parameters.
    #'
    #' @param open logical indicating whether or not to open the project on launch
    #' @param port Optional numeric port number for the project
    #' @param method An optional character string specifying a specific method for the project
    #' @param options An optional list of named attributes that are passed as options to the project launch
    #' @param authentification Logical indicating whether or not to use authentication
    #' @param identify Logical indicating whether or not to use identify part only
    #'
    #' @return The launch application
    #'
    launch = function(open = TRUE, port = NULL, options = list(), test = FALSE, test_record = FALSE, authentification = FALSE,
                      method = NULL,
                      identify = FALSE,
                      bbox = c(13.02915, 11.76533, 13.24897, 11.89713)) {

      private$.bbox <- bbox

      if (identify == 'identify') {
        authentification <- FALSE
      }

      app <- super$launch(open, port, options, test, test_record, authentification) # nolint: undesirable_function_linter.

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

      addModule('dlg_method', ModalDialogMethod$new(id = 'dlg_method', parent = self))

      if (!is.null(isolate(self$url_params$method))) {
        data$project_method <- method
      }

      addModule('data', UserData$new(application = self))

      logDebug(getDirApp())
      logDebug(data$cache)

      shiny::observeEvent(input$opt_project_current, ignoreInit = TRUE, {
        data$projectSelect(name = input$opt_project_current)
        shiny::updateTabsetPanel(session, 'app-steps-tbs_steps', selected = 'Delimit')
      })

      shiny::observeEvent(data$project_name, {
        shinyjs::delay(250L, updatePickerInputEx(session, 'opt_project_current', choices = data$projects, selected = data$project_name))
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
        .globals$session$tokens <- .globals$session$tokens[.globals$session$tokens != session$token]
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

ApplicationModule <- R6::R6Class(
  classname = 'ApplicationModule',
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
      while (class(private$.application)[1L] != 'Application') {
        private$.application <- private$.application$parent
      }
    }
  ),
  private = list(
    .application = NULL
  )
)
