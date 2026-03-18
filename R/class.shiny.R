AppShinyBase <- R6::R6Class(
  classname = 'AppShinyBase',
  portable = FALSE,
  inherit = Base,
  active = list(
    app = function() {
      private$.app
    },
    user_count = function(value) {
      if (missing(value)) {
        private$.user_count_trigger$depend()
      } else {
        logDebug('Set Query URL %s', value)
        private$.user_count <- value
        private$.user_count_trigger$trigger()
      }
      private$.user_count
    },
    domain = function() {
      shiny::getDefaultReactiveDomain()
    },
    session = function(value) {
      if (!missing(value)) {
        private$.session <- value
      }
      private$.session
    },
    url_params = function(value) {
      if (missing(value)) {
        private$.url_params_trigger$depend()
      } else {
        private$.url_params <- value
        private$.url_params_trigger$trigger()
      }
      private$.url_params
    }
  ),
  public = list(
    initialize = function(debug = TRUE,
                          debug_trace = FALSE,
                          debug_trace_full = FALSE,
                          max_request_zize = 500L * 1024L^2L, ## limit upload filesize on server (10MB)
                          ...) {

      private$.url_params <- ''
      private$.url_params_trigger <- reactiveTrigger()

      private$.user_count <- 0L
      private$.user_count_trigger <- reactiveTrigger()

      options(shiny.maxRequestSize = max_request_zize)
      options(shiny.minified = TRUE)
      options(shiny.usecairo = TRUE)

      if (debug) {
        levelLogger('DEBUG')
        options(shiny.trace = debug_trace)
        options(shiny.fullstacktrace = debug_trace_full)
        options(shiny.error = browser)
        options(error = NULL)
      } else {
        levelLogger('INFO')
        options(shiny.trace = FALSE)
        options(shiny.fullstacktrace = FALSE)
        options(shiny.error = NULL)
        options(error = NULL)
      }
    },
    addModule = function(module_name, module) {
      assign(module_name, module, envir = self$domain$userData)
      self$domain$userData[[module_name]]$bind()
    },
    #' @description
    #' Launch the shiny app
    launch = function(open = TRUE, port = NULL, options = list(), test = FALSE, test_record = FALSE) {
      app <- shiny::shinyApp(
        ui                = private$getUI,
        server            = private$getServer,
        onStart           = private$onStart,
        options           = getShinyOptions(open = open, port = port, options = options)
      )
      if (test_record) {
        app <- shinytest2::record_test(app = app, name = 'azerty')
      }
      if (test) {
        options(shiny.testmode = TRUE)
      }
      app
    },
    shutdown = function() {
      shiny::stopApp()
      cat('\n')
    }
  ),
  private = list(
    .app = NULL,
    .session = NULL,
    .url_params = NULL,
    .url_params_trigger = NULL,
    .user_count = NULL,
    .user_count_trigger = NULL,
    getUI = function(request) {
      shiny::tagList(
        getUIHeader(),
        shiny::fluidPage(
          getUITitle(),
          shiny::sidebarLayout(
            shiny::sidebarPanel(
              getUISidebarPanel(request)
            ),
            shiny::mainPanel(
              getUIMainPanel(request)
            )
          )
        ),
        getUIFooter()
      )
    },
    getUIHeader = function(request) {
      shiny::tagList(
        waiter::useWaiter(),
        waiter::useWaitress(),
        waiter::waiterShowOnLoad(
          html =
            shiny::tagList(
              waiter::spin_loaders(7L, color = col_spin_waiter),
              shiny::h3('Please wait...')
            ),
          color = 'white'
        ),
        shinyjs::useShinyjs()
      )
    },
    getUIFooter = function() {
    },
    getUISidebarPanel = function(request) {
      shiny::sliderInput(inputId = 'bins', label = 'Number of bins:', min = 1L, max = 50L, value = 30L)
    },
    getUIMainPanel = function(request) {
      shiny::plotOutput(outputId = 'distPlot')
    },
    getUITitle = function() {
      shiny::titlePanel(getPackageTitle())
    },
    getServer = function(input, output, session) {
      self$session <- session
      self$url_params <- isolate(shiny::parseQueryString(session$clientData$url_search))

      # Session
      # .................................................

      session$allowReconnect(TRUE)

      session$onSessionEnded(
        function() {
          private$onSessionEnded(session)
        }
      )

      onSessionStart <- eval(expression({
        private$onSessionStart(session)
      }))

      # Shiny events
      # .................................................

      shiny::onFlushed(
        fun = function() {
          start()
        },
        once = TRUE
      )

      shiny::observeEvent(input$stop, {
        self$stop()
      })

      shiny::observe({
        self$url_params <- shiny::parseQueryString(session$clientData$url_search)
      })

      getServerInternal(input, output, session)
      getServerInternalEnd(input, output, session)
    },
    getServerInternal = function(input, output, session) {
      output$distPlot <- shiny::renderPlot({
        x <- faithful$waiting
        bins <- seq(min(x), max(x), length.out = input$bins + 1L)
        hist(x,
          breaks = bins, col = '#75AADB', border = 'white',
          xlab = 'Waiting time to next eruption (in mins)',
          main = 'Histogram of waiting times'
        )
      })
    },
    getServerInternalEnd = function(input, output, session) {
      waiter::waiter_hide()
    },
    getShinyOptions = function(open = TRUE, port = NULL, options = list()) {
      if (!is.null(open)) {
        options <- utils::modifyList(options, list(launch.browser = open))
      }
      if (!is.null(port)) {
        options <- utils::modifyList(options, list(port = port))
      }
      options
    },
    onSessionStart = function(session) {
      logDebug('Session started (%s users connected)', shiny::isolate(self$user_count) + 1L)
      self$user_count <- shiny::isolate(self$user_count) + 1L
    },
    onSessionEnded = function(session) {
      logDebug('Session ended (%s  users connected)', shiny::isolate(self$user_count) - 1L)
      self$user_count <- shiny::isolate(self$user_count) - 1L
      if (
        shiny::isolate(session$clientData$url_hostname) == '127.0.0.1' &
          shiny::isolate(self$user_count) == 0L) {
        shiny::stopApp()
        cat('\n')
      }
    },
    onStart = function() {
      fs::dir_create(path = getDirApp(), recurse = TRUE)
      fs::dir_create(path = getDirAppTemp(), recurse = TRUE)

      shiny::addResourcePath(prefix = 'assets', directoryPath = getDirAssets())
      shiny::addResourcePath(prefix = 'cache', directoryPath = getDirApp())
      shiny::addResourcePath(prefix = 'app', directoryPath = getDirApp())
    },
    start = function() {
    }
  )
)

AppShinyWithAuthentification <- R6::R6Class(
  classname = 'AppShinyWithAuthentification',
  portable = FALSE,
  inherit = AppShinyBase,
  active = list(
    auth = function() {
      private$.auth
    },
    logged = function() {
      !is.null(auth)
    },
    user = function() {
      if (logged) {
        return(self$domain$user)
      } else {
        username <- getOption('epi.user.default.name')
        return(
          list(
            email_verified = TRUE,
            name = username,
            preferred_username = sprintf('%s@epicentre.msf.org', username),
            locale = 'fr',
            given_name = stringr::str_to_title(username),
            family_name = stringr::str_to_title(username),
            email = sprintf('%s@epicentre.msf.org', username)
          )
        )
      }
    }
  ),
  public = list(
    initialize = function(...) {
      super$initialize(...)
    },
    launch = function(open = TRUE, port = NULL, options = list(), test = FALSE, test_record = FALSE, authentification = FALSE) {

      if (authentification) {

        private$.auth <- ShinyAuth$new(
          client_id = Sys.getenv('KEYCLOAK_CLIENT_ID'),
          client_secret = Sys.getenv('KEYCLOAK_CLIENT_SECRET'),
          auth_domain = Sys.getenv('KEYCLOAK_AUTH_DOMAIN'),
          realm = Sys.getenv('KEYCLOAK_REALM')
        )

        app <- shiny::shinyApp(
          ui                = auth$ui(private$getUI),
          server            = auth$server(private$getServer),
          onStart           = private$onStart,
          options           = getShinyOptions(open = open, port = port, options = options)
        )

      } else {
        app <- super$launch(open, port, options, test, test_record)
      }
      app
    },
    logout = function() {
      if (logged) {
        shinyjs::runjs(sprintf('location.replace("%s")', auth$url_logout()))
      }
    }
  ),
  private = list(
    .auth = NULL,
    getUIHeader = function(request) {
      shiny::tagList(
        super$getUIHeader(),
        if (logged) {
          auth$scripts()
        }
      )
    }
  )
)

AppShinyWithTranslation <- R6::R6Class(
  classname = 'AppShinyWithTranslation',
  portable = FALSE,
  inherit = AppShinyWithAuthentification,
  active = list(
    language = function(value) {
      if (missing(value)) {
        private$.language_trigger$depend()
      } else {
        if (!identical(private$.language, value)) {
          translator$set_translation_language(value)

          private$.language <- value
          private$.language_trigger$trigger()

          onChangeLanguage()

        }
      }
      private$.language
    },
    translator = function() {
      private$.translator
    }
  ),
  public = list(
    initialize = function(...) {
      super$initialize(...)

      private$.language <- 'en'
      private$.language_trigger <- reactiveTrigger()

      private$.translator <- translator()
      private$.translator$set_translation_language(private$.language)
    }
  ),
  private = list(
    .translator = NULL,
    .language = NULL,
    .language_trigger = NULL,
    getChoicesLanguage = function() {

      c_named(
        c('en', 'fr'),
        c('EN', 'FR')
      )

    },
    getUIHeader = function(request) {
      shiny::tagList(
        super$getUIHeader(request),
        shiny.i18n::usei18n(translator)
      )
    },
    getUISidebarPanel = function(request) {
      shiny::tagList(
        getUILanguage(),
        shiny::sliderInput(inputId = 'bins', label = .('Delimit'), min = 1L, max = 50L, value = 30L)
      )
    },
    getUILanguage = function(request) {
      pickerInputEx(
        inputId = 'opt_language',
        label = NULL,
        choices = getChoicesLanguage(),
        selected = 'en',
        size = 'sm',
        width = 60L
      )
    },
    getServer = function(input, output, session) {
      super$getServer(input, output, session)

      shiny::observeEvent(input$opt_language, ignoreInit = TRUE, {
        self$language <- input$opt_language
      })
    },
    onChangeLanguage = function() {
      shiny.i18n::update_lang(session = self$session, self$language)
    }
  )
)

AppShinyNav <- R6::R6Class(
  classname = 'AppShinyNav',
  portable = FALSE,
  inherit = AppShinyWithTranslation,
  active = list(),
  public = list(
    initialize = function(...) {
      super$initialize(...)
    }
  ),
  private = list(
    getUI = function(request) {
      getUINavBar()
    },
    # getUIHeader = function(request) {
    # htmltools::includeCSS(fs::path(getDirAssets(), 'css/shiny.css'))
    # },
    getUINavBar = function() {
      page_navbar(
        collapsible = TRUE,
        id = 'navbar',
        fluid = TRUE,
        header = getUIHeader(),
        inverse = FALSE,
        position = 'fixed-top',
        title = getPackageDescription()$Title,
        theme = bslib::bs_theme(
          # "font-size-base" = "0.7rem",
          # "enable-rounded" = FALSE,
          # "line-height-base" = 0.7,
          # "line-height-sm" = 0.7,
          # "line-height-lg" = 0.7,
          spacer = '0.2rem',
          # "bs-gutter-x" = 0,
          version = 5L
        ),
        !!!getUINavs()
      )
    },
    getUINavs = function() {
      list(
        nav('a',
          icon = icon('github'),
          navs_tab_card(
            nav('a1', shiny::wellPanel('tab a1 content')),
            nav('a2', shiny::wellPanel('tab a2 content'))
          )
        ),
        nav('b', shiny::wellPanel('tab b content')),
        bslib::nav_item(
          tags$a(icon('github'), 'Shiny', href = 'https://github.com/rstudio/shiny', target = '_blank')
        ),
        bslib::nav_spacer(),
        bslib::nav_menu(
          'Other links',
          align = 'right',
          bslib::nav('c', 'tab c content'),
          bslib::nav_item(
            tags$a(icon('r-project'), 'RStudio', href = 'https://rstudio.com', target = '_blank')
          )
        )
      )
    },
    getServer = function(input, output, session) {
      super$getServer(input, output, session)
    }
  )
)

AppShinyFillPage <- R6::R6Class(
  classname = 'AppShinyFillPage',
  portable = FALSE,
  inherit = AppShinyBase,
  private = list(
    getUI = function(request) {
      getUIFillPage()
    },
    getUIFillPage = function() {
      bslib::page_fill(
        getUIContent(),
        # padding = 10,
        title = getPackageDescription()$Title,
        theme = bslib::bs_theme(
          # "font-size-base" = "0.7rem",
          # "enable-rounded" = FALSE,
          # "line-height-base" = 0.7,
          # "line-height-sm" = 0.7,
          # "line-height-lg" = 0.7,
          spacer = '0.2rem',
          # "bs-gutter-x" = 0,
          version = 5L
        )
      )
    },
    getUIContent = function() {
      list(
        tags$style(
          type = 'text/css',
          '.half-fill { width: 50%; height: 100%; }',
          '#one { float: left; background-color: #ddddff; }',
          '#two { float: right; background-color: #ccffcc; }'
        ),
        shiny::div(
          id = 'one', class = 'half-fill',
          'Left half'
        ),
        shiny::div(
          id = 'two', class = 'half-fill',
          'Right half'
        )
      )
    }
  )
)

AppShiny <- R6::R6Class(
  classname = 'AppShiny',
  inherit = AppShinyWithTranslation,
  portable = FALSE,
  active = list(
    cache = function() {
      getDirAppUsers(user$email, private$.project_active)
    },
    profile = function() {
      private$.profile
    },
    profiler = function() {
      private$.profiler
    },
    ui = function() {
      private$getUI()
    }
  ),
  public = list(

    #' @description
    #' Initialize Application class for shiny app
    #'
    #' @return A new Application object
    initialize = function(about = FALSE,
                          reactlog = FALSE,
                          launch_browser = TRUE,
                          profiler = FALSE,
                          profile = FALSE,
                          project = 'default',
                          ...) {
      super$initialize(...)

      private$.about <- About$new(application = self)

      private$.profile <- profile
      private$.profiler <- profiler

      options(shiny.about = about)
    }
  ),
  private = list(
    .about = NULL,
    .profile = NULL,
    .profiler = NULL,
    getTitle = function() {
      getPackageDescription()$Title
    },
    getUI = function(request) {
      shiny::tagList(
        getUIHeader(),
        shiny::uiOutput('ui'),
        getUIFooter()
      )
    },
    getUIHeader = function() {
      shiny::tagList(
        super$getUIHeader(),
        useEvents(),
        useKeys(),

        shinyFeedback::useShinyFeedback(),

        # rintrojs::introjsUI(),

        # bug with introjs (sub replacement utf 8)
        shiny::tags$head(
          shiny::singleton(
            shiny::tagList(
              shiny::HTML('
                <script src="https://cdn.jsdelivr.net/npm/intro.js@3.2.1/minified/intro.min.js"></script>
                <script src="https://html2canvas.hertzen.com/dist/html2canvas.js"></script>
                <link  href="https://cdn.jsdelivr.net/npm/intro.js@3.2.1/minified/introjs.min.css" rel="stylesheet"/>'),
              shiny::includeScript(system.file(fs::path('javascript', 'rintro.js'), package = 'rintrojs'))
            )
          )
        ),
        htmltools::includeCSS(path = fs::path(getDirAssets(), fs::path('css', 'handsontable.css'))),
        htmltools::includeCSS(path = fs::path(getDirAssets(), fs::path('css', 'leaflet.css'))),
        htmltools::includeCSS(path = fs::path(getDirAssets(), fs::path('css', 'layers.css'))),
        htmltools::includeCSS(path = fs::path(getDirAssets(), fs::path('css', 'shiny.css'))),
        htmltools::includeCSS(path = fs::path(getDirAssets(), fs::path('css', 'shiny-custom.css'))),

        epi.icons::html_dependency_mdi(),
        useGoogle()
      )
    },
    getUIFooter = function() {
      shiny::tagList(
        if (profiler) {
          profvis::profvis_ui('profiler')
        },
        super$getUIFooter()
      )
    },
    getUIMenu = function() {
    },
    getUIMenuSecond = function() {
    },
    getUIOutput = function() {
      shiny::tagList(
        shiny::absolutePanel(
          top = 47L, right = 5L, style = 'z-index: 10000',
          # divInline(pickerInputEx(inputId = 'opt_project_current', label = NULL, choices = NULL, size = 'sm', width = 100)),
          divInline(getUILanguage()),
          if (profile) {
            divInline(shiny::actionButton(inputId = 'act_profvis_stop', label = NULL, icon = icon('stop'), class = 'btn-xs'))
          },
          divInline(shiny::actionButton(inputId = 'act_help', label = NULL, icon = icon('help'), class = 'btn-xs')),
          if (logged) {
            divInline(shiny::actionButton(inputId = 'act_logout', label = NULL, icon = icon('logout'), class = 'btn-xs'))
          }
        ),
        shiny::navbarPage(
          title = getTitle(), id = 'navbar', collapsible = TRUE, position = 'fixed-top',
          # theme = bslib::bs_theme(version = 4),
          getUIMenu(),
          getUIMenuSecond(),
          shiny::navbarMenu(
            title = '', icon = icon('help-circle-outline'),
            shiny::tabPanel(shiny::tags$a('', href = private$getURLSite(page = 'articles/learn.html'), target = '_blank', list(icon('help-circle-outline'), .('Help')))),
            shiny::tabPanel(shiny::tags$a('', href = private$getURLSite(), target = '_blank', list(icon('help-circle-outline', color = 'white'), .('Reference')))),
            '----',
            shiny::tabPanel(shiny::tags$a('', href = getPackageDescription()$BugReports, target = '_blank', list(icon('github'), .('Report issue')))),
            '----',
            private$.about$getUI()
          )
        )
      )
    },
    #' Get the URL site
    #'
    #' @param page a character parameter for selecting the target page to get the site of
    #'
    #' @return a URL (character)
    #'
    getURLSite = function(page = 'index.html') {
      if (session$clientData$url_hostname == '127.0.0.1') {
        # app is offline
        if (fs::dir_exists('docs')) {
          # offline local development mode
          uri <- sprintf(
            '%s//127.0.0.1:%s/docs%s/%s',
            session$clientData$url_protocol,
            session$clientData$url_port, ifelse(isDevPackage(), '/dev', ''), page
          )
        } else {
          # offline capsule mode
          uri <- sprintf(
            '%s//127.0.0.1:%s/site/%s',
            session$clientData$url_protocol,
            session$clientData$url_port, page
          )
        }
      } else {
        # app is online
        uri <- sprintf('https://apps.msf.net/%s/site%s/%s', tolower(getPackageDescription()$Title), ifelse(isDevPackage(), '-dev', ''), page)
      }
      uri
    },
    getServerInternal = function(input, output, session) {

      rintrojs::hintjs(session,
        options = list(hintButtonLabel = 'Hope this hint was helpful'),
        events  = list(onhintclose = I('alert("Wasn\'t that hint helpful")'))
      )

      # Binding
      # .................................................

      private$.about$bind()

      if (profiler) {
        shiny::callModule(module = profvis::profvis_server, id = 'profiler', dir = getDirApp())
      }

      # Shiny events
      # .................................................

      shiny::observeEvent(input$act_logout, ignoreInit = TRUE, {
        logout()
      })

      shiny::observeEvent(input$act_help, ignoreInit = TRUE, {
        logout()
      })

      shiny::observeEvent(input$navbar, {
      })

      shiny::observeEvent(input$act_new, {
        dlg <- ModalDialogTabProject$new(id = 'new', add = TRUE, edit = FALSE, parent = self)
        dlg$bind()
        dlg$show()
      })

      shiny::observeEvent(input$act_save_as, {
        dlg <- ModalDialogTabProject$new(id = 'save', add = FALSE, edit = FALSE, parent = self)
        dlg$bind()
        dlg$show()
      })

      shiny::observeEvent(input$act_edit, {
        dlg <- ModalDialogTabProject$new(id = 'edit', add = FALSE, edit = TRUE, parent = self)
        dlg$bind()
        dlg$show()
      })

      output$ui <- shiny::renderUI({
        getUIOutput()
      })

      if (isEdge(session)) {
        shiny::showModal(shiny::modalDialog(title = 'Warning', 'Sorry! Please open this application in Safari, Mozilla Firefox, or Google Chrome. Microsoft Edge does not work well.', footer = NULL))
      }

      if (profile) {
        utils::Rprof(profvis_file, interval = 0.001, line.profiling = TRUE, gc.profiling = TRUE, memory.profiling = TRUE)
      }
    },
    onStart = function() {
      super$onStart()

      .globals$session <- list(tokens = NULL)
      .globals$vars <- shiny::reactiveValues(db_updated = NULL)
    }
  )
)

ModShiny <- R6::R6Class(
  classname = 'ModShiny',
  inherit = Base,
  portable = FALSE,
  active = list(
    id = function() {
      private$.id
    }
  ),
  public = list(
    initialize = function(id = 'mod', parent = NULL) {
      super$initialize(parent = parent)

      private$.id <- id
    },
    bind = function() {
      shiny::callModule(module = private$getServer, id = private$.id)
    },
    getUI = function(ns = shiny::NS(NULL)) {
      self$ns_parent <- ns
      self$ns <- shiny::NS(ns(id))
    },
    ns = NULL,
    ns_parent = NULL
  ),
  private = list(
    .id = NULL,
    getServer = function(input, output, session) {
      console_out <- private$console_out
    },
    console_out = function(msg, ...) {
      console.out(msg = sprintf('%s: %s', class(self)[1L], msg), ...)
    }
  )
)

ModKey <- R6::R6Class(
  classname = 'ModKey',
  inherit = ModShiny,
  portable = FALSE,
  active = list(
    key = function(value) {
      if (missing(value)) {
        private$.key_trigger$depend()
      } else {
        private$.key <- value
        private$.key_trigger$trigger()
      }
      private$.key
    }
  ),
  public = list(
    initialize = function(keys) {
      private$.key <- ''
      private$.key_trigger <- reactiveTrigger()
      private$.keys <- keys
    },
    getUI = function(ns = shiny::NS(NULL)) {
      super$getUI(ns = ns)
      shiny::tagList(
        keys::useKeys(),
        keys::keysInput(inputId = self$ns('key'), keys = private$.keys)
      )
    }
  ),
  private = list(
    .key = NULL,
    .key_trigger = NULL,
    .keys = NULL,
    getServer = function(input, output, session) {
      super$getServer(input, output, session)

      shiny::observeEvent(input$key, {
        self$key <- input$key
      })
    }
  )
)

ModDatabase <- R6::R6Class(
  classname = 'ModDatabase',
  inherit = ModShiny,
  portable = FALSE,
  active = list(
    pool = function() {
      private$.pool
    },
    pool_changed = function() {
      private$.pool_changed
    }
  ),
  public = list(
    initialize = function(dbname) {
      private$.pool <- pool::dbPool(drv = RSQLite::SQLite(), dbname = dbname)
      private$.pool_changed <- shiny::reactiveVal(0L)
    },
    finalize = function() {
      pool::poolClose(private$.pool)
    },
    dbAppendTable = function(name, value, ..., row.names = NULL) {
      pool::dbAppendTable(conn = self$pool, name = name, value = value, ..., row.names = row.names)
    },
    dbCreateTable = function(name, fields, ..., row.names = NULL, temporary = FALSE) {
      pool::dbCreateTable(conn = self$pool, name = name, fields = fields, ..., row.names = row.names, temporary = temporary)
    },
    dbExecute = function(sql) {
      out <- pool::dbExecute(self$pool, pool::sqlInterpolate(DBI::ANSI(), sql))
      invalidate(pool_changed)
      out
    },
    dbExistsTable = function(name) {
      pool::dbExistsTable(self$pool, name = name)
    },
    dbGetQuery = function(sql) {
      pool::dbGetQuery(self$pool, pool::sqlInterpolate(DBI::ANSI(), sql))
    },
    dbWriteTable = function(name, value, ...) {
      pool::dbWriteTable(self$pool, name = name, value = value, ...)
    }
  ),
  private = list(
    .pool = NULL,
    .pool_changed = NULL
  )
)

ModalDialog <- R6::R6Class(
  classname = 'ModalDialog',
  inherit = ModShiny,
  portable = FALSE,
  active = list(
    out = function(value) {
      if (!missing(value)) {
        private$.out <- value
      }
      private$.out
    },
    ok = function() {
      private$.ok_trigger$depend()
      TRUE
    }
  ),
  public = list(
    initialize = function(id, lbl_title = '', lbl_ok = .('OK'), lbl_cancel = .('Cancel'), parent = NULL) {
      super$initialize(id = id, parent = parent)
      private$.lbl_title <- lbl_title
      private$.lbl_cancel <- lbl_cancel
      private$.lbl_ok <- lbl_ok
      private$.out <- list(ok = FALSE)
      private$.ok_trigger <- reactiveTrigger()
    },
    show = function(ns = shiny::NS(NULL)) {
      shiny::showModal(getUI(ns = ns))
    },
    getUI = function(ns = shiny::NS(NULL)) {
      super$getUI(ns = ns)
      fillModalDialog(
        title = private$.lbl_title,
        getUIDialog(),
        footer = getUIFooter(),
        size = 'm'
      )
    },
    getUIDialog = function() {
      shiny::wellPanel()
    },
    getUIFooter = function() {
      shiny::tagList(
        button(inputId = ns('act_cancel'), label = private$.lbl_cancel),
        shinyjs::disabled(
          buttonLoading(inputId = ns('act_ok'), label = private$.lbl_ok, loadingLabel = 'OK', semantic = 'primary')
        )
      )
    }
  ),
  private = list(
    .lbl_title = NULL,
    .lbl_cancel = NULL,
    .lbl_ok = NULL,
    .out = NULL,
    .ok_trigger = NULL,
    getServer = function(input, output, session, vars) {
      super$getServer(input, output, session)

      shiny::observeEvent(input$act_cancel, ignoreInit = TRUE, {
        shiny::removeModal()
      })

      shiny::observeEvent(input$act_ok, ignoreInit = TRUE, {
        private$.ok_trigger$trigger()
        shiny::removeModal()
      })
    }
  )
)

ModalDialogProgress <- R6::R6Class(
  classname = 'ModalDialogProgress',
  inherit = ModalDialog,
  portable = FALSE,
  public = list(
    getUIDialog = function() {
      shinyWidgets::progressBar(id = self$ns('progress'), value = 0L)
    },
    progress = function(value) {
      shinyWidgets::updateProgressBar(
        id = 'progress',
        value = value,
        total = 100L,
        title = paste('Process', trunc(value / 10L))
      )
    }
  )
)

ModalDialogTab <- R6::R6Class(
  classname = 'ModalDialogTab',
  inherit = ModalDialog,
  portable = FALSE,
  public = list(
    getUIDialog = function() {
      do.call(
        fillTabset,
        append(
          list(
            id = self$ns('tabs')
          ),
          self$getUITabs()
        )
      )
    },
    getUITabs = function() {
      list()
    }
  )
)
