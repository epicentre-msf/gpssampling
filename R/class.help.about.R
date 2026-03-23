About <- R6::R6Class(
  classname = 'About',
  inherit = GpsSamplerModule,
  portable = FALSE,
  public = list(
    initialize = function(application) {
      super$initialize(id = 'about', parent = application)
    },
    getUI = function(ns = shiny::NS('about')) {
      super$getUI(ns = ns)

      fillPanel(
        title = .('About'),
        ns('pan_about'),
        icon = icon('information-outline'),
        shiny::fluidPage(
          shiny::fluidRow(
            shiny::column(1L),
            shiny::column(
              10L,
              htmltools::h1(.('About...')),
              htmltools::p(
                htmltools::strong(.('Version:')),
                sprintf(
                  ' %s (%s)',
                  getPackageVersion(),
                  as.Date(getPackageDescription()$Packaged)
                ),
                style = 'font-size: 18px'
              ),
              shinyWidgets::actionGroupButtons(
                inputIds = c(
                  ns('act_shutdown'),
                  ns('act_refresh')
                ),
                labels = list(
                  icon('power-off', color = 'Tomato'),
                  icon('refresh')
                )
              )
              # if (!getOption('production')) {
              #   shiny::actionButton(inputId = ns('act_explore_cache'), label = NULL, icon = icon('database-search-outline'))
              # }
            ),
            shiny::column(1L)
          ),
          shiny::fluidRow(
            shiny::column(1L),
            shiny::column(
              10L,
              shiny::br(),
              fillTabset(
                id = self$ns('tbs_steps'),
                padding = 0L,
                background_color = 'white',
                fillTabPanel(
                  title = .('About'),
                  height = 288L,
                  overflow = TRUE
                ),
                fillTabPanel(
                  title = .('Development'),
                  height = 288L,
                  overflow = TRUE,
                  shiny::br(),
                  htmltools::p(.(
                    'This application is based on the Shiny package and can be run locally or on a server. It is a fork of GeoSampler, originally developed by Epicentre (MSF).'
                  )),
                  htmltools::h5(.('License')),
                  htmltools::p(
                    'This application is released under the ',
                    a(
                      href = 'http://www.wtfpl.net/',
                      target = '_blank',
                      'WTFPL'
                    ),
                    ' license.'
                  ),
                  htmltools::h5(.('Source code')),
                  htmltools::p(
                    'Source code and issue tracker: ',
                    a(
                      href = 'https://github.com/yves-amevoin/gpssampling',
                      target = '_blank',
                      'github.com/yves-amevoin/gpssampling'
                    )
                  )
                ),
                fillTabPanel(
                  title = .('System'),
                  height = 288L,
                  overflow = TRUE,
                  id = 'system',
                  shiny::verbatimTextOutput(ns('session_info'))
                ),
                fillTabPanel(
                  title = .('Memory'),
                  height = 288L,
                  overflow = TRUE,
                  tableOutput(ns('memory'))
                )
              )
            ),
            shiny::column(1L)
          )
        )
      )
    }
  ),
  private = list(
    getServer = function(input, output, session) {
      super$getServer(input, output, session)

      waiter <- waiter::Waiter$new(
        id = 'system',
        color = waiter::transparent(0.5),
        html = waiter::spin_loaders(7L, color = '#ffffff')
      )

      output$session_info <- shiny::renderPrint({
        waiter$show()
        on.exit({
          waiter$hide()
        })
        if (requireNamespace("sessioninfo", quietly = TRUE)) {
          sessioninfo::session_info()
        } else {
          utils::sessionInfo()
        }
      })

      shiny::observeEvent(input$act_shutdown, {
        application$shutdown()
      })

      shiny::observeEvent(input$act_explore_cache, {
        exploreDirCache()
      })

      output$memory <- shiny::renderTable({
        objs_env <- environment()
        objs <- ls(objs_env)
        data.frame(
          object = objs,
          size = unlist(lapply(objs, function(x) {
            utils::object.size(get(x, envir = objs_env, inherits = FALSE))
          }))
        )
      })
    }
  )
)
