About <- R6::R6Class(
  classname = 'About',
  inherit = ApplicationModule,
  portable = FALSE,
  public = list(
    initialize = function(application) {
      super$initialize(id = 'about', parent = application)
    },
    getUI = function(ns = shiny::NS('about')) {

      super$getUI(ns = ns)

      fillPanel(
        title = .('About'), ns('pan_about'), icon = icon('information-outline'),
        shiny::fluidPage(
          shiny::fluidRow(
            shiny::column(1L),
            shiny::column(
              10L,
              htmltools::h1(.('About...')),
              htmltools::p(htmltools::strong(.('Version:')), sprintf(' %s (%s)', getPackageVersion(), as.Date(getPackageDescription()$Packaged)), style = 'font-size: 18px'),
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
                id = self$ns('tbs_steps'), padding = 0L, background_color = 'white',
                fillTabPanel(title = .('About'), height = 288L, overflow = TRUE),
                fillTabPanel(
                  title = .('Development'), height = 288L, overflow = TRUE,
                  shiny::br(),
                  htmltools::p(.('The application is based on the Shiny package and can be run locally or on a server. It was developed by Serge Balandine.')),
                  htmltools::h5(.('Google analytics')),
                  htmltools::p(.('This site uses Google Analytics to track user behavior while on the site. These data will be used for educational purposes only.')),
                  htmltools::h5(.('License')),
                  htmltools::p('
                    This dashboard is licensed under the', a(href = 'https://tldrlegal.com/license/gnu-affero-general-public-license-v3-(agpl-3.0)', target = '_blank', 'AGPLv3'), '. The
                    documentation on this site as well as the Dashboard help files are licensed under the creative commons attribution, non-commercial,
                    share-alike license ', a(href = 'https://creativecommons.org/licenses/by-nc-sa/4.0/', target = '_blank', 'CC-NC-SA'), '.'),
                  htmltools::p(.('
                    As a summary, the AGPLv3 license requires, attribution, including copyright and license information in copies of the software, stating changes
                    if the code is modified, and disclosure of all source code. Details are in the COPYING file.')),
                  htmltools::p(.('If you are interested in using this application please email me at '), a(href = 'serge.balandine@epicentre.msg.org', 'serge.balandine@epicentre.msg.org'))

                  # <ul>
                  #   <li>The <a href='https://plot.ly/r'>Plotly R Library</a> was used for the interactive plot.<li>
                  # </ul>

                  # <h4>Sources</h4>

                  # <p style='text-align:justify'>
                  #   Surveillance data is taken from ... More information about the data is available here.
                  # </p>

                  # <h4>Credits</h4>

                  # <ul>
                  #   <li>The <a href='https://plot.ly/r'>Plotly R Library</a> was used for the interactive plot.<li>
                  # </ul>

                  # <h4>Developers</h4>

                  # <p style='text-align:justify'>
                  #   This dashboard was developed using Shiny, an open-source web development framework powered by R (this project is
                  #   not affiliated with The Comprehensive R Archive Network or The R Foundation).
                  # </p>

                  # <div style='clear: left;'><img src='assets/img/s-balandine.jpg' alt='' style='float: left; margin-right:5px' /></div><br>

                  # <p style='text-align:justify'>
                  #   <b>Serge Balandine</b><br/>
                  #   Data Scientist | useR<br/>
                  #   <a href='http://epicentre.msf.org/', target='_blank'>Epicentre</a>
                  # </p>
                  # '
                ),
                fillTabPanel(
                  title = .('System'), height = 288L, overflow = TRUE, id = 'system',
                  shiny::verbatimTextOutput(ns('session_info'))
                ),
                fillTabPanel(
                  title = .('Memory'), height = 288L, overflow = TRUE,
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

      waiter <- waiter::Waiter$new(id = 'system', color = waiter::transparent(0.5), html = waiter::spin_loaders(7L, color = col_spin_waiter))

      output$session_info <- shiny::renderPrint({
        waiter$show()
        on.exit({
          waiter$hide()
        })
        sessioninfo::session_info()
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
