# styler: block

modalAbout <- function(id = 'mod_dlg_about', vars, project) {

  shiny::moduleServer(
    id = id,
    module = function(input, output, session) {
      ns <- session$ns

      vars <- shiny::reactiveValues(
        authenticated = TRUE,
        num_fails = 0L
      )

      shiny::observeEvent(input$opt_projects, ignoreNULL = FALSE, {
      })

      shiny::observeEvent(input[['keyPressed']], {
        shinyjs::click('act_ok')
      })

      # --------------------------------------------------------------------------------------
      # Authentification

      # credentials <- readRDS(getCredentialFilename())

      num_fails_to_lockout <- 3L

      # authenticate user by:
      #   1. checking whether their user name and token are in the credentials
      #       data frame and on the same row (credentials are valid)
      #   2. if credentials are valid, retrieve their lockout status from the data frame
      #   3. if user has failed login too many times and is not currently locked out,
      #       change locked out status to TRUE in credentials DF and save DF to file
      #   4. if user is not authenticated, determine whether the user name or the token
      #       is bad (username precedent over token) or he is locked out. set status value for
      #       error message code below
      # shiny::observeEvent(input$act_ok,
      #   {
      #     if (vars$authenticated) {
      #       shiny::removeModal()
      #     } else {
      #       row_token <- which(credentials$token == digest::digest(input$txt_token)) # digest() makes md5 hash of token

      #       # if user name row and token name row are same, credentials are valid
      #       #   and retrieve locked out status
      #       if (length(row_token) >= 1) {
      #         vars$valid_credentials <- TRUE
      #         vars$user_locked_out <- credentials$locked_out[row_token]
      #         project$user <<- credentials$user[row_token]
      #         project$token <<- toupper(credentials$token[row_token])
      #       } else {
      #         vars$valid_credentials <- FALSE
      #         vars$user_locked_out <- FALSE
      #       }

      #       # if user is not currently locked out but has now failed login too many times:
      #       #   1. set current lockout status to TRUE
      #       #   2. if username is present in credentials DF, set locked out status in
      #       #     credentials DF to TRUE and save DF
      #       if (vars$num_fails >= num_fails_to_lockout & vars$user_locked_out == FALSE) {
      #         vars$user_locked_out <- TRUE

      #         if (length(row_token) == 1) {
      #           credentials$locked_out[row_token] <- TRUE

      #           saveRDS(credentials, getCredentialFilename())
      #         }
      #       }

      #       # if a user has valid credentials and is not locked out, he is authenticated
      #       if (vars$valid_credentials == TRUE & vars$user_locked_out == FALSE) {
      #         vars$authenticated <- TRUE
      #       } else {
      #         vars$authenticated <- FALSE
      #       }

      #       # if user is not authenticated, set login status variable for error messages below
      #       if (vars$authenticated == FALSE) {
      #         vars$num_fails <- vars$num_fails + 1
      #         if (vars$user_locked_out == TRUE) {
      #           shinyFeedback::feedbackDanger(
      #             inputId = session$ns('txt_token'),
      #             condition = TRUE,
      #             text = 'Your account is locked because of too many failed login attempts. Contact administrator.'
      #           )
      #         } else if (input$token == '' || length(row_token) == 0) {
      #           vars$status <- 'bad_token'
      #           shinyFeedback::feedbackWarning(
      #             inputId = session$ns('txt_token'),
      #             condition = TRUE,
      #             text = sprintf('Incorrect token!. %s attempts left', 3 - vars$num_fails)
      #           )
      #         }
      #       } else {
      #         shiny::updateActionButton(session, inputId = 'act_ok', label = 'Start...', icon = NULL)
      #         shinyjs::show(id = 'about')
      #       }
      #     }
      #   },
      #   ignoreInit = TRUE
      # )

      shiny::observeEvent(input$act_ok, {
        shiny::removeModal()
        project$setStep('about', TRUE)
      })

      # --------------------------------------------------------------------------------------
      # Links

      # shiny::observeEvent(input$link_guide, {
      #   shiny::removeModal()
      #   project$help <- !project$help
      # })

      # shiny::observe({
      #   lst_week <- getLastYearWeek(datas())
      #   lst_year <- getLastYear(datas())
      #   if (shiny::getShinyOption('authentification')) {
      #     shiny::updateActionButton(session, inputId = 'act_ok', label = 'Login', icon = NULL)
      #     shinyjs::show(id = 'authentification')
      #     vars$authenticated <- FALSE
      #   } else {
      #     shinyjs::show(id = 'about')
      #   }
      #   # if (fs::file_exists(reportWeeklyAllFilename(datas()))) {
      #   #   shinyjs::enable(id = 'act_download_reports')
      #   # }
      #   if (fs::file_exists(getFileCache(sprintf('data-%s', lst_year), ext = 'xlsx'))) {
      #     shinyjs::enable(id = 'act_download_data')
      #   }
      #   output$data_updated <- shiny::renderText(sprintf('<p><b>Updated: </b>%s-W%s (%s)</p>', lst_year, lst_week, aweek::get_date(lst_week, lst_year)))
      # })

      # --------------------------------------------------------------------------------------
      # Upload / Download Data

      # output$act_download_data <- shiny::downloadHandler(
      #   filename = function() {
      #     lst_week <- getLastYearWeek(datas())
      #     lst_year <- getLastYear(datas())
      #     sprintf('IDS_%d_W%0.2d.xlsx', lst_year, lst_week)
      #   },
      #   content = function(file) {
      #     lst_year <- getLastYear(datas())
      #     fs::file_copy(path = getFileCache(sprintf('data-%s', lst_year), ext = 'xlsx'), new_path = file)
      #   }
      # )

      # output$act_download_reports <- shiny::downloadHandler(
      #   filename = function() {
      #     fs::path_file(reportWeeklyAllFilename(datas()))
      #   },
      #   content = function(file) {
      #     fs::file_copy(path = reportWeeklyAllFilename(datas()), new_path = file)
      #   }
      # )
    }
  )
}

modalAboutUI <- function(id = 'mod_dlg_about', project, failed = FALSE) {

  ns <- shiny::NS(id)

  fillModalDialog(
    title = getPackageDescription()$Title, size = 'm',
    fillTabset(
      id = ns('tabs_about'),
      fillTabPanel(
        value = 'tab_projects', title = .('Projects'), height = NULL, icon = icon('information-outline', size = NULL),
        shiny::fillPage(
          shiny::fillCol(
            width = 400L,
            flex = c(NA, NA), height = 350L,
            # p(getPackageDescription()$Title, class = 'about-title'),
            # p(getPackageDescription()$Description, style = 'font-size: 14px', class = 'intro'),
            # shinyjs::hidden(
            # shiny::div(
            # id = ns('authentification'),
            # shiny::textInput(
            #   inputId = ns('txt_token'),
            #   label = .('Authorization Token (Guest: 0000):'),
            #   placeholder = 'CE6ZUY8ZP1B10K5OC41LFBQU',
            #   width = '100%'
            # )
            # )
            # ),
            # shinyjs::hidden(
            # shiny::div(
            # id = ns('about'),
            # shiny::div(class = 'intro-divider'),
            wellPanelInfo(
              title = .('Get Started'),
              shiny::tags$ul(
                shiny::tags$li(actionLink(inputId = ns('link_guide'), shiny::HTML(.('<b>Start the step-by-step guide_polygon and feature introduction (Recommended).</b>')))),
                shiny::tags$li(actionLink(inputId = ns('link_1'), .('WORKING IN PROGRESS...'))),
                shiny::tags$li(actionLink(inputId = ns('link_1'), .('WORKING IN PROGRESS...')))
              )
            ),
            wellPanelInfo(
              title = .('Project'), color = '##E6E0E0'
              # pickerInputEx(
              #   inputId = ns('opt_projects'),
              #   label = .('Open a active project...'),
              #   choices = getProjects(),
              #   options = list(
              #     `live-search` = TRUE
              #   ),
              #   size = 's',
              #   width = '100%'
              # ),
              # shiny::htmlOutput(ns('data_updated')),
              # divInline(shinyjs::disabled(shiny::downloadButton(outputId = ns('act_download_data'), label = 'Download Last Data', icon = icon('cloud-download'), class = 'btn-sm', style = 'width: 160px'))),
              # divInline(shinyjs::disabled(shiny::downloadButton(outputId = ns('act_download_reports'), label = 'Download Last Reports', icon = shiny::icon('cloud-download'), class = 'btn-sm', style = 'width: 160px'))),
              # divInline(button(inputId = ns('act_upload_data'), label = 'Upload...', icon = icon('cloud-upload'), class = 'btn-primary btn-sm', width = '100%'))
            )

            # p(htmltools::strong('PLEASE NOTE:'), 'Information on how to use the dashboard is available on each pages when you click on the ',
            #   button(inputId = ns('help'), label = NULL, icon = icon('check'), class = 'btn-primary btn-xs', style = 'font-size: 10px; vertical-align: text-bottom;'), 'button.',
            #   style = 'font-size: 14px'
            # )
            # )
            # )
          )
        )
      ),
      fillTabPanel(
        value = 'tab_project_new', title = .('New Project'), height = NULL, icon = icon('plus-circle-outline', size = NULL),
        shiny::fillPage(
          shiny::fillCol(
            width = 400L,
            flex = c(NA, NA, NA), height = 350L,
            textInputEx(
              inputId = ns('txt_title'),
              label = .('Title:'),
              size = 's',
              width = '100%'
            ),
            # helper(
            #   textInputEx(
            #     inputId = ns('txt_description'),
            #     label = .('Description:'),
            #     # size = "s",
            #     width = '100%'
            #   )
            # ),
            textInputEx(
              inputId = ns('txt_imagery'),
              label = .('Imagery:'),
              placeholder = 'http://vps135569.ovh.net/imageries/IOM_UAV_Unchiprang_site_201804252/{z}/{x}/{y}.png',
              size = 's',
              width = '100%'
            ),
            tippy::tippy_this('txt_imagery', 'Tooltip')
          )
        )
      )
      # fillTabPanel(
      #   value = 'tab_settings', title = 'Settings', height = NULL,
      #   shiny::fillPage(
      #     shiny::fillCol(
      #       flex = c(NA, NA, NA, NA), height = 350,
      #       p('Stratification'),
      #       shinyWidgets::switchInput(
      #         inputId = ns('opt_stratify'),
      #         label = 'Stratification',
      #         value = TRUE
      #       ),
      #       shiny::sliderInput(inputId = ns('sli_radius'), label = 'Circle Radius (meter):', min = 5, max = 250, value = 50, step = 5),
      #       shiny::sliderInput(inputId = ns('sli_opacity'), label = 'Circle Opacity:', min = 5, max = 100, value = 25, step = 5),
      #       shiny::checkboxInput(inputId = ns('chk_simplify'), label = 'Simplify polygons for performance:', value = TRUE)
      #     )
      #   )
      # )
    ),
    footer =
      shiny::fluidPage(
        shiny::fluidRow(
          shiny::column(
            12L,
            # shinyjs::disabled(
            htmltools::tagAppendAttributes(
              `data-proxy-click` = ns('act_ok'),
              button(inputId = ns('act_ok'), label = .('Ok'), icon = 'check', semantic = 'primary')
            )
            # )
          )
        )
      )
  )
}
