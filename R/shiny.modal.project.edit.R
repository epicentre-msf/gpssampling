ModalDialogTabProject <- R6::R6Class(
  'ModalDialogTabProject',
  inherit = ModalDialogTab,
  portable = FALSE,
  public = list(
    initialize = function(id, add = FALSE, edit = FALSE, parent = NULL) {
      super$initialize(id,
        lbl_title = dplyr::case_when(
          add ~ ..('Add a new project'),
          edit ~ ..('Edit project'),
          .default = ..('Save project')
        ),
        parent = parent
      )
      private$.add <- add
      private$.edit <- edit
    },
    getUITabs = function() {
      txt_name <- shiny::textInput(
        inputId = ns('txt_name'),
        label = labelMandatory(.('Name of the project:')),
        value = ifelse(private$.add, '', data$project_name),
        width = '100%'
      )
      if (private$.edit) {
        txt_name <- shinyjs::disabled(txt_name)
      }
      list(
        fillTabPanel(
          value = 'tab_info', title = .('General'), height = NULL,
          shiny::fillPage(
            shiny::fillRow(
              flex = 1L, height = 350L, width = 400L,
              shiny::fluidPage(
                shiny::fluidRow(
                  shiny::column(
                    6L,
                    pickerInputEx(
                      inputId = ns('opt_status'),
                      label = labelMandatory(.('Status:')),
                      choices = data$getChoicesStatus(),
                      selected = ifelse(private$.add, 'D', data$project_status),
                      width = '100%'
                    )
                  ),
                  shiny::column(
                    6L,
                    pickerInputEx(
                      inputId = ns('opt_priority'),
                      label = labelMandatory(.('Priority:')),
                      choices = data$getChoicesPriority(),
                      selected = ifelse(private$.add, 'U', data$project_priority),
                      width = '100%'
                    )
                  )
                ),
                shiny::fluidRow(
                  shiny::column(
                    12L,
                    txt_name,
                    shiny::textAreaInput(
                      inputId = ns('txt_description'),
                      label = .('Description:'),
                      resize = 'none',
                      rows = 5L,
                      value = ifelse(private$.add, '', data$project_description),
                      width = '100%'
                    )
                  )
                )
              )
            )
          )
        )
      )
    },
    getUIFooter = function() {
      shiny::tagList(
        if (private$.edit) {
          shiny::div(
            style = 'float:left',
            button(inputId = ns('act_delete'), label = .('Delete'))
          )
        },
        button(inputId = ns('act_cancel'), label = private$.lbl_cancel),
        shinyjs::disabled(
          if (private$.add) {
            button(inputId = ns('act_add'), label = .('Add'), semantic = 'primary')
          } else {
            if (private$.edit) {
              button(inputId = ns('act_edit'), label = .('Submit'), semantic = 'primary')
            } else {
              button(inputId = ns('act_save_as'), label = .('Save'), semantic = 'primary')
            }
          }
        )
      )
    }
  ),
  private = list(
    .add = NULL,
    .edit = NULL,
    getServer = function(input, output, session, vars) {
      super$getServer(input, output, session, vars)

      shiny::observeEvent(input$act_add, {
        if (input$txt_name == '') {
          shinyFeedback::showFeedbackWarning(inputId = 'txt_name', text = ..('The name is required.'))
          return(0L)
        }

        if (input$txt_name %in% data$projects) {
          shinyFeedback::showFeedbackWarning(inputId = 'txt_name', text = ..('The name is already used.'))
          return(0L)
        }

        data$addProject(
          status = input$opt_status,
          priority = input$opt_priority,
          name = input$txt_name,
          description = input$txt_description
        )

        shiny::removeModal()
      })

      shiny::observeEvent(input$act_save_as, {
        if (input$txt_name == '') {
          shinyFeedback::showFeedbackWarning(inputId = 'txt_name', text = ..('The name is required.'))
          return(0L)
        }

        if (input$txt_name %in% data$projects) {
          shinyFeedback::showFeedbackWarning(inputId = 'txt_name', text = ..('The name is already used.'))
          return(0L)
        }

        data$projectClone(
          status = input$opt_status,
          priority = input$opt_priority,
          name = input$txt_name,
          description = input$txt_description
        )

        shiny::removeModal()
      })

      shiny::observeEvent(input$act_edit, {
        data$projectEdit(
          status = input$opt_status,
          priority = input$opt_priority,
          description = input$txt_description
        )

        shiny::removeModal()
      })

      shiny::observeEvent(input$act_delete, {
        data$projectDelete()

        shiny::removeModal()
      })

      shiny::observe({
        req(input$opt_status)
        req(input$opt_priority)
        req(input$txt_name)

        modified <-
          (input$opt_status != data$project_status) |
            (input$opt_priority != data$project_priority) |
            (input$txt_name != data$project_name & input$txt_name != '') |
            (input$txt_description != data$project_description)

        if (modified) {
          shinyjs::enable(id = 'act_add')
          shinyjs::enable(id = 'act_edit')
          shinyjs::enable(id = 'act_save_as')
        } else {
          shinyjs::disable(id = 'act_add')
          shinyjs::disable(id = 'act_edit')
          shinyjs::disable(id = 'act_save_as')
        }

        # if (state$can_rollback | state$can_clear) {
        #   shinyjs::enable(id = 'act_clear_btn')
        # } else {
        #   shinyjs::disable(id = 'act_clear_btn')
        # }

        # if (state$is_editing) {
        #   shinyjs::disable(id = 'act_extent_side')
        #   shinyjs::disable(id = 'act_extent_btn')
        # } else {
        #   shinyjs::enable(id = 'act_extent_side')
        #   shinyjs::enable(id = 'act_extent_btn')
        # }

        # if (state$can_commit | state$can_rollback) {

        # vars$info_text <- sprintf(
        #   .('The extent layer is modified. Confirm and save (%s) the extent or rollback (%s) the changes.'),
        #   as.character(icon('check')),
        #   as.character(icon('delete-outline'))
        # )

        # } else if (state$is_editing) {

        #   vars$info_text <- .('Click on the map to draw a rectangle representing the mapping area in the map frame.')

        # } else {

        # if (project()$getCount() == 0) {

        #   vars$info_text <- sprintf(
        #     .('Extent is the rectangular aera of interest covered by your project. Begin to find the location of your project (%s) on the map and draw the area (%s).'),
        #     as.character(icon('magnify')),
        #     as.character(icon('shape-square-plus'))
        #   )

        # } else {

        # vars$info_text <- sprintf(
        #   .('Extent is defined. Go to the next step or delete (%s) the extent of your current project to redefine it.'),
        #   as.character(icon('delete-outline'))
        # )

        # }

        # }
      })
    }
  )
)
