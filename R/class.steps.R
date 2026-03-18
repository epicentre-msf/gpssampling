Steps <- R6::R6Class(
  classname = 'Steps',
  inherit = ApplicationModule,
  portable = FALSE,
  active = list(
    step_delimit = function() {
      private$.step_delimit
    },
    step_identify = function() {
      private$.step_identify
    },
    step_sample = function() {
      private$.step_sample
    },
    step_result = function() {
      private$.step_result
    }
  ),
  public = list(
    initialize = function(id = 'steps', parent = NULL) {
      super$initialize(id = id, parent = parent)

      private$.step_delimit <- StepDelimit$new(steps = self, index = 1L)
      private$.step_identify <- StepIdentify$new(steps = self, index = 2L)
      private$.step_sample <- StepSample$new(steps = self, index = 3L)
      private$.step_result <- StepResult$new(steps = self, index = 4L)
    },
    getUI = function(ns = shiny::NS(NULL)) {

      super$getUI(ns = ns)

      # if (getOption('identify')) {

      #   fillPanel(
      #     title = '', id = ns('pan_steps'), icon = icon('home-outline'),
      #     fillTabset(
      #       id = ns('tbs_steps'),
      #       tabUIStepIdentify(ns = ns)
      #     )
      #   )

      # } else {

      fillPanel(
        title = '', self$ns('pan_steps'), icon = icon('home-outline'),
        fillTabset(
          id = self$ns('tbs_steps'),
          private$.step_delimit$getUI(ns = self$ns),
          private$.step_identify$getUI(ns = self$ns),
          private$.step_sample$getUI(ns = self$ns),
          private$.step_result$getUI(ns = self$ns)
        )
      )
      # }
    }
  ),
  private = list(
    .step_delimit = NULL,
    .step_identify = NULL,
    .step_sample = NULL,
    .step_result = NULL,
    getServer = function(input, output, session) {

      super$getServer(input, output, session)

      shiny::observe({
        self$data$step_delimit$state
        self$data$step_identify$state
        self$data$step_result$state

        if (self$data$step_delimit$state$modified) {
          disableTabPanel(value = self$data$step_identify$ns('tab'))
          disableTabPanel(value = self$data$step_sample$ns('tab'))
          disableTabPanel(value = self$data$step_result$ns('tab'))
        } else {
          if (self$data$polygons_selected_count == 0L) {
            disableTabPanel(value = self$data$step_identify$ns('tab'))
            disableTabPanel(value = self$data$step_sample$ns('tab'))
            disableTabPanel(value = self$data$step_result$ns('tab'))
          } else {
            enableTabPanel(value = self$data$step_identify$ns('tab'))
            if (self$data$step_identify$state$modified) {
              disableTabPanel(value = self$data$step_sample$ns('tab'))
              disableTabPanel(value = self$data$step_result$ns('tab'))
            } else {
              if ((sum(self$data$polygons_selected$roofs_count, na.rm = TRUE) > 0L) |
                (sum(self$data$polygons_selected$cells_count, na.rm = TRUE) > 0L) |
                (self$data$project_method %in% c('SP_TSQ', 'SP_QDR'))) {
                enableTabPanel(value = self$data$step_sample$ns('tab'))
                if (!self$data$step_sample$state$modified & sum(sapply(self$data$polygons_selected$samples_sf, nrow)) > 0L) {
                  enableTabPanel(value = self$data$step_result$ns('tab'))
                } else {
                  disableTabPanel(value = self$data$step_result$ns('tab'))
                }
              } else {
                disableTabPanel(value = self$data$step_sample$ns('tab'))
                disableTabPanel(value = self$data$step_result$ns('tab'))
              }
            }
          }
        }

      })

      shiny::observeEvent(input$tbs_steps, {
        data$step <- stringr::str_match(self$ns(input$tbs_steps), '(step_.*)-tab')[1L, 2L]
      })

      shiny::observeEvent(data$guide_polygon_changed, ignoreNULL = FALSE, ignoreInit = TRUE, {
        shiny::updateTabsetPanel(session, 'pan_steps', selected = paste0('panel', input$controller))
      })

      # if (getOption('identify')) {
      # private$.step_identify$bind()
      # } else {
      private$.step_delimit$bind()
      private$.step_identify$bind()
      private$.step_sample$bind()
      private$.step_result$bind()
      # }
    }
  )
)
