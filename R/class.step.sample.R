StepSample <- R6::R6Class(
  classname = 'StepSample',
  inherit = Step,
  portable = FALSE,
  active = list(
    tab_title = function() {
      .('Sample')
    },
    lbl_cancel = function() {
      ..('Are you sure you want to permanently delete this sample(s)?')
    },
    lbl_cancel_info = function() {
      ..('If <b>yes</b>, all depend work will be deleted <b>permanently</b>. You can\'t undo this action.')
    },
    lbl_ok = function() {
      ..('Are you sure you want to permanently save this sample(s)?')
    },
    lbl_ok_info = function() {
      ..('Saved sample(s) will be overwritten. You can\'t undo this action.')
    }
  ),
  public = list(
    initialize = function(index = 1L, steps = NULL) {
      super$initialize(id = 'step_sample', index = index, steps = steps)

      private$.state$can_generate <- FALSE
      private$status_trigger <- reactiveTrigger()
      private$sample_trigger <- reactiveTrigger()
    },
    save = function() {
      if (is.defined(project)) {
        data$saveToCache(private$polygons, file = 'sample.rds')
      }
    },
    invalidateStatus = function() {
      private$status_trigger$trigger()
    },
    commit = function() {
      data$save(sample = TRUE)
    },
    rollback = function() {
      data$rollback(sample = TRUE)
    },
    clear = function() {
      data$clear(sample = TRUE)
    },
    getStatus = function() {
      private$status_trigger$depend()
      private$status
    },
    invalidatePolygons = function() {
      data$displayPolygons(map, only_selected = TRUE, sample = TRUE, cells = TRUE, status = FALSE)
      super$invalidatePolygons()
    }
  ),
  private = list(
    polygons = NULL,
    polygon_selected = -1L,
    status = list(
      mode = 'select',
      modified = FALSE
    ),
    sample_trigger = NULL,
    status_trigger = NULL,
    getUIMap = function() {
      shiny::tagList(
        super$getUIMap(),
        shiny::absolutePanel(
          bottom = 18L, left = 78L, fixed = TRUE,
          shiny::uiOutput(outputId = self$ns('legend'))
        )
      )
    },
    getUISideBar = function() {
      shinyjs::hidden(
        sidebar_tabs(
          id = ns('sidebar'),
          list(
            icon('menu'),
            icon('download'),
            icon('cog-outline'),
            icon('help-circle-outline')
          ),
          sidebar_pane(
            title = .('STEP 3: SAMPLE'),
            id = ns('pnl_step'),
            icon = icon('menu', size = 's'),
            shiny::br(),
            p(.('Todo...'))
          ),
          sidebar_pane(
            title = .('EXPORT'),
            id = ns('pnl_data'),
            icon = icon('download', size = 's'),
            shiny::br(),
            shiny::h6('Export:'),
            p(.('Export the current sample layer.'))
          ),
          sidebar_pane(
            title = .('SETTINGS'),
            id = ns('pnl_settings'),
            icon = icon('cog-outline', size = 's')
          ),
          sidebar_pane(
            title = .('HELP'),
            id = ns('pnl_help'),
            icon = icon('help-circle-outline', size = 's'),
            shiny::br(),
            shiny::tags$p(class = 'intro', .('Get Started with Step-by-Step Guide'), style = 'font-size: 14px'),
            shiny::tags$ul(
              shiny::tags$li(actionLink(inputId = ns('link_guide_ui'), label = .('Interface')))
            )
          )
        )
      )
    },
    getOutputMap = function(session) {
      map <- super$getOutputMap(session)

      map <- leaflet::addEasyButtonBar(map,
        id = ns('bar_layers'),
        position = 'topleft',
        easyButtonShiny(inputId = ns('act_upload'), icon = icon('layers-plus'), title = ..('Add a sample layer...'))
      )

      map <- leaflet::addEasyButtonBar(map,
        id = ns('bar_sample_1'),
        position = 'topleft',
        easyButtonShiny(inputId = ns('act_generate_modal'), icon = icon('map-marker-multiple-outline'), title = ..('New Sample [SPACE]'))
      )

      map <- leaflet::addEasyButtonBar(map,
        id = ns('bar_sample_2'),
        position = 'topleft',
        easyButtonShiny(inputId = ns('act_sample_generate'), icon = icon('map-marker-radius-outline'), title = ..('New Sample [SPACE]')),
        easyButtonShiny(inputId = ns('act_sample_save'), icon = icon('map-marker-check-outline'), title = ..('Save [ENTER]'))
      )

      map <- pm_attach_dependencies(map,
        targetGroup = 'sample_quadrat',
        drawOptions = leafpm::pmDrawOptions(
          snappable = TRUE,
          snapDistance = 10L,
          snapMiddle = TRUE,
          tooltips = TRUE,
          cursorMarker = TRUE,
          finishOn = NULL,
          allowSelfIntersection = FALSE,
          templineStyle = list(
            color = 'yellow',
            weight = 2L
          ),
          hintlineStyle = list(
            color = 'yellow',
            dashArray = '5,5',
            weight = 2L
          ),
          markerStyle = list(
            draggable = TRUE
          ),
          pathOptions = list(
            color = 'yellow',
            weight = 2L
          )
        )
      )

      # map <- map |>
      #   leaflet::addLegend(
      #     position = 'bottomleft',
      #     colors = c('#ff00ff', '#00ff00', 'yellow', '#ff8800'),
      #     labels = c(
      #       .('Point (Roof)'),
      #       .('Point (TSquare)'),
      #       .('Area (Quadrat)'),
      #       .('Area (StepDelimit)')
      #     ),
      #     opacity = 1,
      #     title = .('Sample')
      #   )

      map
    },
    getServer = function(input, output, session) {
      super$getServer(input, output, session)

      vars <- shiny::reactiveValues(
        sample_quadrat = 0L,
        sample_selected = -1L,
        sample_selected_sf = NULL,
        map_shape_click = FALSE
      )

      # --------------------------------------------------------------------------------------
      # Settings

      # --------------------------------------------------------------------------------------
      # Actions

      shiny::observeEvent(input$act_generate_modal, {
        polygon <- data$polygon_focused

        if (
          data$project_method == 'RS_SMP' &&
            data$polygon_focused$roofs_count < 2L) {
          return()
        }

        shiny::showModal(
          shiny::modalDialog(
            title = sprintf('Settings for %s', df_sampling_method[data$project_method, 'label']),
            size = 's',
            shiny::fillPage(
              shiny::fillCol(
                width = 350L,
                flex = c(NA, NA, NA, NA),
                if (data$project_method != 'RS_SMP') {
                  shiny::div(
                    style = 'height: 105px',
                    shiny::radioButtons(
                      inputId = self$ns('opt_sample_method'),
                      inline = TRUE,
                      label = ..('Method:'),
                      choiceNames = c(..('Random'), ..('Regular'), ..('Stratified')),
                      choiceValues = c('random', 'regular', 'stratified'),
                      selected = polygon$method,
                      width = '100%'
                    )
                  )
                } else {
                  shiny::div()
                },
                if (data$project_method == 'RS_SMP') {
                  roofs_count <- data$polygon_focused$roofs_count

                  shiny::div(
                    style = 'height: 105px',
                    shiny::sliderInput(inputId = self$ns('sli_roofs_count'), label = ..('Count:'), min = 1L, max = roofs_count, value = roofs_count %/% 10L, step = 1L, width = '100%')
                  )
                } else {
                  shiny::div()
                },
                if (data$project_method %in% c('SP_SMP', 'SP_QDR', 'SP_TSQ')) {
                  shiny::div(
                    style = 'height: 105px',
                    shiny::sliderInput(inputId = self$ns('sli_sample_count'), label = ..('Count:'), min = 1L, max = 1000L, value = polygon$count, step = 1L, width = '100%')
                  )
                } else {
                  shiny::div()
                },
                if (data$project_method == 'SP_QDR') {
                  shiny::div(
                    style = 'height: 105px',
                    shiny::sliderInput(inputId = self$ns('sli_sample_size'), label = ..('Size (meter):'), min = 5L, max = 500L, value = polygon$size, step = 1L, width = '100%')
                  )
                } else {
                  shiny::div()
                }
              )
            ),
            footer = shiny::tagList(
              buttonLoading(inputId = self$ns('act_generate'), label = ..('Generate'), style = 'float: left', semantic = 'primary', loadingLabel = ..('Generating...')),
              button(inputId = self$ns('act_generate_ok'), label = ..('Ok'))
            )
          )
        )
      })

      shiny::observeEvent(input$act_generate, {
        shinyFeedback::hideFeedback(inputId = 'sli_roofs_count')
        shinyFeedback::hideFeedback(inputId = 'sli_sample_count')
        shinyFeedback::hideFeedback(inputId = 'sli_sample_size')

        shinyjs::disable(selector = 'div.modal-content')

        on.exit({
          shinyjs::enable(selector = 'div.modal-content')
          shinyFeedback::resetLoadingButton(inputId = 'act_generate')
        })

        if (data$project_method == 'RS_SMP') {
          sample_count <- input$sli_roofs_count

          r <- data$generateSamples(map,
            type = data$project_method,
            count = sample_count
          )
        } else {
          sample_count <- input$sli_sample_count

          r <- data$generateSamples(map,
            type = data$project_method,
            method = input$opt_sample_method,
            count = sample_count,
            size = input$sli_sample_size
          )
        }

        if ((data$project_method == 'SP_QDR') && (any(rowSums(sf::st_intersects(r$quadrat, r$quadrat, sparse = FALSE)) > 1L))) {
          shinyFeedback::showFeedbackWarning(
            inputId = 'sli_sample_size',
            text = ..('Quadrats are too big or intersect.')
          )
        }

        if (r$count != sample_count) {
          shinyFeedback::showFeedbackWarning(
            inputId = 'sli_sample_count',
            text = sprintf(..('%s points are generated.'), r$count)
          )
        }
      })

      shiny::observeEvent(input$act_generate_ok, {
        shiny::removeModal()
      })

      shiny::observeEvent(input$act_sample_generate, {
        data$addSample(map)
      })

      shiny::observeEvent(input$act_sample_save, {
        data$sampleAppend(map)
      })

      shiny::observeEvent(input$sli_sample_radius, ignoreInit = TRUE, {
        data$settings$setValue('sli_sample_radius', input$sli_sample_radius)
      })

      shiny::observeEvent(input$sli_sample_zoom, ignoreInit = TRUE, {
        data$settings$setValue('sli_sample_zoom', input$sli_sample_zoom)
      })


      # --------------------------------------------------------------------------------------
      # Shortcuts

      shiny::observeEvent(application$keypress$key_up, ignoreInit = TRUE, ignoreNULL = TRUE, {
        if (data$step == 'step_sample') {
          key <- application$keypress$key_up
          if (key$code == 'Enter') {
            data$sampleAppend(map)
          } else if (key$code == 'Space') {
            data$addSample(map)
          }
        }
      })

      # --------------------------------------------------------------------------------------
      # Upload Samples

      dlg_upload <- ModalDialogTabUploadSample$new(
        id = 'mod_2',
        lbl_title = .('Add a sample layer...'),
        lbl_ok = .('Add'),
        parent = self
      )

      dlg_upload$bind()

      shiny::observeEvent(input$act_upload_side, {
        dlg_upload$show(ns = ns)
      })

      shiny::observeEvent(input$act_upload, {
        dlg_upload$show(ns = ns)
      })

      shiny::observeEvent(dlg_upload$ok, ignoreInit = TRUE, {
        data$addSamples(map, sf = dlg_upload$out$sf)
      })

      # --------------------------------------------------------------------------------------
      # Leaflet

      shiny::observeEvent(input$pnl_data_open, {
        shiny::insertUI(
          selector = sprintf('#%s', self$ns('pnl_data')),
          where = 'beforeEnd',
          ui = disabledConditional(
            condition = data$samples_count == 0L,
            ui = shiny::tagList(
              buttonDownload(outputId = ns('act_download_shp'), label = 'SHP'),
              buttonDownload(outputId = ns('act_download_kml'), label = 'KML')
            )
          )
        )
      })

      shiny::observeEvent(input$pnl_settings_open, {
        shiny::insertUI(
          selector = sprintf('#%s', self$ns('pnl_settings')),
          where = 'afterBegin',
          ui =
            shiny::absolutePanel(
              top = 45L,
              left = 5L,
              bottom = 5L,
              right = 5L,
              style = 'overflow: hidden'
            )
        )
      })

      output$legend <- shiny::renderUI({
        self$state

        polygon <- data$polygon_focused

        if (nrow(polygon) == 1L) {
          shiny::wellPanel(
            style = 'background-color: #ffffff85',
            shiny::HTML(
              sprintf(
                '
                  <b>%s %s</b><br><br>
                  <b>%s</b> %s.',
                .('Polygon'), polygon$id_n,
                nrow(polygon$samples_sf[[1L]]), .('sample(s)')
              )
            )
          )
        } else {
          NULL
        }
      })

      # --------------------------------------------------------------------------------------
      # Map Observers

      shiny::observeEvent(input$map_marker_click, {
        console_out('Map Marker Click %s', input$map_marker_click$id)

        if (input$map_marker_click$group == 'sample') {
          vars$sample_quadrat <- match(input$map_marker_click$id, vars$sample$quadrats_sf$id)
        }
      })

      shiny::observeEvent(input$map_shape_click, {
        if (grepl('QDR_', input$map_shape_click$group, fixed = TRUE)) {
          data$sampleQuadratEdit(map = map, id = input$map_shape_click$id)
        } else if (input$map_shape_click$group == 'polygons') {
          data$togglePolygonFocused(map, id = input$map_shape_click$id, step = 'sample')
          invalidateState()
        }
      })

      # --------------------------------------------------------------------------------------
      # Drawing

      shiny::observeEvent(input$map_draw_edited_features, {
        data$sampleQuadratPost(map, feature = input$map_draw_edited_features)
      })

      shiny::observeEvent(input$map_edit_toggle, {
      })

      # --------------------------------------------------------------------------------------
      # Status UI

      output$act_download_kml <- shiny::downloadHandler(
        filename = function() {
          getSampleFilename(data$polygon_focused, ext = 'kml')
        },
        content = function(file) {
          writeSpatialLayer(sf = data$samples, file = file, layer = 'samples', fieldname = 'id_user')
        }
      )

      output$act_download_shp <- shiny::downloadHandler(
        filename = function() {
          getSampleFilename(data$polygon_focused, ext = 'zip')
        },
        content = function(file) {
          writeSpatialLayer(sf = data$samples, file = file, layer = 'samples', fieldname = 'id_user')
        }
      )

      output$info <- shiny::renderUI({
        shiny::tagList(
          shiny::tags$div(
            class = 'tip',
            shiny::tags$strong(..('Sample: ')), shiny::HTML(text = self$state$info)
          )
        )
      })
    },
    renderMap = function() {
      shiny::insertUI(
        selector = sprintf('#%s', ns('pnl_settings')),
        where = 'beforeEnd',
        ui =
          shiny::tagList(
            shiny::br(),
            shiny::h6('Sample:'),
            p(shiny::sliderInput(
              inputId = ns('sli_sample_radius'),
              label = .('Sample Radius (meter):'),
              min = 5L,
              max = 200L,
              value = data$settings$getValue('sli_sample_radius'),
              step = 1L
            )),
            p(shiny::sliderInput(
              inputId = ns('sli_sample_zoom'),
              label = .('Sample Zoom:'),
              min = 17L,
              max = 20L,
              value = data$settings$getValue('sli_sample_zoom'),
              step = 1L
            ))
          )
      )
      super$renderMap()
    },
    checkState = function(value) {
      polygons_selected_count <- data$polygons_selected_count

      value$can_commit <- value$modified & polygons_selected_count > 0L
      value$can_rollback <- value$modified & polygons_selected_count > 0L
      value$can_clear <- !value$modified & polygons_selected_count > 0L

      value$can_generate <- any(data$polygons$focused_sample)

      if (state$can_generate) {
        value$info <- sprintf(
          ..('Generate a new sample (%s).'),
          as.character(icon('map-marker-multiple-outline'))
        )
      } else {
        value$info <- ..('Before generate new sample, please click on the map to select the target Polygon.')
      }

      value
    },
    onStateChange = function() {
      if (data$project_method == 'SP_SPV') {
        shinyjs::hide(id = 'bar_sample_1')
        shinyjs::show(id = 'bar_sample_2')
      } else {
        shinyjs::show(id = 'bar_sample_1')
        shinyjs::hide(id = 'bar_sample_2')
      }

      if (data$samples_count == 0L) {
        shinyjs::disable(id = 'act_download_kml')
        shinyjs::disable(id = 'act_download_shp')
      } else {
        shinyjs::enable(id = 'act_download_kml')
        shinyjs::enable(id = 'act_download_shp')
      }

      if (state$can_commit) {
        shinyjs::enable(id = 'act_ok_btn')
      } else {
        shinyjs::disable(id = 'act_ok_btn')
      }

      if (state$can_rollback) {
        shinyjs::enable(id = 'act_rollback_btn')
      } else {
        shinyjs::disable(id = 'act_rollback_btn')
      }

      if (state$can_clear) {
        shinyjs::enable(id = 'act_clear_btn')
      } else {
        shinyjs::disable(id = 'act_clear_btn')
      }

      if (state$can_generate) {
        shinyjs::enable(id = 'act_sample_generate_btn')
        shinyjs::enable(id = 'act_sample_save_btn')
        shinyjs::enable(id = 'act_generate_modal_btn')
        shinyjs::enable(id = 'act_upload_btn')
      } else {
        shinyjs::disable(id = 'act_sample_generate_btn')
        shinyjs::disable(id = 'act_sample_save_btn')
        shinyjs::disable(id = 'act_generate_modal_btn')
        shinyjs::disable(id = 'act_upload_btn')
      }

      super$onStateChange()
    }
  )
)
