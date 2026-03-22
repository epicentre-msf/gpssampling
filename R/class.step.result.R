StepResult <- R6::R6Class(
  classname = 'StepResult',
  inherit = Step,
  portable = FALSE,
  active = list(
    tab_title = function() {
      .('Plan')
    }
  ),
  public = list(
    initialize = function(index = 1L, steps = NULL) {
      super$initialize(id = 'step_result', index = index, steps = steps)
      private$.map_zoom_max <- 20L
    },
    commit = function() {
      data$save(result = TRUE)
    },
    rollback = function() {
      data$rollback(result = TRUE)
    },
    clear = function() {
      data$clear(result = TRUE)
    },
    invalidatePolygons = function() {
      data$displayPolygons(map, only_selected = TRUE, sample = TRUE, status = TRUE)
      data$sampleLoadTable(tbl_id = ns('table'))
      super$invalidatePolygons()
    },
    setSample = function(polygon_selected, samples_sf, samples_quadrat_sf) {
      polygon_selected <- match(TRUE, private$polygons$selected)
      private$polygons$samples_sf[[polygon_selected]] <- samples_sf
      private$polygons$samples_quadrat_sf[[polygon_selected]] <- samples_quadrat_sf
      private$status$modified <- TRUE
    }
  ),
  private = list(
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
            title = .('STEP 4: RESULT'),
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
            p(.('Export the current sampler layer.'))
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
    getUISidePanel = function() {
      shiny::wellPanel(
        style = 'height: 100%; width: 450px; margin-left: 7px; padding: 0px;',
        fillTabset(
          id = ns('tbs_results'),
          fillTabPanel(
            height = 210L, title = .('Data'),
            shiny::fillCol(
              flex = c(NA, 1L, NA),
              shiny::div(
                style = 'padding: 0',
                shiny::div(
                  style = 'float:left; width: 70%',
                  p(shiny::tags$b(.('STEP 4:')), .('SURVEY AND CALCUL'), style = 'color: #337ab7')
                ),
                shiny::div(
                  style = 'float:right; width: 30%',
                  shinyWidgets::textInputIcon(ns('search'), label = NULL, size = 'sm', width = '100%')
                )
              ),
              rhandsontable::rHandsontableOutput(outputId = ns('table'), width = '100%', height = '100%'),
              shiny::div(
                style = 'height: 44px; padding: 7px',
                # shiny::div(
                #   style = 'float:left',
                #   buttonDownload(outputId = self$ns('act_document'), label = .('Survey (Form)'))
                # ),
                shiny::div(
                  style = 'float:right',
                  buttonDownload(outputId = ns('act_download_gpx'), label = 'GPX'),
                  if (isHostedApp()) {
                    button(inputId = self$ns('act_download_gpx_qr'), label = 'GPX', icon = 'qrcode-scan')
                  }
                )
              )
            )
          )
        )
      )
      #   shiny::wellPanel(
      #     style = 'height: 100%; width: 600px; margin-left: 7px',
      #     shiny::fillCol(
      # #       flex = c(NA, NA, NA, 1, NA, NA, NA, NA),
      #       p(shiny::tags$b(.('STEP 4:')), .('SURVEY AND CALCUL'), style = 'color: #337ab7'),
      #       # shiny::fillRow(
      #         # flex = c(1, 1),
      #         # shiny::div(
      #         p(.('SELECTED ZONE'), style = 'color: #337ab7'),
      #         # ),
      #         # shiny::div(
      #         pickerInputEx(
      #           inputId = ns('opt_polygons'),
      #           label = NULL,
      #           choices = NULL,
      #           selected = NULL
      #         ),
      #         # )
      #       # ),
      # #       # shiny::uiOutput(outputId = self$ns('info_polygon')),
      #       rhandsontable::rHandsontableOutput(outputId = ns('table'), width = '100%', height = '100%')
      # #       shiny::div(
      # #         style = 'height: 44px; padding: 7px 0 0 0',
      # #         shiny::div(
      # #           style = 'float:left',
      # #           button(inputId = self$ns('act_calculate'), label = .('Calculate'), semantic = 'primary')
      # #         ),
      # #         shiny::div(
      # #           style = 'float:right',
      # #           buttonDownload(outputId = self$ns('act_document'), label = .('Survey (Form)')),
      # #           buttonDownload(outputId = self$ns('act_download_kml_sb'), label = 'KML'),
      # #           buttonDownload(outputId = self$ns('act_download_shp_sb'), label = 'SHP'),
      # #           buttonDownload(outputId = self$ns('act_download_gpx_sb'), label = 'GPX'),
      # #           button(inputId = self$ns('act_download_gpx_qr_sb'), label = 'GPX', icon = 'qrcode-scan')
      # #         )
      # #       ),
      # #       shiny::br(),
      # #       p(.('RESULT'), style = 'color: #337ab7'),
      # #       shiny::uiOutput(outputId = self$ns('info_result'))
      #     )

      # #     # p(.('RESULT'), style = 'color: #337ab7'),
      # #     # shiny::uiOutput(outputId = self$ns('info_result')),
      # #     # button(inputId = self$ns('act_ok'), icon = 'check-bold', label = 'Save')
      #   )
    },
    getOutputMap = function(session) {

      map <- super$getOutputMap(session)

      map <- leaflet::addEasyButtonBar(map,
        id = ns('bar_group'),
        position = 'topleft',
        easyButtonShiny(inputId = ns('act_set_selector_1'), icon = icon(df_selectors['C1', 'icon'], color = df_selectors['C1', 'color']), title = df_selectors['C1', 'shortcut_label']),
        easyButtonShiny(inputId = ns('act_set_selector_2'), icon = icon(df_selectors['C2', 'icon'], color = df_selectors['C2', 'color']), title = df_selectors['C2', 'shortcut_label']),
        easyButtonShiny(inputId = ns('act_set_selector_3'), icon = icon(df_selectors['C3', 'icon'], color = df_selectors['C3', 'color']), title = df_selectors['C3', 'shortcut_label']),
        easyButtonShiny(inputId = ns('act_set_selector_4'), icon = icon(df_selectors['C4', 'icon'], color = df_selectors['C4', 'color']), title = df_selectors['C4', 'shortcut_label']),
        easyButtonShiny(inputId = ns('act_set_selector_5'), icon = icon(df_selectors['C5', 'icon'], color = df_selectors['C5', 'color']), title = df_selectors['C5', 'shortcut_label']),
        easyButtonShiny(inputId = ns('act_set_selector_6'), icon = icon(df_selectors['C6', 'icon'], color = df_selectors['C6', 'color']), title = df_selectors['C6', 'shortcut_label']),
        easyButtonShiny(inputId = ns('act_set_selector_7'), icon = icon(df_selectors['C7', 'icon'], color = df_selectors['C7', 'color']), title = df_selectors['C7', 'shortcut_label']),
        easyButtonShiny(inputId = ns('act_set_selector_8'), icon = icon(df_selectors['C8', 'icon'], color = df_selectors['C8', 'color']), title = df_selectors['C8', 'shortcut_label'])
      )

      map <- addPaintPolygonControl(map, menu = FALSE, radius = 15L)

      map
    },
    getServer = function(input, output, session) {

      super$getServer(input, output, session)

      vars <- shiny::reactiveValues()

      selecting <- FALSE
      selector_previous <- 'none'
      selector_current <- 'none'

      polygon_focused <- shiny::reactiveVal(0L)

      filtered_samples <- function() {

        table_select <- input$table_select

        if (is.null(table_select)) {
          table_select <- input$table
        }

        recs <- as.data.frame(
          base::t(
            matrix(
              unlist(table_select$data),
              length(table_select$params$columns),
              length(table_select$data)
            )
          )
        )

        dplyr::filter(self$data$samples, id_n %in% as.integer(recs[, 1L]))
      }

      # --------------------------------------------------------------------------------------
      # Actions

      startSelector <- function(selector) {

        selector_current <<- selector
        selecting <<- TRUE

        shinyjs::html(
          id = sprintf('act_set_selector_%s_btn', tolower(selector)),
          html = sprintf(
            '<i class="mdi mdi-%s mdi-18px" style="color: %s"></i>',
            df_selectors[selector, 'icon'],
            df_selectors[selector, 'color']
          )
        )

        map <- startDrawPolygon(map, color = df_selectors[selector, 'color'])
      }

      stopSelector <- function() {

        selecting <<- FALSE

        shinyjs::html(id = 'act_set_selector_1_btn', html = sprintf('<i class="mdi mdi-%s mdi-18px" style="color: %s"></i>', df_selectors['C1', 'icon'], df_selectors['C1', 'color']))
        shinyjs::html(id = 'act_set_selector_2_btn', html = sprintf('<i class="mdi mdi-%s mdi-18px" style="color: %s"></i>', df_selectors['C2', 'icon'], df_selectors['C2', 'color']))
        shinyjs::html(id = 'act_set_selector_3_btn', html = sprintf('<i class="mdi mdi-%s mdi-18px" style="color: %s"></i>', df_selectors['C3', 'icon'], df_selectors['C3', 'color']))
        shinyjs::html(id = 'act_set_selector_4_btn', html = sprintf('<i class="mdi mdi-%s mdi-18px" style="color: %s"></i>', df_selectors['C4', 'icon'], df_selectors['C4', 'color']))
        shinyjs::html(id = 'act_set_selector_5_btn', html = sprintf('<i class="mdi mdi-%s mdi-18px" style="color: %s"></i>', df_selectors['C5', 'icon'], df_selectors['C5', 'color']))
        shinyjs::html(id = 'act_set_selector_6_btn', html = sprintf('<i class="mdi mdi-%s mdi-18px" style="color: %s"></i>', df_selectors['C6', 'icon'], df_selectors['C6', 'color']))
        shinyjs::html(id = 'act_set_selector_7_btn', html = sprintf('<i class="mdi mdi-%s mdi-18px" style="color: %s"></i>', df_selectors['C7', 'icon'], df_selectors['C7', 'color']))
        shinyjs::html(id = 'act_set_selector_8_btn', html = sprintf('<i class="mdi mdi-%s mdi-18px" style="color: %s"></i>', df_selectors['C8', 'icon'], df_selectors['C8', 'color']))

        map <- map |>
          stopDrawPolygon() |>
          eraseAllPolygon()

      }

      shiny::observeEvent(input$act_ok, {
        modalWarning(
          inputIdOk = self$ns('act_ok_confirm'),
          icon = 'alert-plus',
          msg = ..('Are you sure you want to permanently save this sample(s).'),
          msg_info = ..('Saved sample(s) will be overwritten. You can\'t undo this action.'),
          label_cancel = ..('Cancel'),
          label_ok = ..('Ok')
        )
      })

      shiny::observeEvent(input$act_ok_confirm, {
        data$step_sample$commit()
        shiny::removeModal()
      })

      shiny::observeEvent(input$act_set_selector_1, {
        startSelector('C1')
      })

      shiny::observeEvent(input$act_set_selector_2, {
        startSelector('C2')
      })

      shiny::observeEvent(input$act_set_selector_3, {
        startSelector('C3')
      })

      shiny::observeEvent(input$act_set_selector_4, {
        startSelector('C4')
      })

      shiny::observeEvent(input$act_set_selector_5, {
        startSelector('C5')
      })

      shiny::observeEvent(input$act_set_selector_6, {
        startSelector('C6')
      })

      shiny::observeEvent(input$act_set_selector_7, {
        startSelector('C7')
      })

      shiny::observeEvent(input$act_set_selector_8, {
        startSelector('C8')
      })

      shiny::observeEvent(input$map_paint_data, {

        selector <- st_as_sf.feature(input$map_paint_data)
        selector <- sf::st_make_valid(selector)

        samples <- data$polygon_focused$samples_sf[[1L]]

        if (nrow(samples) > 0L) {

          samples_intersect <- sf::st_intersects(samples, selector, FALSE)
          samples <- samples[samples_intersect, ]

          rhandsontable::set_data(id = ns('table'), row = samples$id_n, col = 6L, val = df_selectors[selector_current, 'label'], session = application$session, zero_indexed = FALSE)

        }

      })

      # --------------------------------------------------------------------------------------
      # Shortcuts

      shiny::observeEvent(application$keypress$key_up, ignoreInit = TRUE, ignoreNULL = TRUE, {
        if (data$step == 'step_result') {
          key <- application$keypress$key_up
          if (key$target$name != 'textarea') {
            if (key$key == '1') {
              startSelector('C1')
            } else if (key$key == '2') {
              startSelector('C2')
            } else if (key$key == '3') {
              startSelector('C3')
            } else if (key$key == '4') {
              startSelector('C4')
            } else if (key$key == '5') {
              startSelector('C5')
            } else if (key$key == '6') {
              startSelector('C6')
            } else if (key$key == '7') {
              startSelector('C7')
            } else if (key$key == '8') {
              startSelector('C8')
            }
          }
        }
      })

      # --------------------------------------------------------------------------------------
      # Map Observers

      shiny::observeEvent(input$map_click, {
        # if (!vars$map_shape_click) {
        #   vars$polygon_selected_sf <- NULL
        #   vars$stratum_unselected_sf <- data$step_delimit$getPolygon()
        # }
        # vars$map_shape_click <- FALSE
        if (selecting) {
          stopSelector()
          return()
        }
      })

      shiny::observeEvent(input$map_marker_click, {

        sample <- parseSampleId(id = input$map_marker_click$id)

        # data$sampleToggleSelect(map, sample = sample)

        # data$sampleLoadTable(tbl_id = ns('table'), session = application$session)

        # data$polygonSetFocus(map, id = sample_id$polygon, step = 'result')

        # invalidate(polygon_focused)

        hot_select_cell(ns('table'), sample$id - 1L, 0L, session)

        # invalidateState()
      })

      shiny::observeEvent(input$map_shape_click, {

        if (selecting) {
          stopSelector()
          return()
        }

        if (grepl('HUL', input$map_shape_click$group)) {
          data$togglePolygonFocused(map, id = input$map_shape_click$id, step = 'result')
        } else {
          data$togglePolygonFocused(map, id = input$map_shape_click$id, step = 'result')
          #   invalidate(polygon_focused)
        }
        invalidateState()
        waiter <- waiter::Waiter$new(color = waiter::transparent(.1), html = waiter::spin_3())
        waiter$show()
        on.exit({
          waiter$hide()
        })
        data$sampleLoadTable(tbl_id = ns('table'))
      })

      # shiny::observeEvent(input$opt_polygons, ignoreNULL = TRUE, ignoreInit = TRUE, {

      #   if (selecting) {
      #     stopSelector()
      #     return()
      #   }

      #   data$togglePolygonFocused(map, id = input$opt_polygons, step = 'result')
      #   invalidateState()
      #   waiter <- waiter::Waiter$new(color = waiter::transparent(.1), html = waiter::spin_3())
      #   waiter$show()
      #   on.exit({
      #     waiter$hide()
      #   })
      #   data$sampleLoadTable(tbl_id = ns('table'), session = application$session)
      #   # data$step_sample$selectPolygon(id = input$opt_polygons, map = map)
      # })

      # --------------------------------------------------------------------------------------
      # Handsontable

      output$table <- rhandsontable::renderRHandsontable({
        tbl <- private$getOutputTable(session)
        return(tbl)
      })

      # shiny::observeEvent(vars$activate, ignoreInit = TRUE, {
      #   rhandsontable::render(ns('table'), session)
      #   data$step_sample$selectPolygon(id = input$opt_polygons, map = map)
      # })

      shiny::observeEvent(input$table, ignoreInit = TRUE, {

        changes <- getChange(input$table)

        if (nrow(changes) == 0L) {
          return()
        }

        self$data$samplesSetValueFromTable(
          map,
          recs   = changes$recs,
          vars   = changes$vars,
          values = changes$values_new
        )

      })

      rec_current <- -1L

      shiny::observeEvent(input$table_select, ignoreInit = TRUE, {

        rec_row <- input$table_select$select$r
        rec <- NULL

        if (rec_row == 0L) {
          return()
        } else {
          rec <- input$table_select$data[[rec_row]][[1L]]
        }

        if (is.null(rec)) {
          return()
        } else {
          rec <- as.integer(rec)
        }

        if (rec == rec_current) {
          return()
        }

        rec_current <- rec

        polygon <- dplyr::filter(data$polygons, focused_result)

        if (nrow(polygon) == 1L) {

          sample <- polygon$samples_sf[[1L]][rec, ]

          flyToSpatialLL(map,
            lng = sample$lon,
            lat = sample$lat,
            zoom = 17L
          )

          map |>
            leaflet::clearGroup(group = 'label') |>
            leaflet::addCircleMarkers(
              lng = sample$lon,
              lat = sample$lat,
              color = 'yellow',
              group = 'label',
              label = sample$id_user,
              labelOptions = leaflet::labelOptions(
                interactive = TRUE,
                noHide = TRUE,
                direction = 'top',
                offset = c(0L, -9L),
                sticky = FALSE
              ),
              layerId = sprintf('%s_/LBL', sample$id),
              opacity = 1L,
              options = leaflet::pathOptions(pane = 'marker'),
              radius = 8L,
              stroke = TRUE,
              weight = 1L
            )

        }

      })

      shiny::observeEvent(input$search, ignoreInit = TRUE, ignoreNULL = TRUE, {
        if (input$search != '') {
          hot_filter(id = ns('table'), column = 1L, filter = input$search, session = application$session)
        }
      })

      shiny::observeEvent(polygon_focused(), ignoreInit = TRUE, {
        data$sampleLoadTable(tbl_id = ns('table'))
      })

      shiny::observeEvent(input$act_calculate, {
        data$calculateSample()
      })

      # --------------------------------------------------------------------------------------
      # Download Actions

      shiny::observeEvent(input$pnl_data_open, {
        shiny::insertUI(
          selector = sprintf('#%s', ns('pnl_data')),
          where = 'beforeEnd',
          ui = disabledConditional(
            condition = self$data$samples_count == 0L,
            ui = shiny::tagList(
              buttonDownload(outputId = ns('act_download_shp_sb'), label = 'SHP'),
              buttonDownload(outputId = ns('act_download_kml_sb'), label = 'KML'),
              buttonDownload(outputId = ns('act_download_gpx_sb'), label = 'GPX')
            )
          )
        )
      })
      output$act_document <- shiny::downloadHandler(
        filename = function() {
          getSampleFilename(data$polygon_focused, ext = 'docx')
        },
        content = function(file) {
          data$sampleExportToDoc(file)
        }
      )

      output$act_download_kml_sb <- shiny::downloadHandler(
        filename = function() {
          getSampleFilename(data$polygon_focused, ext = 'kml')
        },
        content = function(file) {
          writeSpatialLayer(sf = filtered_samples(), file = file, layer = 'samples', fieldname = 'id_user')
        }
      )

      output$act_download_shp_sb <- shiny::downloadHandler(
        filename = function() {
          getSampleFilename(data$polygon_focused, ext = 'zip')
        },
        content = function(file) {
          writeSpatialLayer(sf = filtered_samples(), file = file, layer = 'samples', fieldname = 'id_user')
        }
      )

      output$act_download_csv <- shiny::downloadHandler(
        filename = function() {
          getSampleFilename(data$polygon_focused, ext = 'csv')
        },
        content = function(file) {
          fp <- fs::file_temp()
          utils::write.csv2(ascache.frame(project$sptsdf), fp, row.names = FALSE, na = '.')
          fs::file_copy(path = fp, new_path = file)
        }
      )

      output$act_download_gpx_sb <- shiny::downloadHandler(
        filename = function() {
          getSampleFilename(data$polygon_focused, ext = 'gpx')
        },
        content = function(file) {
          writeSpatialLayer(sf = filtered_samples(), file = file, layer = 'samples', fieldname = 'id_user')
        }
      )

      output$act_download_gpx <- shiny::downloadHandler(
        filename = function() {
          getSampleFilename(data$polygon_focused, ext = 'gpx')
        },
        content = function(file) {
          writeSpatialLayer(sf = filtered_samples(), file = file, layer = 'samples', fieldname = 'id_user')
        }
      )

      shiny::observeEvent(input$act_download_gpx_qr, {

        file <- getSampleFilename(data$polygon_focused, ext = 'gpx')

        fname_local <- getDirAppTemp(file)
        fname_uri <-
          sprintf(
            '%s//%s',
            session$clientData$url_protocol,
            session$clientData$url_hostname
          )
        if (session$clientData$url_port != '') {
          fname_uri <- sprintf('%s:%s', fname_uri, session$clientData$url_port)
        }
        if (session$clientData$url_pathname != '') {
          fname_uri <- sprintf('%s%s', fname_uri, session$clientData$url_pathname)
        } else {
          fname_uri <- sprintf('%s/', fname_uri)
        }
        fname_uri <- sprintf('%scache/temp/%s', fname_uri, gsub(' ', '%20', file))

        writeSpatialLayer(sf = filtered_samples(), file = fname_local, layer = 'samples', fieldname = 'id_user')

        dlg <- ModalDialogQRCode$new(id = 'qrcode', parent = self)
        dlg$bind()
        dlg$show(ns = ns, qrcode = fname_uri)
      })

      # --------------------------------------------------------------------------------------
      # Status UI

      output$info <- shiny::renderUI({
        shiny::tagList(
          shiny::tags$div(
            class = 'tip',
            shiny::tags$strong(..('Result: ')), shiny::HTML(text = self$state$info)
          )
        )
      })

      output$info_polygon <- shiny::renderUI({
        console_out('Render Info Polygon')
        polygon <- data$polygons_selected[input$opt_polygons, ]
        polygon_type <- dplyr::case_when(
          data$project_method == 'SP_QDR' ~ .('Quadrat'),
          data$project_method == 'SP_TSQ' ~ .('TSquare'),
          data$project_method == 'SP_SPV' ~ .('Random sample')
        )
        polygon_count <- dplyr::case_when(
          data$project_method == 'SP_QDR' ~ sprintf(..('%s samples (%sx%s m)'), polygon$count, polygon$size, polygon$size),
          data$project_method == 'SP_TSQ' ~ sprintf(..('%s samples'), polygon$count),
          data$project_method == 'SP_SPV' ~ sprintf(..('%s samples'), polygon$count)
        )
        shiny::tagList(
          shiny::tags$div(
            class = 'tip',
            shiny::tags$strong(polygon_type), shiny::HTML(text = polygon_count)
          )
        )
      })

      output$info_result <- shiny::renderUI({

        self$state

        if (nrow(data$polygon_focused) == 0L) {
          return()
        }

        #   df <- self$getPolygon()
        #   if (is.defined(df)) {
        #     rf <- data$step_identify$getPolygon()$roofs_count
        #     df <- self$getPolygon() |>
        #       sf::st_set_geometry(NULL) |>
        #       dplyr::mutate(
        #         type = dplyr::recode(type,
        #           'SP_QDR' = .('Quadrat'),
        #           'SP_TSQ' = .('TSquare'),
        #           'SP_SPV' = .('Sample')
        #         ),
        #         method = dplyr::recode(method,
        #           'random' = .('Random'),
        #           'regular' = .('Regular'),
        #           'hexagonal' = .('Hexagonal')
        #         ),
        #         area = round(as.integer(area) / 10000, 2),
        #         roofs_count = rf
        #       ) |>
        #       dplyr::select(
        #         id,
        #         area,
        #         roofs_count,
        #         type,
        #         method,
        #         count,
        #         pop_u5,
        #         pop_u5_i,
        #         pop_u5_il,
        #         pop_u5_iu,
        #         pop_a5,
        #         pop_a5_i,
        #         pop_a5_il,
        #         pop_a5_iu,
        #         pop,
        #         pop_i,
        #         pop_il,
        #         pop_iu,
        #         confidence,
        #         tvalue,
        #         t,
        #         z,
        #         z_p,
        #         mc,
        #         mc_p,
        #         comment
        #       )
        #   }

        #   df <-
        #     dplyr::mutate_all(df, as.character) |>
        #     dplyr::filter(id == input$opt_polygons)

        #   df[is.na(df)] <- ''

        out <- sprintf(
          '
            <table style="width: 350px; text-align: right;">
             <tr>
              <td align="left" style="width:40%%;"><b>Map</b></td>
              <td align="left" style="width:5%%;"></td>
              <td colspan = 3></td>
             </tr>
             <tr><td align="left" style="padding-left:10px;">Area</td>              <td colspan = 4>%s</td></tr>
             <tr><td align="left" style="padding-left:20px;"><i>in km\u00b2</i></td>     <td colspan = 4>%s</td></tr>
             <tr><td align="left" style="padding-left:20px;"><i>in hectares</i></td><td colspan = 4>%s</td></tr>
             <tr><td colspan = 5>&nbsp;</td></tr>
             <tr><td align="left"><b>Confidence</b></td>                            <td colspan = 4>%s</td></tr>
             <tr><td align="left" style="padding-left:10px;">Critical Value (t)</td><td colspan = 4>%s</td></tr>
            </table>',
          snumber(data$polygon_focused$area, '%0.0f m\u00b2'),
          snumber(data$polygon_focused$area / 1000000L, '%0.2f km\u00b2'),
          snumber(data$polygon_focused$area / 10000L, '%0.2f hectares'),
          snumber(data$polygon_focused$confidence, '%s %%'),
          snumber(data$polygon_focused$tvalue, '%0.2f')
        )

        if (data$project_method == 'SP_QDR') {

          out_method <- sprintf(
            '
              <table style="width: 350px; text-align: right;">
              <tr>
                <td style="width:40%%;"align="left"><b>Quadrat Estimation</b></td>
                <td style="width:15%%;">Lower</td>
                <td style="width:15%%;">Value</td>
                <td style="width:15%%;" align="left" style="padding-left:10px;">SE</td>
                <td style="width:15%%;">Upper</td>
              </tr>
              <tr><td align="left" style="padding-left:10px;">Population</td>        <td>%s</td>   <td>%s</td>   <td align="left" style="padding-left:10px;">%s</td><td>%s</td></tr>
              <tr><td align="left" style="padding-left:20px;"><i>< 5 years</i></td>  <td>%s</td>   <td>%s</td>   <td align="left" style="padding-left:10px;">%s</td><td>%s</td></tr>
              <tr><td align="left" style="padding-left:20px;"><i>>= 5 years</i></td> <td>%s</td>   <td>%s</td>   <td align="left" style="padding-left:10px;">%s</td><td>%s</td></tr>
              </table>',
            snumber(data$polygon_focused$pop - data$polygon_focused$pop_i, '%0.0f'),
            snumber(data$polygon_focused$pop, '%0.0f'),
            snumber(data$polygon_focused$pop_i, '+/- %0.0f'),
            snumber(data$polygon_focused$pop + data$polygon_focused$pop_i, '%0.0f'),
            snumber(data$polygon_focused$pop_u5 - data$polygon_focused$pop_u5_i, '%0.0f'),
            snumber(data$polygon_focused$pop_u5, '%0.0f'),
            snumber(data$polygon_focused$pop_u5_i, '+/- %0.0f'),
            snumber(data$polygon_focused$pop_u5 + data$polygon_focused$pop_u5_i, '%0.0f'),
            snumber(data$polygon_focused$pop_a5 - data$polygon_focused$pop_a5_i, '%0.0f'),
            snumber(data$polygon_focused$pop_a5, '%0.0f'),
            snumber(data$polygon_focused$pop_a5_i, '+/- %0.0f'),
            snumber(data$polygon_focused$pop_a5 + data$polygon_focused$pop_a5_i, '%0.0f')
          )

        } else if (data$project_method == 'SP_TSQ') {

          if (is.na(data$polygon_focused$z_p)) {
            out_test_1 <- '-'
            out_test_2 <- '-'
            out_sd <- '-'
          } else {

            if (data$polygon_focused$z_p < 0.025) {
              out_test_1 <- sprintf(
                'Signifiant (t=%s z=%s p=%s)',
                snumber(data$polygon_focused$t, '%0.4f'),
                snumber(data$polygon_focused$z, '%0.4f'),
                snumber(data$polygon_focused$z_p, '%0.2f')
              )
              out_test_2 <- ''
              if (data$polygon_focused$z > 0L) {
                out_sd <- 'Heterogeneous (Not Random Regular)'
              } else {
                out_sd <- 'Heterogeneous (Not Random Aggregated)'
              }

            } else {
              out_test_1 <- sprintf(
                'Not Signifiant (t=%s z=%s p=%s)',
                snumber(data$polygon_focused$t, '%0.4f'),
                snumber(data$polygon_focused$z, '%0.4f'),
                snumber(data$polygon_focused$z_p, '%0.2f')
              )
              if (data$polygon_focused$mc_p > 0.05) {
                out_test_2 <- sprintf(
                  'Signifiant (mc=%s p=%s)',
                  snumber(data$polygon_focused$mc, '%0.4f'),
                  snumber(data$polygon_focused$mc_p, '%0.2f')
                )
                out_sd <- 'Homogeneous (Random)'
              } else {
                out_test_2 <- sprintf(
                  'Not Signifiant (mc=%s p=%s)',
                  snumber(data$polygon_focused$mc, '%0.4f'),
                  snumber(data$polygon_focused$mc_p, '%0.2f')
                )
                out_sd <- 'Non-Homogeneous (Random)'
              }
            }
          }

          out_method <- sprintf(
            '
              <table style="width: 350px; text-align: right;">
              <tr>
                <td style="width:40%%;"align="left"><b>Spatial Distribution</b></td>
                <td colspan= 4"></td>
              </tr>
              <tr><td align="left" style="padding-left:10px;"><i>Test 1</i></td><td align="left" colspan= 4">%s</td></tr>
              <tr><td align="left" style="padding-left:10px;"><i>Test 2</i></td><td align="left" colspan= 4">%s</td></tr>
              <tr><td align="left" style="padding-left:20px;"><i>></i></td><td align="left" colspan= 4">%s</td></tr>
              <tr><td colspan = 5>&nbsp;</td></tr>
              <tr>
                <td style="width:40%%;"align="left"><b>TSquare Estimation</b></td>
                <td style="width:15%%;">Lower</td>
                <td style="width:15%%;">Value</td>
                <td style="width:15%%;" align="left" style="padding-left:10px;">SE</td>
                <td style="width:15%%;">Upper</td>
              </tr>
              <tr><td align="left" style="padding-left:10px;">Population</td>        <td>%s</td>   <td>%s</td>   <td align="left" style="padding-left:10px;">%s</td><td>%s</td></tr>
              <tr><td align="left" style="padding-left:20px;"><i>< 5 years</i></td>  <td>%s</td>   <td>%s</td>   <td align="left" style="padding-left:10px;">%s</td><td>%s</td></tr>
              <tr><td align="left" style="padding-left:20px;"><i>>= 5 years</i></td> <td>%s</td>   <td>%s</td>   <td align="left" style="padding-left:10px;">%s</td><td>%s</td></tr>
              </table>',
            out_test_1,
            out_test_2,
            out_sd,
            snumber(data$polygon_focused$pop - data$polygon_focused$pop_i, '%0.0f'),
            snumber(data$polygon_focused$pop, '%0.0f'),
            snumber(data$polygon_focused$pop_i, '+/- %0.0f'),
            snumber(data$polygon_focused$pop + data$polygon_focused$pop_i, '%0.0f'),
            snumber(data$polygon_focused$pop_u5 - data$polygon_focused$pop_u5_i, '%0.0f'),
            snumber(data$polygon_focused$pop_u5, '%0.0f'),
            snumber(data$polygon_focused$pop_u5_i, '+/- %0.0f'),
            snumber(data$polygon_focused$pop_u5 + data$polygon_focused$pop_u5_i, '%0.0f'),
            snumber(data$polygon_focused$pop_a5 - data$polygon_focused$pop_a5_i, '%0.0f'),
            snumber(data$polygon_focused$pop_a5, '%0.0f'),
            snumber(data$polygon_focused$pop_a5_i, '+/- %0.0f'),
            snumber(data$polygon_focused$pop_a5 + data$polygon_focused$pop_a5_i, '%0.0f')
          )

        } else {

          out_method <- sprintf(
            '
              <table style="width: 350px; text-align: right;">
              <tr>
                <td style="width:40%%;"align="left"><b>Random Estimation</b></td>
                <td style="width:15%%;">Lower</td>
                <td style="width:15%%;">Value</td>
                <td style="width:15%%;" align="left" style="padding-left:10px;">SE</td>
                <td style="width:15%%;">Upper</td>
              </tr>
              <tr><td align="left" style="padding-left:10px;">Population</td>        <td>%s</td>   <td>%s</td>   <td align="left" style="padding-left:10px;">%s / %s</td><td>%s</td></tr>
              <tr><td align="left" style="padding-left:20px;"><i>< 5 years</i></td>  <td>%s</td>   <td>%s</td>   <td align="left" style="padding-left:10px;">%s / %s</td><td>%s</td></tr>
              <tr><td align="left" style="padding-left:20px;"><i>>= 5 years</i></td> <td>%s</td>   <td>%s</td>   <td align="left" style="padding-left:10px;">%s / %s</td><td>%s</td></tr>
              </table>',
            snumber(data$polygon_focused$pop - data$polygon_focused$pop_il, '%0.0f'),
            snumber(data$polygon_focused$pop, '%0.0f'),
            snumber(data$polygon_focused$pop_il, '- %0.0f'),
            snumber(data$polygon_focused$pop_iu, '+ %0.0f'),
            snumber(data$polygon_focused$pop + data$polygon_focused$pop_iu, '%0.0f'),
            snumber(data$polygon_focused$pop_u5 - data$polygon_focused$pop_u5_il, '%0.0f'),
            snumber(data$polygon_focused$pop_u5, '%0.0f'),
            snumber(data$polygon_focused$pop_u5_il, '- %0.0f'),
            snumber(data$polygon_focused$pop_u5_iu, '+ %0.0f'),
            snumber(data$polygon_focused$pop_u5 + data$polygon_focused$pop_u5_iu, '%0.0f'),
            snumber(data$polygon_focused$pop_a5 - data$polygon_focused$pop_a5_il, '%0.0f'),
            snumber(data$polygon_focused$pop_a5, '%0.0f'),
            snumber(data$polygon_focused$pop_a5_il, '- %0.0f'),
            snumber(data$polygon_focused$pop_a5_iu, '+ %0.0f'),
            snumber(data$polygon_focused$pop_a5 + data$polygon_focused$pop_a5_iu, '%0.0f')
          )
        }
        #   if(df$pop=='') {
        # out <- sprintf(out_fmt,'Not enough data. Complete the survey',..('Age Group'),'-','-','-','-','-','-', '')
        #   } else {

        #     if(df$type == 'TSquare') {
        #         out <- sprintf(out_fmt,
        #           '',
        #           ..('Age Group'),
        #           df$pop_u5,
        #           df$pop_u5_i,
        #           df$pop_a5,
        #           df$pop_a5_i,
        #           df$pop,
        #           df$pop_i,
        #           ''
        #         )

        #     } else if(df$type == 'Quadrat') {
        #         out <- sprintf(out_fmt,
        #           '',
        #           ..('Age Group'),
        #           df$pop_u5,
        #           df$pop_u5_i,
        #           df$pop_a5,
        #           df$pop_a5_i,
        #           df$pop,
        #           df$pop_i,
        #           sprintf('Confidence %s%% (t-value : %0.3f)', df$confidence, as.double(df$tvalue))
        #         )
        #     } else {

        #       out_fmt <- '
        #           <p>%s</p>
        #           <table>
        #           <tr>
        #             <td width = 100><b>%s</b></td>
        #             <td width = 75 ><b>Pop.</b></td>
        #             <td width = 75 ><b>Pop. (CI)</b></td>
        #           </tr>
        #           <tr>
        #             <td>&lt; 5 years</td><td>%s</td><td>%s</td>
        #           </tr>
        #           <tr>
        #             <td>&gt;= 5 years</td><td>%s</td><td>%s</td>
        #           </tr>
        #           <tr>
        #             <td>Total</td><td>%s</td><td>%s</td>
        #           </tr>
        #           </table>
        #           <br>
        #           <p>%s</p>
        #           '

        #         out <- sprintf(out_fmt,
        #         '',
        #         ..('Age Group'),
        #         df$pop_u5,
        #         sprintf('%s - %s', df$pop_u5_il, df$pop_u5_iu),
        #         df$pop_a5,
        #         sprintf('%s - %s', df$pop_a5_il, df$pop_a5_iu),
        #         df$pop,
        #         sprintf('%s - %s', df$pop_il, df$pop_iu),
        #         ''
        #         )
        #     }

        #   }

        shiny::tagList(
          shiny::tags$div(
            class = 'tip',
            shiny::HTML(text = paste(out, '<br>', out_method))
          )
        )
      })

    },
    onStateChange = function() {

      if (self$data$samples_count == 0L) {
        shinyjs::disable(id = 'act_download_kml_sb')
        shinyjs::disable(id = 'act_download_shp_sb')
        shinyjs::disable(id = 'act_download_gpx_sb')
        shinyjs::disable(id = 'act_download_gpx')
        shinyjs::disable(id = 'act_download_gpx_qr')
      } else {
        shinyjs::enable(id = 'act_download_kml_sb')
        shinyjs::enable(id = 'act_download_shp_sb')
        shinyjs::enable(id = 'act_download_gpx_sb')
        shinyjs::enable(id = 'act_download_gpx')
        shinyjs::enable(id = 'act_download_gpx_qr')
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

      # if (state$can_calculate) {
      #   shinyjs::enable(id = 'act_calculate')
      # } else {
      #   shinyjs::disable(id = 'act_calculate')
      # }

      sample_count <- 0L

      if (state$can_set) {

        shinyjs::enable(id = 'act_set_selector_1_btn')
        shinyjs::enable(id = 'act_set_selector_2_btn')
        shinyjs::enable(id = 'act_set_selector_3_btn')
        shinyjs::enable(id = 'act_set_selector_4_btn')
        shinyjs::enable(id = 'act_set_selector_5_btn')
        shinyjs::enable(id = 'act_set_selector_6_btn')
        shinyjs::enable(id = 'act_set_selector_7_btn')
        shinyjs::enable(id = 'act_set_selector_8_btn')

      } else {

        shinyjs::disable(id = 'act_set_selector_1_btn')
        shinyjs::disable(id = 'act_set_selector_2_btn')
        shinyjs::disable(id = 'act_set_selector_3_btn')
        shinyjs::disable(id = 'act_set_selector_4_btn')
        shinyjs::disable(id = 'act_set_selector_5_btn')
        shinyjs::disable(id = 'act_set_selector_6_btn')
        shinyjs::disable(id = 'act_set_selector_7_btn')
        shinyjs::disable(id = 'act_set_selector_8_btn')

      }

      super$onStateChange()

    },
    checkState = function(value) {

      polygons_selected_count <- data$polygons_selected_count

      value$can_commit <- value$modified & polygons_selected_count > 0L
      value$can_rollback <- value$modified & polygons_selected_count > 0L
      value$can_clear <- !value$modified & polygons_selected_count > 0L

      value$can_set <- any(data$polygons$focused_result)

      if (value$can_set) {
        polygon <- data$polygon_focused
        polygon_samples <- as.data.frame(polygon$samples_sf[[1L]])
        if (nrow(polygon_samples) > 0L) {
          if (data$project_method == 'SP_TSQ') {
            polygon_samples <- dplyr::select(polygon_samples, d1, d2, pop_u5_1, pop_a5_1, pop_u5_2, pop_a5_2)
          } else {
            polygon_samples <- dplyr::select(polygon_samples, pop_u5, pop_a5)
          }
        }
        value$can_calculate <- !anyNA(polygon_samples)
        if (value$can_calculate) {
          value$info <- sprintf(..('Data collected is completed. You can calculate the population estimation.'))
        } else {
          value$info <- sprintf(..('Complete the data table before calculate the population estimation.'))
        }
      } else {
        value$can_calculate <- FALSE
        value$info <- sprintf(..('<span class="blink">Click on the layer to select polygon(s).</span>'))
      }

      value
    }
  )
)
