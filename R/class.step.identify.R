StepIdentify <- R6::R6Class(
  classname = 'StepIdentify',
  inherit = Step,
  portable = FALSE,
  active = list(
    tab_title = function() {
      if (data$project_method == 'RS_SMP') {
        return(.('Identify'))
      } else {
        return(.('Shrink'))
      }
    },
    lbl_cancel = function() {
      if (data$project_method == 'RS_SMP') {
        return(..('Are you sure you want to permanently delete this roofs?'))
      } else {
        return(..(
          'Are you sure you want to permanently delete this urbans areas?'
        ))
      }
    },
    lbl_cancel_info = function() {
      ..(
        'If <b>yes</b>, all depend work will be deleted <b>permanently</b>. You can\'t undo this action.'
      )
    },
    lbl_ok = function() {
      if (data$project_method == 'RS_SMP') {
        return(..('Are you sure you want to permanently save this roofs?'))
      } else {
        return(..(
          'Are you sure you want to permanently save this urbans areas?'
        ))
      }
    },
    lbl_ok_info = function() {
      if (data$project_method == 'RS_SMP') {
        return(..(
          'Saved roofs will be overwritten. You can\'t undo this action.'
        ))
      } else {
        return(..(
          'Saved urbans areas will be overwritten. You can\'t undo this action.'
        ))
      }
    }
  ),
  public = list(
    initialize = function(index = 1L, steps = NULL) {
      super$initialize(id = 'step_identify', index = index, steps = steps)

      private$grid_trigger <- reactiveTrigger()
    },
    invalidateStatus = function() {
    },
    commit = function() {
      data$save(identify = TRUE)
    },
    rollback = function() {
      data$rollback(identify = TRUE)
    },
    clear = function() {
      data$clear(identify = TRUE)
    },
    invalidatePolygons = function() {
      data$displayPolygons(map, only_selected = TRUE)
      map |>
        leaflet::clearGroup('identify_grid') |>
        leaflet::clearGroup('identify_grid_border') |>
        leaflet::clearGroup('identify_building')
      super$invalidatePolygons()
    },
    invalidateGridIdentifyStatus = function(force = FALSE) {
      data$displayGridIdentifyStatus(
        map = map,
        token = shiny::getDefaultReactiveDomain()$token,
        force = force
      )
    }
  ),
  private = list(
    grid = NULL,
    grid_trigger = NULL,
    roofs_uploaded = NULL,
    status = list(
      mode = 'select',
      modified = FALSE
    ),
    getUIMap = function() {
      shiny::tagList(
        super$getUIMap(),
        shiny::absolutePanel(
          bottom = 18L,
          left = 78L,
          fixed = TRUE,
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
            title = .('STEP 2: IDENTIFY'),
            id = ns('pnl_step'),
            icon = icon('menu', size = 's'),
            shiny::br(),
            p(.(
              'The second step is to identify each building in the polygons of interest.'
            )),
            p(.('You can select all the polygons directly.')),
            p(
              button(
                inputId = ns('act_valid_all_side'),
                icon = icon('checkbox-multiple-marked', color = 'green'),
                label = .('Valid All')
              )
            )
          ),
          sidebar_pane(
            title = .('EXPORT'),
            id = ns('pnl_data'),
            icon = icon('download', size = 's'),
            shiny::br(),
            shiny::h6('Export:'),
            p(.('Export the current rooftops layer.'))
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
            shiny::tags$p(
              class = 'intro',
              .('Get Started with Step-by-Step Guide'),
              style = 'font-size: 14px'
            ),
            shiny::tags$ul(
              shiny::tags$li(actionLink(
                inputId = ns('link_guide_ui'),
                label = .('Interface')
              ))
            )
          )
        )
      )
    },
    getOutputMap = function(session) {
      map <- super$getOutputMap(session)

      # if (!getOption('identify')) {
      map <- leaflet::addEasyButtonBar(
        map,
        id = ns('bar_layers'),
        position = 'topleft',
        easyButtonShiny(
          inputId = ns('act_upload'),
          icon = icon('layers-plus'),
          title = ..('Add a Roofs layer...')
        )
      )
      # }

      map <- leaflet::addEasyButtonBar(
        map,
        id = ns('bar_identify'),
        position = 'topleft',
        easyButtonShiny(
          inputId = ns('act_identify'),
          icon = icon('feature-search-outline'),
          title = ..('Search and identify')
        )
      )

      map <- leaflet::addEasyButtonBar(
        map,
        id = ns('bar_check'),
        position = 'topleft',
        easyButtonShiny(
          inputId = ns('act_check'),
          icon = icon('checkbox-blank', color = '#ffd557'),
          title = ..('Check (SPACE)')
        ),
        easyButtonShiny(
          inputId = ns('act_valid'),
          icon = icon('checkbox-blank', color = 'green'),
          title = ..('Valid (ENTER)')
        )
      )

      map <- leaflet::addEasyButtonBar(
        map,
        id = ns('bar_valid'),
        position = 'topleft',
        easyButtonShiny(
          inputId = ns('act_valid_all'),
          icon = icon('checkbox-multiple-marked', color = 'green'),
          title = ..('Valid All')
        )
      )

      map
    },
    getServer = function(input, output, session) {
      super$getServer(input, output, session)

      vars <- shiny::reactiveValues(
        polygon_selected_sf = NULL,
        identify = NULL,
        mode = 'identify_unspecified',
        mode_draw = 'swipe'
      )

      # --------------------------------------------------------------------------------------
      # Actions

      shiny::observeEvent(input$act_identify, {
        data$searchSwipeTile(
          map = map,
          direction = 'next',
          map_size = input$map_size,
          token = session$token
        )
      })

      shiny::observeEvent(input$act_check, {
        data$searchSwipeTile(
          map = map,
          direction = 'next',
          map_size = input$map_size,
          token = session$token,
          mapped = TRUE
        )
      })

      shiny::observeEvent(input$act_valid, {
        data$searchSwipeTile(
          map = map,
          direction = 'next',
          map_size = input$map_size,
          token = session$token,
          valid = TRUE
        )
      })

      shiny::observeEvent(input$act_valid_all, {
        modalWarning(
          inputIdOk = ns('act_valid_all_confirm'),
          icon = 'alert-plus',
          msg = ..('Are you sure you want to valid all the polygons.'),
          msg_info = ..(
            'All work will be cleared / overwritten. You can\'t undo this action.'
          ),
          label_cancel = ..('Cancel'),
          label_ok = ..('Ok')
        )
      })

      shiny::observeEvent(input$act_valid_all_confirm, {
        data$validAll(map = map)
        shiny::removeModal()
      })

      shiny::observeEvent(input$sli_circle_radius, ignoreInit = TRUE, {
        data$settings$setValue('sli_circle_radius', input$sli_circle_radius)
      })

      shiny::observeEvent(input$sli_circle_opacity, ignoreInit = TRUE, {
        data$settings$setValue('sli_circle_opacity', input$sli_circle_opacity)
      })

      shiny::observeEvent(input$csi_circle_color, ignoreInit = TRUE, {
        data$settings$setValue('csi_circle_color', input$csi_circle_color)
      })

      # --------------------------------------------------------------------------------------
      # Shortcuts

      shiny::observeEvent(
        application$keypress$key,
        ignoreInit = TRUE,
        ignoreNULL = TRUE,
        {
          if (data$step == 'step_identify') {
            key <- application$keypress$key
            logDebug("Keypress: %s", key)
            if (key %in% paste0('alt+', 0:9)) {
              data$addCell(
                map = map,
                token = session$token,
                cell = as.integer(stringr::str_remove(
                  string = key,
                  pattern = '^alt+'
                ))
              )
            } else if (key == '-') {
              data$addCell(map = map, token = session$token, cell = -1L)
            } else if (key == 'enter') {
              data$searchSwipeTile(
                map = map,
                direction = 'next',
                map_size = input$map_size,
                token = session$token,
                valid = TRUE
              )
            } else if (key == 'space') {
              data$searchSwipeTile(
                map = map,
                direction = 'next',
                map_size = input$map_size,
                token = session$token,
                mapped = TRUE
              )
            } else if (key == 'delete') {
              data$searchSwipeTile(
                map = map,
                direction = 'next',
                map_size = input$map_size,
                token = session$token,
                invalid = TRUE
              )
            } else if (key == '4') {
              data$searchSwipeTile(
                map = map,
                direction = 'left',
                map_size = input$map_size,
                token = session$token
              )
            } else if (key == '6') {
              data$searchSwipeTile(
                map = map,
                direction = 'right',
                map_size = input$map_size,
                token = session$token
              )
            } else if (key == '8') {
              data$searchSwipeTile(
                map = map,
                direction = 'top',
                map_size = input$map_size,
                token = session$token
              )
            } else if (key == '2') {
              data$searchSwipeTile(
                map = map,
                direction = 'bottom',
                map_size = input$map_size,
                token = session$token
              )
            }
          }
        }
      )

      # --------------------------------------------------------------------------------------
      # Download Actions

      shiny::observeEvent(input$pnl_data_open, {
        shiny::insertUI(
          selector = sprintf('#%s', ns('pnl_data')),
          where = 'beforeEnd',
          ui = disabledConditional(
            condition = sum(data$polygons_selected$roofs_count, na.rm = TRUE) ==
              0L,
            ui = shiny::tagList(
              buttonDownload(outputId = ns('act_download_shp'), label = 'SHP'),
              buttonDownload(outputId = ns('act_download_kml'), label = 'KML'),
              buttonDownload(outputId = ns('act_download_gpx'), label = 'GPX')
            )
          )
        )
      })

      output$act_download_kml <- shiny::downloadHandler(
        filename = function() {
          sprintf('rooftops - %s.kml', Sys.Date())
        },
        content = function(file) {
          writeSpatialLayer(
            sf = data$roofs,
            file = file,
            layer = sprintf('rooftops - %s', Sys.Date()),
            fieldname = 'id'
          )
        }
      )

      output$act_download_shp <- shiny::downloadHandler(
        filename = function() {
          sprintf('rooftops - %s.zip', Sys.Date())
        },
        content = function(file) {
          writeSpatialLayer(
            sf = data$roofs,
            file = file,
            layer = sprintf('rooftops - %s', Sys.Date()),
            fieldname = 'id'
          )
        }
      )

      output$act_download_gpx <- shiny::downloadHandler(
        filename = function() {
          sprintf('rooftops - %s.gpx', Sys.Date())
        },
        content = function(file) {
          writeSpatialLayer(
            sf = data$roofs,
            file = file,
            layer = sprintf('rooftops - %s', Sys.Date()),
            fieldname = 'id'
          )
        }
      )

      # --------------------------------------------------------------------------------------
      # Upload Roofs

      dlg_upload <- ModalDialogTabUploadIdentify$new(
        id = 'mod_2',
        lbl_title = .('Add a buildings layer...'),
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
        if (inherits(dlg_upload$out$sf, 'sf')) {
          data$addRoofs(map, roofs = dlg_upload$out$sf)
        } else {
          data$addRoofs(map, roofs = dlg_upload$out$sf$roofs)
        }
        shiny::removeModal()
      })

      # --------------------------------------------------------------------------------------
      # Leaflet

      output$legend <- shiny::renderUI({
        data$roofs_changed()

        tiles_status <- data$getTileStatus()

        tiles_n <- sum(tiles_status$n[1:4])

        shiny::wellPanel(
          style = 'background-color: #ffffff85',
          shiny::HTML(
            sprintf(
              '
                <table width="100%%">
                 <tr>
                  <td><b>%s</b> (N=%s)</td>
                  <td style="text-align: right;"><b>%s %s</b> %s</td>
                 </tr>
                </table>
                <br>
                <table>
                <tr><td><font color="white">&#9632;</font> %s:&nbsp;&nbsp;</td><td><b>%s</b> (%s%%)</td></tr>
                <tr><td><font color="yellow">&#9632;</font> %s:&nbsp;&nbsp;</td><td><b>%s</b> (%s%%)</td></tr>
                <tr><td><font color="green">&#9632;</font> %s:&nbsp;&nbsp;</td><td><b>%s</b> (%s%%)</td></tr>
                <tr><td><font color="red">&#9632;</font> %s:&nbsp;&nbsp;</td><td><b>%s</b> (%s%%)</td></tr>
                </table>
                ',
              .('Tiles'),
              tiles_n,
              tiles_status$n[5L],
              .('roof(s)'),
              .('identified'),
              .('Available for mapping'),
              tiles_status$n[1L],
              round(100L * tiles_status$n[1L] / tiles_n, 1L),
              .('Checked (Ready for validation)'),
              tiles_status$n[2L],
              round(100L * tiles_status$n[2L] / tiles_n, 1L),
              .('Validated'),
              tiles_status$n[3L],
              round(100L * tiles_status$n[3L] / tiles_n, 1L),
              .('Locked'),
              max(0L, tiles_status$n[4L] - 1L),
              round(100L * (max(0L, tiles_status$n[4L] - 1L)) / tiles_n, 1L)
            )
          )
        )
      })

      # --------------------------------------------------------------------------------------
      # Map Observers

      shiny::observeEvent(input$map_shape_click, {
        if (input$map_shape_click$group == 'polygons') {
          data$searchSwipeTile(
            map = map,
            direction = 'coordinates',
            lat = input$map_shape_click$lat,
            lon = input$map_shape_click$lng,
            map_size = input$map_size,
            token = session$token
          )
        } else if (input$map_shape_click$group == 'identify_grid') {
          cell <- as.integer(stringr::str_extract(
            input$map_shape_click$id,
            '[1-9]+'
          ))

          if (is.na(cell)) {
            data$addRoof(
              map = map,
              lng = input$map_click$lng,
              lat = input$map_click$lat
            )
          } else {
            data$addCell(map = map, token = session$token, cell = cell)
          }
        } else if (input$map_shape_click$group == 'identify_building') {
          data$removeRoof(map = map, roof_id = input$map_shape_click$id)
        }
      })

      # --------------------------------------------------------------------------------------
      # Drawing

      shiny::observeEvent(input$map_draw_start, {
        console_out('Map drawing start')
      })

      shiny::observeEvent(input$map_draw_stop, {
        console_out('Map drawing stop')
      })

      shiny::observeEvent(input$map_draw_toggle, {
        console_out('Map drawing toggle')
        if (data$step_delimit$getStatus$mode == 'swipe') {
          vars$mode_draw <- 'add_marker'
        } else {
          self$mode <- 'swipe'
        }
      })

      shiny::observeEvent(input$map_draw_new_feature, {
        console_out('Draw New Feature')

        feature <- input$map_draw_new_feature
        feature_sf <-
          st_as_sf.feature(feature)

        if (is.null(vars$polygon_identify$roofs_sf)) {
          feature_sf$identifier <- 1L
          shiny::isolate(vars$polygon_identify$roofs_sf <- feature_sf)
        } else {
          shiny::isolate(
            vars$polygon_identify$roofs_sf <- combine_list_of_sf(list(
              vars$polygon_identify$roofs_sf,
              feature_sf
            ))
          )
        }
      })

      # Edited Features
      shiny::observeEvent(input$map_draw_edited_features, {
        logDebug("Edited Features: %s", length(input$map_draw_edited_features))
      })

      # Deleted features
      shiny::observeEvent(input$map_draw_deleted_features, {
        console_out(
          'Delete Layer (%s / %s)',
          input$map_draw_deleted_features$id,
          input$map_draw_deleted_features$group
        )

        feature <- input$map_draw_deleted_features

        shiny::isolate(
          vars$polygon_identify$roofs_sf <- dplyr::filter(
            vars$polygon_identify$roofs_sf,
            X_leaflet_id != feature$properties$`_leaflet_id`
          )
        )
      })

      # We also listen for draw_all_features which is called anytime
      # features are created/edited/deleted from the map
      shiny::observeEvent(input$map_draw_all_features, {
        logDebug("All Features: %s", length(input$map_draw_all_features))
      })

      # --------------------------------------------------------------------------------------
      # Status UI

      shiny::observe({
        self$data$project_method_changed
        if (self$data$project_method %in% c('SP_QDR', 'SP_TSQ')) {
          shinyjs::hide(
            selector = sprintf(
              'li:has(a[data-value=%s])',
              'app-steps-step_identify-tab'
            )
          )
        } else {
          shinyjs::show(
            selector = sprintf(
              'li:has(a[data-value=%s])',
              'app-steps-step_identify-tab'
            )
          )
        }
      })

      output$info <- shiny::renderUI({
        shiny::tagList(
          shiny::tags$div(
            class = 'tip',
            shiny::tags$strong(..('Identify: ')),
            shiny::HTML(text = self$state$info)
          )
        )
      })

      # --------------------------------------------------------------------------------------
      # Tour

      shiny::observeEvent(input$intro_change, {
        console_out(input$intro_change)
      })

      shiny::observeEvent(input$intro_complete, {
      })

      shiny::observeEvent(input$link_guide_ui, {
        closeSidebar()

        steps <-
          tibble::tribble(
            ~element,
            ~group,
            ~tooltip,
            ~description,
            'map_zoom_in_btn',
            .('Map'),
            .('Zoom In'),
            .('Zoom in to the map'),
            'map_zoom_out_btn',
            .('Map'),
            .('Zoom Out'),
            .('Zoom out from the map'),
            'map_zoom_extent_btn',
            .('Map'),
            .('Zoom to layer\'s extent'),
            NA,
            'map .leaflet-control-fullscreen',
            .('Map'),
            .('FullScreen'),
            .('Enlarge the map to fullscreen'),
            'act_ok_btn',
            .('Delimit'),
            .('Validate modifications'),
            .('Validate the modifications done in this page'),
            'act_rollback_btn',
            .('Delimit'),
            .('Undo'),
            .('Undo the last modification'),
            'act_clear_btn',
            .('Delimit'),
            .('Reset page'),
            .(
              'Reset the whole page by clearing all outlines created or imported'
            ),
            'act_upload_btn',
            .('Draw'),
            .('Add polygon(s) layer from file'),
            .(
              'Open a dialog window to browse and upload a file from your computer containing the polygon or the outlines.'
            ),
            'act_identify_btn',
            .('?'),
            .('?'),
            .('?')
          )

        step_instructions <-
          tibble::tribble(
            ~element,
            ~instructions,
            'map_zoom_in_btn',
            .('The zooming can also be done by moving the mouse wheel.'),
            'map_zoom_out_btn',
            .('The zooming can also be done by moving the mouse wheel.'),
            'map .leaflet-control-fullscreen',
            .('Press `Esc` to exit the fullscreen.'),
            'act_upload_btn',
            .(
              'File formats accepted are zipped shape files, kml and kmz. Only files containing polygons are accepted'
            )
          )

        step_warnings <-
          tibble::tribble(
            ~element,
            ~warning,
            'act_ok_btn',
            .(
              'You cannot move to the next pages before having selected at least one outline and validated modifications.'
            ),
            'act_rollback_btn',
            .('Only the last modification can be undo.'),
            'act_clear_btn',
            .(
              'By resetting this page you also clear all items created or imported in the other pages.'
            ),
            'act_upload_btn',
            .(
              'Do not confuse polygon and line geometry types. In a GIS software lines may look like polygons. To understand the difference between line and polygon types click [here](https://www.igismap.com/gis-tutorial-basic-spatial-elements-points-lines-and-polygons/). If you have a file containing lines, you first need to convert the lines into polygons before importing the file. You can use [QGIS](https://www.qgis.org/en/site/) and follow [these instructionss](https://gis.stackexchange.com/questions/207463/converting-line-to-polygon-using-qgis) to convert the lines into polygons.'
            )
          )

        steps <- steps |>
          dplyr::left_join(step_instructions, by = 'element') |>
          dplyr::left_join(step_warnings, by = 'element') |>
          dplyr::mutate(
            element = sprintf('#mod_steps-mod_step_identify-%s', element),
            intro = sprintf('<b>%s</b>: %s.', group, tooltip),
            intro = ifelse(
              is.na(description),
              intro,
              sprintf('%s<br><br>%s', intro, description)
            ),
            intro = ifelse(
              is.na(instructions),
              intro,
              sprintf('%s<br><br>%s', intro, instructions)
            ),
            intro = ifelse(
              is.na(warning),
              intro,
              sprintf(
                '%s<br><br>%s: %s',
                intro,
                icon('alert-rhombus', size = 's'),
                warning
              )
            )
          )

        guide(steps = steps, session = application$session, ns = ns)
      })
    },
    onStateChange = function() {
      if (sum(self$data$polygons_selected$roofs_count, na.rm = TRUE) == 0L) {
        shinyjs::disable(id = 'act_download_kml')
        shinyjs::disable(id = 'act_download_shp')
      } else {
        shinyjs::enable(id = 'act_download_kml')
        shinyjs::enable(id = 'act_download_shp')
      }

      if (self$state$can_upload) {
        shinyjs::enable(id = 'act_upload_btn')
      } else {
        shinyjs::disable(id = 'act_upload_btn')
      }

      if (self$state$can_commit) {
        shinyjs::enable(id = 'act_ok_btn')
      } else {
        shinyjs::disable(id = 'act_ok_btn')
      }

      if (self$state$can_rollback) {
        shinyjs::enable(id = 'act_rollback_btn')
      } else {
        shinyjs::disable(id = 'act_rollback_btn')
      }

      if (self$state$can_clear) {
        shinyjs::enable(id = 'act_clear_btn')
      } else {
        shinyjs::disable(id = 'act_clear_btn')
      }

      if (self$state$mode == 'select') {
        shinyjs::disable(id = 'act_check')
        shinyjs::disable(id = 'act_valid')
      } else {
        shinyjs::enable(id = 'act_check')
        shinyjs::enable(id = 'act_valid')
      }

      super$onStateChange()
    },
    onMapZoom = function() {
      zoom <- floor(self$map_zoom)
      if (zoom > 18L) {
        map |>
          leaflet::showGroup('identify_buildings') |>
          leaflet::hideGroup('grid_status')
      } else {
        map |>
          leaflet::hideGroup('identify_buildings') |>
          leaflet::showGroup('grid_status')
      }
      if (data$grid_status_invalidated) {
        invalidateGridIdentifyStatus()
      }
    },
    renderMap = function() {
      shiny::insertUI(
        selector = sprintf('#%s', ns('pnl_settings')),
        where = 'beforeEnd',
        ui = shiny::tagList(
          shiny::br(),
          shiny::h6('Roofs:'),
          p(shiny::sliderInput(
            inputId = ns('sli_circle_radius'),
            label = .('Circle Radius (meter):'),
            min = 1L,
            max = 5L,
            value = data$settings$getValue('sli_circle_radius'),
            step = 0.5
          )),
          p(shiny::sliderInput(
            inputId = ns('sli_circle_opacity'),
            label = .('Circle Opacity:'),
            min = 0L,
            max = 100L,
            value = data$settings$getValue('sli_circle_opacity'),
            step = 5L
          )),
          p(shinyWidgets::colorSelectorInput(
            inputId = ns('csi_circle_color'),
            label = .('Color:'),
            choices = grDevices::rainbow(12L),
            ncol = 12L,
            selected = data$settings$getValue('csi_circle_color')
          ))
        )
      )
      super$renderMap()
      invalidateGridIdentifyStatus(force = TRUE)
    },
    checkState = function(value) {
      polygons_selected_count <- data$polygons_selected_count

      value$can_commit <- value$modified & polygons_selected_count > 0L
      value$can_rollback <- value$modified & polygons_selected_count > 0L
      value$can_clear <- !value$modified
      value$can_upload <- !value$modified

      if (self$state$mode == 'select') {
        # if(identify_pct == 0) {
        value$info_text <- sprintf(
          ..(
            'Outlines is divided into task tiles. Search the first tile and begin to draw roof (%s). Beforehand, it is possible to upload data from external source (%s).'
          ),
          as.character(icon('home-edit-polygon')),
          as.character(icon('layers-plus'))
        )
        # }
      } else {
        # if(identify_pct == 0) {
        value$info_text <- sprintf(
          ..(
            'Click on the map to add or delete roof. At the end, complete the task (<b>[SPACE]</b> or %s) or valid it (<b>[ENTER]</b>). When you want, you can change tile task with arrow keys.'
          ),
          as.character(icon('home-edit-polygon'))
        )
        # }
      }

      value
    }
  )
)
