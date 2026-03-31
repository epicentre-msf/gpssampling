StepDelimit <- R6::R6Class(
  classname = 'StepDelimit',
  inherit = Step,
  portable = FALSE,
  active = list(
    tab_title = function() {
      .('Delimit')
    },
    lbl_cancel_info = function() {
      ..(
        'If <b>yes</b>, all depend work will be deleted <b>permanently</b>. You can\'t undo this action.'
      )
    },
    lbl_ok = function() {
      ..('Are you sure you want to permanently save this polygon layer.')
    },
    lbl_ok_info = function() {
      ..('Saved content will be overwritten. You can\'t undo this action.')
    }
  ),
  public = list(
    initialize = function(index = 1L, steps = NULL) {
      super$initialize(id = 'step_delimit', index = index, steps = steps)
    },
    invalidateStatus = function() {
    },
    commit = function() {
      data$save(delimit = TRUE)
    },
    rollback = function() {
      data$rollback(delimit = TRUE)
    },
    clear = function() {
      data$clear(delimit = TRUE)
    },
    invalidatePolygons = function() {
      data$displayPolygons(map)
      super$invalidatePolygons()
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
            title = .('STEP 1: DELIMIT'),
            id = ns('pnl_step'),
            icon = icon('menu', size = 's'),
            shiny::br(),
            p(.(
              '
            The page allows to set the geographical boundaries
            within which the population is to be estimated.'
            )),
            p(.(
              'The first step is to define one or more area of interest for yours samples.'
            )),
            p(.('You have several ways to proceed.')),
            shiny::h6('Option 1:'),
            p(.('You can draw the polygons directly.')),
            p(
              button(
                inputId = ns('act_draw_polygon_side'),
                icon = 'shape-square-plus',
                label = .('Draw a new polygon')
              )
            ),
            shiny::br(),
            shiny::h6('Option 2:'),
            p(.(
              'You can either import the polygons from a file in your computer.'
            )),
            p(
              button(
                inputId = ns('act_upload_side'),
                icon = 'layers-plus',
                label = .('Add a polygon layer...')
              )
            ),
            p(.('Then save the polygon layer to move on to the next step...'))
          ),
          sidebar_pane(
            title = .('EXPORT'),
            id = ns('pnl_data'),
            icon = icon('download', size = 's'),
            shiny::br(),
            shiny::h6('Export:'),
            p(.('Export the current polygon layer.'))
          ),
          sidebar_pane(
            title = .('SETTINGS'),
            id = ns('pnl_settings'),
            icon = icon('cog-outline', size = 's'),
            shiny::br(),
            shiny::h6('Guide:'),
            p(.('Add an extra layer to help you find your way.')),
            p(
              button(
                inputId = ns('act_guide_polygon'),
                icon = 'layers-outline',
                label = .('Add Extra Polygon Layer')
              ),
              shinyjs::disabled(
                button(
                  inputId = ns('act_guide_polygon_clear'),
                  icon = 'layers-off-outline',
                  label = .('Clear')
                )
              )
            ),
            p(
              button(
                inputId = ns('act_guide_point'),
                icon = 'layers-outline',
                label = .('Add Extra Point Layer')
              ),
              shinyjs::disabled(
                button(
                  inputId = ns('act_guide_point_clear'),
                  icon = 'layers-off-outline',
                  label = .('Clear')
                )
              )
            ),
            shiny::h6('Basemap (Offline):')
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
                inputId = ns('link_guide'),
                label = .('Instruction: How add a new polygon.')
              )),
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
      lf <- super$getOutputMap(session)

      lf <-
        leaflet.extras::addSearchOSM(
          lf,
          options = leaflet.extras::searchOptions(
            autoType = TRUE,
            autoResize = TRUE,
            # autoCollapse = TRUE,
            firstTipSubmit = TRUE,
            hideMarkerOnCollapse = TRUE,
            minLength = 2L,
            position = 'bottomleft',
            textErr = .('Location Not Found'),
            textCancel = .('Cancel'),
            # textPlaceholder = .('Search...'),
            tipAutoSubmit = TRUE
          )
        )

      lf <- leaflet::addEasyButtonBar(
        lf,
        id = ns('bar_layers'),
        position = 'topleft',
        easyButtonShiny(
          inputId = ns('act_upload'),
          icon = icon('layers-plus'),
          title = ..('Add a polygon layer...')
        )
      )

      lf <- leaflet::addEasyButtonBar(
        lf,
        id = ns('bar_draw_2'),
        position = 'topleft',
        easyButtonShiny(
          inputId = ns('act_draw_polygon'),
          icon = icon('shape-square-plus'),
          title = ..('Draw a new polygon')
        ),
        easyButtonShiny(
          inputId = ns('act_edit_polygon'),
          icon = icon('shape-square-edit'),
          title = ..('Edit a polygon')
        ),
        easyButtonShiny(
          inputId = ns('act_remove_polygon'),
          icon = icon('shape-square-remove'),
          title = ..('Remove polygon(s)')
        )
      )

      lf <- leaflet::addEasyButtonBar(
        lf,
        id = ns('bar_draw_3'),
        position = 'topleft',
        easyButtonShiny(
          inputId = ns('act_cut_polygon'),
          icon = icon('scissors-cutting'),
          title = ..('Cut a polygon')
        )
      )

      lf <- leaflet::addEasyButtonBar(
        lf,
        id = ns('bar_tool'),
        position = 'topleft',
        easyButtonShiny(
          inputId = ns('act_select_invert'),
          icon = icon('yin-yang'),
          title = ..('Inverse Selection')
        )
      )

      # map <- leaflet::addEasyButtonBar(map,
      #   id = ns('bar_draw_4'),
      #   position = 'topleft',
      #   easyButtonShiny(inputId = session$ns('act_group'), icon = icon('select-all'), title = .('Group polygon(s)')),
      #   easyButtonShiny(inputId = session$ns('act_ungroup'), icon = icon('select-group'), title = .('Ungroup polygon'))
      # )

      lf <- pm_attach_dependencies(
        lf,
        targetGroup = 'polygons',
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
          )
        )
      )

      lf
    },
    getServer = function(input, output, session) {
      super$getServer(input, output, session)

      # --------------------------------------------------------------------------------------
      # Actions

      shiny::observeEvent(input$act_cut_polygon, {
        pm_toggle_draw_mode(map, shape = 'Line')
      })

      shiny::observeEvent(input$act_draw_polygon_side, {
        pm_toggle_draw_mode(map, shape = 'Polygon')
      })

      shiny::observeEvent(input$act_draw_polygon, {
        pm_toggle_draw_mode(map, shape = 'Polygon')
      })

      shiny::observeEvent(input$act_edit_polygon, {
        pm_toggle_edit_mode(map, targetGroup = 'polygons')
      })

      shiny::observeEvent(input$act_remove_polygon, {
        pm_toggle_removal_mode(map, targetGroup = 'polygons')
      })

      shiny::observeEvent(input$act_select_invert, {
        data$invertSelection(map)
      })

      shiny::observeEvent(input$act_group, {
        if (state$mode == 'group_polygon') {
          self$mode <- 'select'
        } else {
          self$mode <- 'group_polygon'
        }
      })

      shiny::observeEvent(input$act_ungroup, {
        # if (getStatus$mode == 'ungroup_polygon') {
        #   self$mode <- 'select'
        # } else {
        #   self$mode <- 'ungroup_polygon'
        # }
        # selection <- getPolygon() |> dplyr::filter(selected)
        # selection <- selection[sf::st_is(selection, 'MULTIPOLYGON'), ]
        # if (nrow(selection) == 0) {
        #   return()
        # }
        # polygons_ungroup <- sf::st_cast(selection, 'POLYGON')
        # polygons_ungroup$id <- as.character(max(as.integer(getPolygon()$id)) + seq_len(nrow(polygons_ungroup)))
        # for (i in seq_len(nrow(selection))) {
        #   leaflet::removeShape(map, layerId = selection$id[i])
        # }
        # getPolygon() %<>% dplyr::filter(!(id %in% selection$id))
        # getPolygon() %<>% dplyr::bind_rows(polygons_ungroup)
      })

      # --------------------------------------------------------------------------------------
      # Download Actions

      shiny::observeEvent(input$pnl_data_open, {
        shiny::insertUI(
          selector = sprintf('#%s', ns('pnl_data')),
          where = 'beforeEnd',
          ui = disabledConditional(
            condition = data$polygons_count == 0L,
            ui = shiny::tagList(
              buttonDownload(outputId = ns('act_download_shp'), label = 'SHP'),
              buttonDownload(outputId = ns('act_download_kml'), label = 'KML')
            )
          )
        )
      })

      shiny::observeEvent(input$pnl_settings_open, {
        shiny::insertUI(
          selector = sprintf('#%s', ns('pnl_settings')),
          where = 'beforeEnd',
          ui = shiny::tagList(
            p(.('Export')),
            p(
              buttonDownload(
                outputId = ns('act_download_tiles'),
                label = .('Export')
              ),
              button(
                inputId = ns('act_basemap'),
                icon = 'map-search-outline',
                label = .('Scan...')
              )
            ),
            p(.('Import')),
            p(
              fileInput(
                inputId = ns('file'),
                label = NULL,
                buttonLabel = ..('Import...'),
                placeholder = ..('Select basemap file...'),
                width = '100%',
                accept = '.zip'
              )
            )
          )
        )
      })

      output$act_download_kml <- shiny::downloadHandler(
        filename = function() {
          sprintf('polygons - %s.kml', Sys.Date())
        },
        content = function(file) {
          sf <-
            dplyr::transmute(data$polygons, name = id, description = label)
          writeSpatialLayer(
            sf = sf,
            file = file,
            layer = sprintf('polygons - %s', Sys.Date()),
            fieldname = 'lf_name'
          )
        }
      )

      output$act_download_shp <- shiny::downloadHandler(
        filename = function() {
          sprintf('polygons - %s.zip', Sys.Date())
        },
        content = function(file) {
          sf <-
            dplyr::transmute(data$polygons, name = id, description = label)
          writeSpatialLayer(
            sf = sf,
            file = file,
            layer = sprintf('polygons - %s', Sys.Date()),
            fieldname = 'lf_name'
          )
        }
      )

      output$act_download_tiles <- shiny::downloadHandler(
        filename = function() {
          sprintf('tiles - %s.zip', Sys.Date())
        },
        content = function(file) {
          dp <- getDirApp('imageries')
          fp <- fs::file_temp(ext = 'zip')
          if (fs::file_exists(fp)) {
            fs::file_delete(path = fp)
          }
          zip::zip(
            zipfile = fp,
            files = fs::path_file(fs::dir_ls(path = dp)),
            root = dp
          )
          fs::file_copy(path = fp, new_path = file, overwrite = TRUE)
        }
      )

      # --------------------------------------------------------------------------------------
      # Upload Polygon

      dlg_upload <- ModalDialogTabUploadDelimit$new(
        id = 'mod_1',
        lbl_title = .('Add a polygon layer...'),
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
        data$polygons <- dlg_upload$out$sf
      })

      # --------------------------------------------------------------------------------------
      # Upload Guide Polygon

      dlg_guide_polygon <- ModalDialogTabUploadGuide$new(
        id = 'mod_guide_polygon',
        lbl_title = .('Add a polygon guide layer...'),
        lbl_ok = .('Add'),
        type = 'polygon',
        parent = self
      )

      dlg_guide_polygon$bind()

      shiny::observeEvent(input$act_guide_polygon_clear, {
        data$guide_polygon <- NULL
        self$state <- utils::modifyList(
          self$state,
          list(modified = TRUE, void = rnorm(1L))
        )
      })

      shiny::observeEvent(input$act_guide_polygon, {
        dlg_guide_polygon$show(ns = ns)
      })

      shiny::observeEvent(dlg_guide_polygon$ok, ignoreInit = TRUE, {
        data$guide_polygon <- dlg_guide_polygon$out$sf
        self$state <- utils::modifyList(
          self$state,
          list(modified = TRUE, void = rnorm(1L))
        )
      })

      # --------------------------------------------------------------------------------------
      # Upload Guide Point

      dlg_guide_point <- ModalDialogTabUploadGuide$new(
        id = 'mod_guide_point',
        lbl_title = .('Add a point guide layer...'),
        lbl_ok = .('Add'),
        type = 'point',
        parent = self
      )

      dlg_guide_point$bind()

      shiny::observeEvent(input$act_guide_point_clear, {
        data$guide_point <- NULL
        self$state <- utils::modifyList(
          self$state,
          list(modified = TRUE, void = rnorm(1L))
        )
      })

      shiny::observeEvent(input$act_guide_point, {
        dlg_guide_point$show(ns = ns)
      })

      shiny::observeEvent(dlg_guide_point$ok, ignoreInit = TRUE, {
        data$guide_point <- dlg_guide_point$out$sf
        self$state <- utils::modifyList(
          self$state,
          list(modified = TRUE, void = rnorm(1L))
        )
      })

      # --------------------------------------------------------------------------------------
      # Downloa&d Basemap

      dlg_offline <- ModalDialogTabOffline$new(
        id = 'mod_offline',
        lbl_title = .('Scan Basemap...'),
        parent = self
      )

      dlg_offline$bind()

      shiny::observeEvent(input$act_basemap, {
        dlg_offline$show(ns = ns)
      })

      shiny::observeEvent(dlg_offline$ok, ignoreInit = TRUE, {
        shiny::removeModal()
      })

      # --------------------------------------------------------------------------------------
      # Map Observers

      shiny::observeEvent(input$map_shape_click, {
        if (input$map_shape_click$group == 'extent') {
          return()
        }

        if (state$mode == 'select') {
          data$togglePolygonSelected(map, id = input$map_shape_click$id)
        } else {
          map |>
            pm_edit_feature(
              targetGroup = 'polygons',
              targetId = input$map_shape_click$id,
              editOptions = leafpm::pmEditOptions(
                snappable = TRUE,
                snapDistance = 20L,
                pinning = TRUE,
                allowSelfIntersection = FALSE,
                allowSelfIntersectionEdit = FALSE,
                draggable = TRUE,
                preventMarkerRemoval = FALSE,
                preventVertexEdit = FALSE,
                limitMarkersToCount = -1L
              )
            )
        }

        # if (state$mode == 'none') {

        #   if (vars$mode_draw == 'group_polygon') {

        #     if (is.null(vars$polygon_selected)) {

        #       vars$polygon_selected <- input$map_shape_click$id

        #     } else {

        #     }

        #   } else {

        #     i <- match(input$map_shape_click$id, getPolygon()$id)

        #     getPolygon()$selected[i] <- !getPolygon()$selected[i]
        #     vars$stratum_invalidate_selection <- vars$stratum_invalidate_selection + 1
        #     vars$status <- 'stratum_changed'

        #   }

        # } else {

        #   if (vars$mode_draw == 'group_polygon') {
        #     vars$polygon_selected <- getPolygon()$id
        #   } else {
        #     vars$stratum_user_selected <- input$map_shape_click$id
        #   }
        # }
      })

      # --------------------------------------------------------------------------------------
      # Drawing

      shiny::observeEvent(input$map_draw_toggle, {
        if (input$map_draw_toggle$enabled) {
          if (input$map_draw_toggle$shape == 'Polygon') {
            self$mode <- 'draw_polygon'
          } else {
            self$mode <- 'cut_polygon'
          }
        } else {
          self$mode <- 'select'
        }
      })

      shiny::observeEvent(input$map_remove_toggle, {
        if (input$map_remove_toggle$enabled) {
          self$mode <- 'remove_polygon'
        } else {
          self$mode <- 'select'
        }
      })

      shiny::observeEvent(input$map_edit_toggle, {
        if (state$mode == 'select') {
          self$mode <- 'edit_polygon'
        } else {
          self$mode <- 'select'
        }
      })

      shiny::observeEvent(input$map_draw_edited_features, {
        data$editPolygon(map, feature = input$map_draw_edited_features)
      })

      shiny::observeEvent(input$map_draw_new_feature, {
        data$drawNewFeature(map, feature = input$map_draw_new_feature)
      })

      shiny::observeEvent(input$map_draw_deleted_features, {
        data$deletePolygon(map, id = input$map_draw_deleted_features$id)
      })

      # --------------------------------------------------------------------------------------
      # Status UI

      output$info <- shiny::renderUI({
        shiny::tagList(
          shiny::tags$div(
            class = 'tip',
            shiny::tags$strong(..('Delimit: ')),
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

      shiny::observeEvent(input$link_guide, {
        steps <-
          tibble::tribble(
            ~tab,
            ~view,
            ~selector,
            'steps',
            'step_delimit',
            'map',
            'steps',
            'step_delimit',
            'act_draw_polygon_btn',
            'steps',
            'step_delimit',
            'act_edit_polygon_btn',
            'steps',
            'step_delimit',
            'act_remove_polygon_btn',
            'steps',
            'step_delimit',
            'act_cut_polygon_btn',
            'steps',
            'step_delimit',
            'act_select_invert_btn',
            'steps',
            'step_delimit',
            'act_group_btn',
            'steps',
            'step_delimit',
            'act_ok_btn',
            'steps',
            'step_delimit',
            'act_clear_btn'
          )

        steps$intro <- c(
          ..(
            'STEP 2: Set polygon(s)<br>Use this screen to add one or more polygons area.<img src=\'assets/img/stratify.gif\' width = "300px"/></div>'
          ),
          ..(
            '<b>Stratify</b>: Draw. <br><br>Click to draw a polygon representing the polygons of interest in the map frame.'
          ),
          ..('<b>Stratify</b>: (dev).'),
          ..('<b>Stratify</b>: (dev).'),
          ..('<b>Stratify</b>: (dev).'),
          ..('<b>Stratify</b>: (dev).'),
          ..('<b>Stratify</b>: (dev).'),
          ..('<b>Extent</b>: Save. <br><br>Click to save the polygon layer.'),
          ..(
            '<b>Extent</b>: Rollback or Clear. <br><br>Click to rollback or delete the current polygon layer.'
          )
        )

        steps <- steps |>
          dplyr::transmute(
            element = sprintf('#%s', ns(selector)),
            intro = intro
          )

        private$guide(steps = steps)
      })

      shiny::observeEvent(input$link_guide_ui, {
        steps <-
          tibble::tribble(
            ~element,
            ~group,
            ~tooltip,
            ~description,
            'map_zoom_in_btn',
            ..('Map'),
            ..('Zoom In'),
            ..('Zoom in to the map'),
            'map_zoom_out_btn',
            ..('Map'),
            ..('Zoom Out'),
            ..('Zoom out from the map'),
            'map_zoom_extent_btn',
            ..('Map'),
            ..('Zoom to layer\'s extent'),
            NA,
            'map .leaflet-control-fullscreen',
            ..('Map'),
            ..('FullScreen'),
            ..('Enlarge the map to fullscreen'),
            'act_ok_btn',
            ..('Delimit'),
            ..('Validate modifications'),
            ..('Validate the modifications done in this page'),
            'act_rollback_btn',
            ..('Delimit'),
            ..('Undo'),
            ..('Undo the last modification'),
            'act_clear_btn',
            ..('Delimit'),
            ..('Reset page'),
            ..(
              'Reset the whole page by clearing all outlines created or imported'
            ),
            'act_upload_btn',
            ..('Draw'),
            ..('Add polygon(s) layer from file'),
            ..(
              'Open a dialog window to browse and upload a file from your computer containing the polygon or the outlines.'
            ),
            'act_draw_polygon_btn',
            ..('Draw'),
            ..('Draw a new polygon'),
            ..('Draw new outlines straight in this page'),
            'act_edit_polygon_btn',
            ..('Draw'),
            ..('Edit an polygon'),
            ..('Edit the outlines already drawn or imported'),
            'act_remove_polygon_btn',
            ..('Draw'),
            ..('Remove an polygon'),
            ..('Remove outlines already drawn or imported'),
            'act_cut_polygon_btn',
            ..('Draw'),
            ..('Split an polygon'),
            ..('Draw a line passing throughout an already existing polygon')
          )

        step_instructions <-
          tibble::tribble(
            ~element,
            ~instructions,
            'map_zoom_in_btn',
            ..('The zooming can also be done by moving the mouse wheel.'),
            'map_zoom_out_btn',
            ..('The zooming can also be done by moving the mouse wheel.'),
            'map .leaflet-control-fullscreen',
            ..('Press `Esc` to exit the fullscreen.'),
            'act_upload_btn',
            ..(
              'File formats accepted are zipped shape files, kml and kmz. Only files containing polygons are accepted'
            ),
            'act_draw_polygon_btn',
            ..(
              'Move the satellite image so that the page displays the area of interest. After clicking on the %s icon, move the pointer to the image just outside the area of interest and left-click of the mouse to start drawing the polygon. Move the mouse clockwise and left-click to mark each corner of the polygon. Continue moving and left-clicking until you reach to where you started. Left-click again on the first marker to close the polygon. If you need to move the background image, hold the left-click of the mouse and move the pointer. '
            ),
            'act_edit_polygon_btn',
            ..(
              'After clicking on the %s icon, select first one polygon and then move the corners. Re-click on the icon to exit the editing.'
            ),
            'act_remove_polygon_btn',
            ..(
              'After clicking on the %s icon, click on one polygon to remove it. Re-click on the icon to exit the remove. '
            ),
            'act_cut_polygon_btn',
            ..(
              'Click on any existing markers to end the line and execute the split. Re-click on the icon to exit the split.'
            )
          )

        step_warnings <-
          tibble::tribble(
            ~element,
            ~warning,
            'act_ok_btn',
            ..(
              'You cannot move to the next pages before having selected at least one outline and validated modifications.'
            ),
            'act_rollback_btn',
            ..('Only the last modification can be undo.'),
            'act_clear_btn',
            ..(
              'By resetting this page you also clear all items created or imported in the other pages.'
            ),
            'act_upload_btn',
            ..(
              'Do not confuse polygon and line geometry types. In a GIS software lines may look like polygons. To understand the difference between line and polygon types click [here](https://www.igismap.com/gis-tutorial-basic-spatial-elements-points-lines-and-polygons/). If you have a file containing lines, you first need to convert the lines into polygons before importing the file. You can use [QGIS](https://www.qgis.org/en/site/) and follow [these instructionss](https://gis.stackexchange.com/questions/207463/converting-line-to-polygon-using-qgis) to convert the lines into polygons.'
            )
          )

        steps <- steps |>
          dplyr::left_join(step_instructions, by = 'element') |>
          dplyr::left_join(step_warnings, by = 'element') |>
          dplyr::mutate(
            element = sprintf('#%s', ns(element)),
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

        private$guide(steps = steps)
      })
    },
    checkState = function(value) {
      polygons_count <- data$polygons_count
      polygons_selected_count <- data$polygons_selected_count
      polygons_count_multi <- data$polygons_count_multi

      value$can_commit <- value$mode == 'select' & value$modified
      value$can_rollback <- value$mode == 'select' & value$modified
      value$can_clear <- value$mode == 'select' & !value$modified
      value$can_upload <- value$mode == 'select' & !value$modified

      value$can_draw_polygon <- value$mode == 'select'
      value$can_edit_polygon <- value$mode == 'select' & polygons_count > 0L
      value$can_cut <- value$mode == 'select' & polygons_count > 0L
      value$can_remove <- value$mode == 'select' & polygons_count > 0L
      value$can_group <- value$mode == 'select' & polygons_count > 1L
      value$can_ungroup <- value$mode == 'select' & polygons_count_multi > 0L

      if (value$mode == 'select') {
        if (value$can_commit | value$can_rollback) {
          value$info <- sprintf(
            ..(
              'The polygon layer is modified. Confirm and save (%s) the polygon(s) or rollback (%s) the changes.'
            ),
            as.character(icon('check')),
            as.character(icon('delete-outline'))
          )
        } else {
          if (polygons_count == 0L) {
            value$info <- sprintf(
              ..(
                'Sampling can be subdivided into one or more polygons. Add a polygon layer (%s). Use the editing tools (%s) to modify the layer. Finally click on the map to select the polygon(s).'
              ),
              as.character(icon('layers-plus')),
              as.character(icon('shape-square-plus'))
            )

            # } else if if (polygons_selected_count == 0) {

            #   value$info <- sprintf(
            #     .('The polygons layer is defined but empty. Add a polygons layer (%s) or/and use the editing tools (%s) to modify the layer.'),
            #     as.character(icon('layers-plus')),
            #     as.character(icon('shape-square-plus'))
            #   )
          } else if (polygons_selected_count == 0L) {
            value$info <- sprintf(
              ..(
                '<span class="blink">The polygon layer is defined but none polygon are selected. Click on the layer to select polygon(s).</span>'
              )
            )
          } else {
            value$info <- sprintf(
              ..(
                '%s on %s polygon(s) are defined. You can go to the next step or continue to edit the polygon layer.'
              ),
              polygons_selected_count,
              polygons_count
            )
          }
        }
      } else if (value$mode == 'draw_polygon') {
        value$info <- sprintf(
          ..(
            'Click on the map to draw a new polygon. Click (%s) again to cancel.'
          ),
          as.character(icon('shape-square-plus'))
        )
      } else if (value$mode == 'edit_polygon') {
        value$info <- sprintf(
          ..(
            'Click on the map to select a polygon and edit the nodes. Click (%s) again to finish.'
          ),
          as.character(icon('shape-square-edit'))
        )
      } else if (value$mode == 'cut_polygon') {
        value$info <- sprintf(
          ..(
            'Click on the map to cut one or several polygon(s) with a line. Click (%s) again to cancel.'
          ),
          as.character(icon('shape-square-remove'))
        )
      } else if (value$mode == 'remove_polygon') {
        value$info <- sprintf(
          ..(
            'Click on the map to remove a polygon. Click (%s) again to cancel.'
          ),
          as.character(icon('scissors-cutting'))
        )
      } else if (value$mode == 'group_polygon') {
        value$info <- sprintf(
          ..(
            'Click on the polygons to union them. Click (%s) again to cancel.'
          ),
          as.character(icon('scissors-cutting'))
        )
      }

      value
    },
    onStateChange = function() {
      shinyjs::toggleClass(
        selector = 'i.mdi-shape-square-plus',
        class = 'mdi-selected',
        condition = state$mode == 'draw_polygon'
      )
      shinyjs::toggleClass(
        selector = 'i.mdi-shape-square-edit',
        class = 'mdi-selected',
        condition = state$mode == 'edit_polygon'
      )
      shinyjs::toggleClass(
        selector = 'i.mdi-shape-square-remove',
        class = 'mdi-selected',
        condition = state$mode == 'remove_polygon'
      )
      shinyjs::toggleClass(
        selector = 'i.mdi-scissors-cutting',
        class = 'mdi-selected',
        condition = state$mode == 'cut_polygon'
      )
      shinyjs::toggleClass(
        selector = 'i.mdi-select-all',
        class = 'mdi-selected',
        condition = state$mode == 'group_polygon'
      )
      shinyjs::toggleClass(
        selector = 'i.mdi-select-group',
        class = 'mdi-selected',
        condition = state$mode == 'ungroup_polygon'
      )

      if (data$polygons_count == 0L) {
        shinyjs::disable(id = 'act_download_kml')
        shinyjs::disable(id = 'act_download_shp')
      } else {
        shinyjs::enable(id = 'act_download_kml')
        shinyjs::enable(id = 'act_download_shp')
      }

      if (is.null(data$guide_polygon)) {
        shinyjs::enable(id = 'act_guide_polygon')
        shinyjs::disable(id = 'act_guide_polygon_clear')
      } else {
        shinyjs::disable(id = 'act_guide_polygon')
        shinyjs::enable(id = 'act_guide_polygon_clear')
      }

      if (is.null(data$guide_point)) {
        shinyjs::enable(id = 'act_guide_point')
        shinyjs::disable(id = 'act_guide_point_clear')
      } else {
        shinyjs::disable(id = 'act_guide_point')
        shinyjs::enable(id = 'act_guide_point_clear')
      }

      if (is.null(data$guide_point)) {
        shinyjs::enable(id = 'act_guide_polygon_point')
        shinyjs::disable(id = 'act_guide_polygon_point_clear')
      } else {
        shinyjs::disable(id = 'act_guide_polygon_point')
        shinyjs::enable(id = 'act_guide_polygon_point_clear')
      }

      if (state$can_upload) {
        shinyjs::enable(id = 'act_upload_btn')
      } else {
        shinyjs::disable(id = 'act_upload_btn')
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

      if (state$can_draw_polygon | state$mode == 'draw_polygon') {
        shinyjs::enable(id = 'act_draw_polygon_btn')
      } else {
        shinyjs::disable(id = 'act_draw_polygon_btn')
      }

      if (state$can_edit_polygon | state$mode == 'edit_polygon') {
        shinyjs::enable(id = 'act_edit_polygon_btn')
        shinyjs::enable(id = 'act_select_invert_btn')
      } else {
        shinyjs::disable(id = 'act_edit_polygon_btn')
        shinyjs::disable(id = 'act_select_invert_btn')
      }

      if (state$can_remove | state$mode == 'remove_polygon') {
        shinyjs::enable(id = 'act_remove_polygon_btn')
      } else {
        shinyjs::disable(id = 'act_remove_polygon_btn')
      }

      if (state$can_cut | state$mode == 'cut_polygon') {
        shinyjs::enable(id = 'act_cut_polygon_btn')
      } else {
        shinyjs::disable(id = 'act_cut_polygon_btn')
      }

      if (state$can_group | state$mode == 'group_polygon') {
        shinyjs::enable(id = 'act_group_btn')
      } else {
        shinyjs::disable(id = 'act_group_btn')
      }

      if (state$can_ungroup | state$mode == 'ungroup_polygon') {
        shinyjs::enable(id = 'act_ungroup_btn')
      } else {
        shinyjs::disable(id = 'act_ungroup_btn')
      }

      super$onStateChange()
    }
  )
)
