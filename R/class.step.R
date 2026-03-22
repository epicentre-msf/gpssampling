Step <- R6::R6Class(
  classname = 'Step',
  inherit = GpsSamplerModule,
  portable = FALSE,
  active = list(
    steps = function() {
      parent
    },
    tab_title = function() {
      ..('Step')
    },
    lbl_cancel = function() {
      ..('Are you sure you want to permanently delete this step?')
    },
    lbl_cancel_info = function() {
      ..(
        'If <b>yes</b>, all depend work will be deleted <b>permanently</b>. You can\'t undo this action.'
      )
    },
    lbl_ok = function() {
      ..('Are you sure you want to permanently save this step.')
    },
    lbl_ok_info = function() {
      ..('Saved content will be overwritten. You can\'t undo this action.')
    },
    map = function() {
      private$.map
    },
    map_changed = function() {
      private$.map_changed
    },
    map_zoom = function(value) {
      if (missing(value)) {
        return(private$.map_zoom)
      } else {
        if (!identical(private$.map_zoom, value)) {
          private$.map_zoom <- value
          private$onMapZoom()
        }
      }
    },
    mode = function(value) {
      if (missing(value)) {
        return(state$mode)
      } else {
        if (!identical(private$.state$mode, value)) {
          private$.state$mode <- value
          private$.state <- checkState(private$.state)
          private$.state_trigger$trigger()
        }
      }
    },
    state = function(value) {
      if (missing(value)) {
        private$.state_trigger$depend()
        return(private$.state)
      } else {
        private$.state <- checkState(value)
        private$.state_trigger$trigger()
      }
    }
  ),
  public = list(
    initialize = function(id = 'mod', index = 1L, steps = NULL) {
      super$initialize(id = id, parent = steps)

      private$.index <- index

      private$.state$can_commit <- FALSE
      private$.state$can_rollback <- FALSE
      private$.state$can_clear <- TRUE
      private$.state$modified <- FALSE
      private$.state$mode <- 'select'
      private$.state_trigger <- reactiveTrigger()

      private$.map_basemap <- 'sat.google'
      private$.map_changed <- shiny::reactiveVal(0L)

      private$.map_vars <- shiny::reactiveValues()
      private$.map_vars$map_bounds <- list(
        east = 21.17,
        north = 92.14,
        west = 21.19,
        south = 92.16
      )

      private$.tab_icon <- sprintf('numeric-%s-circle-outline', private$.index)
    },
    invalidateState = function() {
      self$state <- utils::modifyList(state, list(void = rnorm(1L)))
    },
    invalidatePolygons = function() {
      fitToSpatialFeatureBounds(map, data$polygons_selected)
    },
    invalidateGuidePolygon = function() {
      data$displayGuidePolygon(map)
    },
    invalidateGuidePoint = function() {
      data$displayGuidePoint(map)
    },
    commit = function() {
    },
    clear = function() {
    },
    rollback = function() {
    },
    getUI = function(ns = shiny::NS(NULL)) {
      super$getUI(ns = ns)

      fillTabPanel(
        title = shiny::uiOutput(self$ns('title_panel'), inline = TRUE),
        value = self$ns('tab'),
        icon = icon(private$.tab_icon, size = NULL),
        shiny::fillCol(
          flex = c(NA, 1L),
          shiny::wellPanel(
            shiny::uiOutput(outputId = self$ns('info'))
          ),
          shiny::fillRow(
            flex = c(NA, 1L, NA),
            private$getUISideBar(),
            private$getUIMap(),
            private$getUISidePanel()
          )
        )
      )
    }
  ),
  private = list(
    .index = NULL,
    .map = NULL,
    .map_changed = TRUE,
    .map_vars = NULL,
    .map_basemap = NULL,
    .map_zoom = NULL,
    .map_zoom_max = 21L,
    .state = NULL,
    .state_trigger = NULL,
    .tab_icon = 'numeric-1-circle-outline',
    closeSidebar = function() {
      closeSidebar(self$map)
    },
    getUISidePanel = function() {
      NULL
    },
    getUISideBar = function() {
      NULL
    },
    getUIMap = function() {
      leaflet::leafletOutput(
        outputId = self$ns('map'),
        width = 'auto',
        height = '100%'
      )
    },
    getOutputMap = function(session) {
      lf <- leaflet(
        session,
        fullscreen = TRUE,
        fullscreen_pos = 'topright',
        inputId = 'map',
        prefer_canvas = FALSE,
        searchbar = FALSE,
        zoom = TRUE,
        zoom_max = private$.map_zoom_max,
        zoom_position = 'topright'
      )

      lf <- setBasemap(lf, private$.map_basemap)

      lf <- lf |>
        addStyleFast() |>
        addEasyToolbar() |>
        addSidebar(
          options = list(
            container = self$ns('sidebar'),
            position = 'left'
          )
        )

      lf <- leaflet::addEasyButtonBar(
        lf,
        id = ns('bar_ok'),
        position = 'topleft',
        easyButtonShiny(
          inputId = ns('act_ok'),
          icon = icon('check-bold'),
          title = ..('Save')
        ),
        easyButtonShiny(
          inputId = ns('act_rollback'),
          icon = icon('undo'),
          title = ..('Undo')
        ),
        easyButtonShiny(
          inputId = ns('act_clear'),
          icon = icon('delete-outline'),
          title = ..('Reset')
        )
      )

      lf <- lf |>
        leaflet::addMapPane('polygon_1', zIndex = 310L) |>
        leaflet::addMapPane('polygon_2', zIndex = 320L) |>
        leaflet::addMapPane('marker', zIndex = 510L)

      lf <-
        leafem::addMouseCoordinates(lf)

      lf
    },
    getOutputTable = function(session) {
      tbl <-
        rhandsontable::rhandsontable(
          data = tibble::tibble(
            id_n = integer(),
            id_user = character(),
            id_key = integer(),
            id_key_calc = integer(),
            id_void = integer(),
            status = character(),
            d1 = double(),
            d2 = double(),
            pop_u5_1 = integer(),
            pop_a5_1 = integer(),
            pop_u5_2 = integer(),
            pop_a5_2 = integer(),
            pop_u5 = integer(),
            pop_a5 = integer(),
            comment = character()
          ),
          allowInsertColumn = FALSE,
          allowInvalid = FALSE,
          collapsibleColumns = FALSE,
          currentRowClassName = 'currentRow',
          # currentColClassName= 'currentCol',
          dropdownMenu = list(
            'filter_by_value',
            'filter_action_bar'
          ),
          enterBeginsEditingBoolean = FALSE,
          # enterMoves = list(row = 0, col = 1),
          hiddenColumns = list(
            copyPasteEnabled = FALSE,
            indicators = FALSE,
            columns = c(3:4, 6:13)
          ),
          licenseKey = 'non-commercial-and-evaluation',
          filters = TRUE,
          manualColumnResize = FALSE,
          manualRowResize = FALSE,
          multiColumnSorting = FALSE,
          nestedHeaders = list(
            list(
              list(label = ..('N.')),
              list(label = ..('Sample'), colspan = 2L),
              list(label = ..('Key'), colspan = 2L),
              list(label = ..('Status')),
              list(label = ..('Distance'), colspan = 2L),
              list(label = ..('Pop. 1'), colspan = 2L),
              list(label = ..('Pop. 2'), colspan = 2L),
              list(label = ..('Population'), colspan = 2L),
              list(label = ..('Comment'))
            ),
            list(
              ' ',
              ..('ID'),
              ..('Key'),
              ..('Key'),
              ..('Key'),
              ' ',
              '1',
              '2',
              '< 5',
              '>= 5',
              '< 5',
              '>= 5',
              '< 5',
              '>= 5',
              ' '
            )
          ),
          outsideClickDeselects = TRUE,
          rowHeaders = FALSE,
          selectCallback = TRUE,
          selectionMode = 'single',
          width = '100%',
          height = '100%'
        ) |>
        rhandsontable::hot_table(
          colWidths = c(
            35L,
            90L,
            25L,
            25L,
            25L,
            50L,
            45L,
            45L,
            30L,
            30L,
            30L,
            30L,
            30L,
            30L,
            100L
          ),
          highlightCol = FALSE,
          highlightRow = FALSE,
          stretchH = 'last'
          # overflow = 'visible'
        ) |>
        rhandsontable::hot_context_menu(
          allowRowEdit = FALSE,
          allowColEdit = FALSE,
          allowComments = TRUE
        ) |>
        rhandsontable::hot_col(
          col = 'id_n',
          type = 'numeric',
          format = '1',
          readOnly = TRUE
        ) |>
        rhandsontable::hot_col(
          col = 'id_key_calc',
          type = 'numeric',
          format = '1',
          readOnly = TRUE
        ) |>
        rhandsontable::hot_col(
          col = 'id_key',
          type = 'numeric',
          format = '1',
          renderer = "
           function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.NumericRenderer.apply(this, arguments);
               var a = instance.getDataAtRow(row);
               if(a[2]==a[3]) {
                 td.style.color = 'black';
               } else {
                 td.style.color = 'red';
               }
           }"
        ) |>
        rhandsontable::hot_col(
          col = 'status',
          type = 'dropdown',
          trimDropdown = FALSE,
          source = df_selectors$label,
          renderer = sprintf(
            '
           function (instance, td, row, col, prop, value, cellProperties) {
             Handsontable.renderers.DropdownRenderer.apply(this, arguments);
              var a = instance.getDataAtRow(row);
              if(value=="%s") {
                 td.innerHTML = `<div class=\"htAutocompleteArrow\">&#x25BC;</div><i class="mdi mdi-circle-medium mdi-18px" style="color: %s"></i>`;
              } else if(value=="%s") {
                 td.innerHTML = `<div class=\"htAutocompleteArrow\">&#x25BC;</div><i class="mdi mdi-circle-medium mdi-18px" style="color: %s"></i>`;
              } else if(value=="%s") {
                 td.innerHTML = `<div class=\"htAutocompleteArrow\">&#x25BC;</div><i class="mdi mdi-circle-medium mdi-18px" style="color: %s"></i>`;
              } else if(value=="%s") {
                 td.innerHTML = `<div class=\"htAutocompleteArrow\">&#x25BC;</div><i class="mdi mdi-circle-medium mdi-18px" style="color: %s"></i>`;
              } else if(value=="%s") {
                 td.innerHTML = `<div class=\"htAutocompleteArrow\">&#x25BC;</div><i class="mdi mdi-circle-medium mdi-18px" style="color: %s"></i>`;
              } else if(value=="%s") {
                 td.innerHTML = `<div class=\"htAutocompleteArrow\">&#x25BC;</div><i class="mdi mdi-circle-medium mdi-18px" style="color: %s"></i>`;
              } else if(value=="%s") {
                 td.innerHTML = `<div class=\"htAutocompleteArrow\">&#x25BC;</div><i class="mdi mdi-circle-medium mdi-18px" style="color: %s"></i>`;
              } else if(value=="%s") {
                 td.innerHTML = `<div class=\"htAutocompleteArrow\">&#x25BC;</div><i class="mdi mdi-circle-medium mdi-18px" style="color: %s"></i>`;
              } else {
                 td.innerHTML = `<div class=\"htAutocompleteArrow\">&#x25BC;</div><i class="mdi mdi-circle-medium mdi-18px" style="color: %s"></i>`;
              }
           }',
            df_selectors['C1', 'label'],
            df_selectors['C1', 'color'],
            df_selectors['C2', 'label'],
            df_selectors['C2', 'color'],
            df_selectors['C3', 'label'],
            df_selectors['C3', 'color'],
            df_selectors['C4', 'label'],
            df_selectors['C4', 'color'],
            df_selectors['C5', 'label'],
            df_selectors['C5', 'color'],
            df_selectors['C6', 'label'],
            df_selectors['C6', 'color'],
            df_selectors['C7', 'label'],
            df_selectors['C7', 'color'],
            df_selectors['C8', 'label'],
            df_selectors['C8', 'color'],
            df_selectors['C1', 'color']
          )
        ) |>
        rhandsontable::hot_col(col = 'd1', type = 'numeric') |>
        rhandsontable::hot_col(col = 'd2', type = 'numeric') |>
        rhandsontable::hot_col(
          col = 'pop_u5_1',
          type = 'numeric',
          format = '1'
        ) |>
        rhandsontable::hot_col(
          col = 'pop_a5_1',
          type = 'numeric',
          format = '1'
        ) |>
        rhandsontable::hot_col(
          col = 'pop_u5_2',
          type = 'numeric',
          format = '1'
        ) |>
        rhandsontable::hot_col(
          col = 'pop_a5_2',
          type = 'numeric',
          format = '1'
        ) |>
        rhandsontable::hot_col(
          col = 'pop_u5',
          type = 'numeric',
          format = '1'
        ) |>
        rhandsontable::hot_col(col = 'pop_a5', type = 'numeric', format = '1')

      tbl
    },
    getServer = function(input, output, session) {
      super$getServer(input, output, session)

      # --------------------------------------------------------------------------------------
      # Actions

      shiny::observeEvent(input$act_ok, {
        modalWarning(
          inputIdOk = ns('act_ok_confirm'),
          icon = 'alert-plus',
          msg = lbl_ok,
          msg_info = lbl_ok_info,
          label_cancel = ..('Cancel'),
          label_ok = ..('Ok')
        )
      })

      shiny::observeEvent(input$act_clear, {
        modalDanger(
          inputIdOk = ns('act_clear_confirm'),
          icon = 'alert-remove',
          msg = lbl_cancel,
          msg_info = lbl_cancel_info,
          label_cancel = .('Cancel'),
          label_ok = .('Ok')
        )
      })

      shiny::observeEvent(input$act_ok_confirm, {
        commit()
        shiny::removeModal()
      })

      shiny::observeEvent(input$act_clear_confirm, {
        clear()
        shiny::removeModal()
      })

      shiny::observeEvent(input$act_rollback, {
        rollback()
      })

      # --------------------------------------------------------------------------------------
      # Leaflet

      output$map <- leaflet::renderLeaflet({
        lf <- private$getOutputMap(session)
        return(lf)
      })

      # --------------------------------------------------------------------------------------
      # Map Observers

      observeEventBasemaps(session, input, private$.map_vars, 'map')
      observeEventMap(session, input, private$.map_vars, 'map')

      shiny::observeEvent(input$map_rendered, {
        insertSelectBasemaps(
          inputId = self$ns('map'),
          selected = DEFAULT_BASEMAP,
          position = 'bottomright'
        )

        shinyjs::show(id = 'sidebar')

        # if (!project$getStep('stratify')) {
        # shinyjs::click('link_guide')
        #  project$setStep('stratify', TRUE)
        # }

        private$.map |>
          leaflet::addTiles(
            urlTemplate = 'cache/imageries/{z}_{x}_{y}.png',
            options = leaflet::tileOptions(
              minZoom = 0L,
              maxZoom = 21L,
              maxNativeZoom = 20L,
              zIndex = 201L
            ),
            group = 'custom_basemap'
          )

        private$renderMap()
      })

      shiny::observeEvent(
        data$polygons_changed,
        ignoreNULL = FALSE,
        ignoreInit = TRUE,
        {
          invalidatePolygons()
        }
      )

      shiny::observeEvent(map_changed(), ignoreInit = TRUE, {
        invalidatePolygons()
      })

      shiny::observeEvent(
        data$guide_polygon_changed,
        ignoreNULL = FALSE,
        ignoreInit = TRUE,
        {
          invalidateGuidePolygon()
        }
      )

      shiny::observeEvent(
        data$guide_point_changed,
        ignoreNULL = FALSE,
        ignoreInit = TRUE,
        {
          invalidateGuidePoint()
        }
      )

      shiny::observeEvent(input$map_zoom, ignoreInit = TRUE, {
        self$map_zoom <- input$map_zoom
      })

      shiny::observeEvent(self$state, ignoreInit = TRUE, {
        private$onStateChange()
      })

      private$.map <- leafletProxy(deferUntilFlush = FALSE)

      output$title_panel <- shiny::renderText({
        data$project_method_changed
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
        return(as.character(tab_title))
      })
    },
    guide = function(steps) {
      closeSidebar()

      guideSteps(steps = steps, session = application$session, ns = ns)
    },
    onStateChange = function() {
      private$updateMapBounds(sf = self$data$polygons_selected)
    },
    onMapZoom = function() {
    },
    renderMap = function() {
      invalidatePolygons()
      invalidateState()
    },
    updateMapBounds = function(sf = NULL) {
      sf_valid <- FALSE
      if (!is.null(sf) && (nrow(sf) > 0L)) {
        sf_valid <- TRUE
      }
      if (!sf_valid) {
        sf <- sf::st_sf(sf::st_sfc(st_bbox_polygon(data$bbox_default)))
      }
      private$.map_vars$map_bounds <- getMapBounds(sf::st_bbox(sf))
    },
    checkState = function(value) {
      value
    },
    checkMap = function(value) {
      value
    }
  )
)
