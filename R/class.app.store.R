UserData <- R6::R6Class(
  classname = 'UserData',
  inherit = ApplicationModule,
  portable = FALSE,
  active = list(
    # Creating a function that will return the bounding box of the polygons.
    bbox = function() {
      if (is.null(polygons)) {
        return(private$.bbox_default)
      } else {
        return(sf::st_bbox(polygons))
      }
    },
    bbox_default = function(value) {
      if (!missing(value)) {
        private$.bbox_default <- value
      }
      private$.bbox_default
    },
    db = function() {
      private$.db
    },
    exporter = function() {
      private$.exporter
    },
    tile_mgr = function() {
      private$.tile_mgr
    },
    sample_mgr = function() {
      private$.sample_mgr
    },
    polygon_mgr = function() {
      private$.polygon_mgr
    },
    persistence = function() {
      private$.persistence
    },
    project_active_raw = function(value) {
      if (missing(value)) {
        private$.project_active
      } else {
        private$.project_active <- value
      }
    },
    polygons_raw = function(value) {
      if (missing(value)) {
        private$.polygons
      } else {
        private$.polygons <- value
      }
    },
    cache = function() {
      createDir(getDirAppUsers(application$user$email))
    },
    guide_polygon = function(value) {
      if (!missing(value)) {
        private$.guide_polygon <- value
        private$.guide_polygon_trigger$trigger()
      }
      private$.guide_polygon
    },
    guide_polygon_changed = function() {
      private$.guide_polygon_trigger$depend()
    },
    guide_point = function(value) {
      if (!missing(value)) {
        private$.guide_point <- value
        private$.guide_point_trigger$trigger()
      }
      private$.guide_point
    },
    guide_point_changed = function() {
      private$.guide_point_trigger$depend()
    },
    grid_status_invalidated = function(value) {
      if (!missing(value)) {
        private$.grid_status_invalidated <- value
      }
      private$.grid_status_invalidated
    },
    pool = function() {
      self$db$pool
    },
    project_method = function(value) {
      if (!missing(value)) {
        private$.project_method <- value
        private$.project_method_trigger$trigger()
      }
      private$.project_method
    },
    project_method_changed = function() {
      private$.project_method_trigger$depend()
    },
    projects = function() {
      fs::dir_ls(
        path = fs::path_dir(path = cache),
        full.names = FALSE,
        recurse = FALSE
      )
    },
    project_name = function(value) {
      if (missing(value)) {
        private$.project_name_trigger$depend()
        return(private$.project_name)
      } else {
        if (private$.project_name != value & value != '') {
          private$.project_name <- value
          private$.project_name_trigger$trigger()
        }
      }
    },
    project_description = function(value) {
      if (missing(value)) {
        return(private$.project_description)
      } else {
        if (is.null(value)) {
          value <- ''
        }
        if (private$.project_description != value) {
          private$.project_description <- value
        }
      }
    },
    project_status = function(value) {
      if (missing(value)) {
        return(private$.project_status)
      } else {
        if (private$.project_status != value) {
          private$.project_status <- value
        }
      }
    },
    project_priority = function(value) {
      if (missing(value)) {
        return(private$.project_priority)
      } else {
        if (private$.project_priority != value) {
          private$.project_priority <- value
        }
      }
    },
    #'
    #' Polygons: sf not null
    polygons = function(value) {
      if (missing(value)) {
        return(private$.polygons)
      } else {
        private$.polygons <- makeValidPolygons(value)
        private$.polygons_memento <- private$.polygons
        private$.polygons_trigger$trigger()
        # if(!is.null(private$.session)) {
        #   step_delimit$state <- utils::modifyList(step_delimit$state, list('modified' = TRUE, 'mode' = 'select'))
        # }
      }
    },
    polygons_changed = function() {
      private$.polygons_trigger$depend()
    },
    polygons_count = function() {
      nrow(self$polygons)
    },
    polygons_count_multi = function() {
      sum(sf::st_is(self$polygons, 'MULTIPOLYGON'))
    },
    polygons_selected = function() {
      dplyr::filter(self$polygons, selected)
    },
    polygons_selected_count = function() {
      nrow(polygons_selected)
    },
    polygon_focused = function() {
      if (step == 'step_sample') {
        return(dplyr::filter(self$polygons, focused_sample))
      } else {
        return(dplyr::filter(self$polygons, focused_result))
      }
    },
    polygons_memento = function(value) {
      if (missing(value)) {
        return(private$.polygons_memento)
      } else {
        private$.polygons_memento <- value
      }
    },
    roofs = function() {
      roofs_sf <- safe_db(
        db$dbGetQuery('SELECT * FROM roofs'),
        default = data.frame(
          id = character(),
          polygon = integer(),
          x = integer(),
          y = integer(),
          geometry = character(),
          stringsAsFactors = FALSE
        ),
        msg = "roofs:dbGetQuery"
      )
      if (nrow(roofs_sf) > 0L && "geometry" %in% names(roofs_sf)) {
        roofs_sf <- roofs_sf |>
          sf::st_as_sf(wkt = 'geometry') |>
          sf::st_set_crs(4326L)
      }
      roofs_sf
    },
    roofs_changed = function() {
      private$.roofs_changed
    },
    samples = function() {
      if (nrow(polygon_focused) > 0L) {
        return(polygon_focused$samples_sf[[1L]])
      } else {
        return(sf_empty())
      }
    },
    samples_count = function() {
      nrow(samples)
    },
    selector = function(value) {
      if (missing(value)) {
        return(private$.selector)
      } else {
        if (private$.selector != value) {
          private$.selector <- value
        }
      }
    },
    step = function(value) {
      if (missing(value)) {
        return(private$.step)
      } else {
        if (private$.step != value) {
          private$.step <- value
        }
      }
    },
    settings = function() {
      private$.settings
    },
    steps = function() {
      application$domain$userData$steps
    },
    step_delimit = function() {
      steps$step_delimit
    },
    step_identify = function() {
      steps$step_identify
    },
    step_sample = function() {
      steps$step_sample
    },
    step_result = function() {
      steps$step_result
    },
    tiles = function() {
      dplyr::tbl(self$db$pool, 'tiles')
    }
  ),
  public = list(
    #' @description
    #' Initialize Application class for shiny app
    #'
    #' @return A new Application object
    initialize = function(application, load = TRUE) {
      super$initialize(id = 'app', parent = application)

      self$ns <- shiny::NS('app')

      private$.bbox_default <- application$private$.bbox
      private$.guide_polygon_trigger <- reactiveTrigger()
      private$.guide_point_trigger <- reactiveTrigger()

      private$.selector <- 'none'

      private$.polygons <- makeValidPolygons(NULL)
      private$.polygons_trigger <- reactiveTrigger()

      private$.project_status <- 'D'
      private$.project_priority <- 'U'
      private$.project_description <- ''
      private$.project_method <- 'UNK'
      private$.project_method_trigger <- reactiveTrigger()
      private$.project_name <- 'default'
      private$.project_name_trigger <- reactiveTrigger()

      private$.project_active <- private$.project_name

      private$.roofs_changed <- shiny::reactiveVal(0L)

      private$.db <- ModDatabase$new(
        dbname = fs::path(cache, 'identify.sqlite')
      )

      if (!self$db$dbExistsTable('tiles')) {
        self$db$dbCreateTable(
          name = 'tiles',
          fields = data.frame(
            x = integer(),
            y = integer(),
            geometry = character(),
            locked = logical(),
            status = character(),
            stringsAsFactors = FALSE
          )
        )
      }

      if (!self$db$dbExistsTable('roofs')) {
        self$db$dbCreateTable(
          name = 'roofs',
          fields = data.frame(
            id = character(),
            polygon = integer(),
            x = integer(),
            y = integer(),
            geometry = character(),
            stringsAsFactors = FALSE
          )
        )
      }

      tokens <- .globals$session$tokens
      placeholders <- paste(rep('?', length(tokens)), collapse = ', ')
      safe_db(
        self$db$dbExecute(
          sql = paste0(
            'UPDATE tiles SET locked = NULL WHERE locked NOT IN (',
            placeholders,
            ')'
          ),
          params = as.list(tokens)
        ),
        msg = "initialize:unlock stale tiles"
      )

      private$.settings <- Settings$new()

      private$.settings$addSetting('csi_circle_color', DEFAULT_CSI_CIRCLE_COLOR)
      private$.settings$addSetting(
        'sli_circle_radius',
        DEFAULT_SLI_CIRCLE_RADIUS
      )
      private$.settings$addSetting(
        'sli_circle_opacity',
        DEFAULT_SLI_CIRCLE_OPACITY
      )
      private$.settings$addSetting('sli_identify_zoom', ZOOM_TILE)

      private$.settings$addSetting(
        'sli_sample_radius',
        DEFAULT_SLI_SAMPLE_RADIUS
      )
      private$.settings$addSetting('sli_sample_zoom', ZOOM_SAMPLE)

      private$.step <- 'none'

      private$.tile <- NULL
      private$.tile_trigger <- reactiveTrigger()

      private$.tiles <- terra::rast()
      private$.tiles_trigger <- reactiveTrigger()
      private$.tiles_pal <- leaflet::colorNumeric(
        palette = TILE_PALETTE_COLORS,
        domain = TILE_PALETTE_DOMAIN,
        na.color = scales::alpha('black', 0L),
        alpha = TRUE
      )

      private$.exporter <- DocumentExporter$new(data = self)
      private$.tile_mgr <- TileManager$new(data = self)
      private$.sample_mgr <- SampleManager$new(data = self)
      private$.polygon_mgr <- PolygonManager$new(data = self)
      private$.persistence <- DataPersistence$new(data = self)

      if (load) {
        self$load()
      }
    },
    updateCells = function(polygons_sf) {
      self$polygon_mgr$updateCells(polygons_sf)
    },
    drawNewFeature = function(map, feature) {
      self$polygon_mgr$drawNewFeature(map, feature)
    },
    getChoicesStatus = function() {
      c_named(
        c('D', 'P', 'A'),
        c(..('Draft'), ..('Published'), ..('Archived'))
      )
    },
    getChoicesPriority = function() {
      c_named(
        c('U', 'H', 'M', 'L'),
        c(..('Urgent'), ..('High'), ..('Medium'), ..('Low'))
      )
    },
    getTileStatus = function(polygon = NULL) {
      self$tile_mgr$getTileStatus(polygon)
    },
    displayGridIdentifyStatus = function(map, token, force = FALSE, ...) {
      self$tile_mgr$displayGridIdentifyStatus(map, token, force, ...)
    },
    displayGuidePolygon = function(map) {
      self$polygon_mgr$displayGuidePolygon(map)
    },
    displayGuidePoint = function(map) {
      self$polygon_mgr$displayGuidePoint(map)
    },
    export = function() {
      self$exporter$export()
    },
    import = function() {
      self$exporter$import()
    },
    invertSelection = function(map) {
      self$polygon_mgr$invertSelection(map)
    },
    load = function(project_active = NULL, force = FALSE) {
      self$persistence$load(project_active, force)
    },
    cutPolygons = function(map, line) {
      self$polygon_mgr$cutPolygons(map, line)
    },
    makeValidPolygons = function(sf, id_max = 0L) {
      self$polygon_mgr$makeValidPolygons(sf, id_max)
    },
    displayPolygons = function(
      map,
      only_selected = FALSE,
      sample = FALSE,
      cells = FALSE,
      plan = FALSE,
      fit = TRUE,
      status = TRUE,
      sf = NULL,
      ...
    ) {
      self$polygon_mgr$displayPolygons(
        map,
        only_selected,
        sample,
        cells,
        plan,
        fit,
        status,
        sf,
        ...
      )
    },
    addPolygon = function(map, polygon) {
      self$polygon_mgr$addPolygon(map, polygon)
    },
    deletePolygon = function(map, id) {
      self$polygon_mgr$deletePolygon(map, id)
    },
    editPolygon = function(map, feature) {
      self$polygon_mgr$editPolygon(map, feature)
    },
    togglePolygonSelected = function(map, id) {
      self$polygon_mgr$togglePolygonSelected(map, id)
    },
    togglePolygonFocused = function(map, id, group, step) {
      self$polygon_mgr$togglePolygonFocused(map, id, group, step)
    },
    setStylePolygonFocused = function(map, focused, group = 'polygons') {
      self$polygon_mgr$setStylePolygonFocused(map, focused, group)
    },
    addProject = function(status, priority, name, description) {
      self$persistence$addProject(status, priority, name, description)
    },
    projectClone = function(status, priority, name, description) {
      self$persistence$projectClone(status, priority, name, description)
    },
    projectEdit = function(status, priority, description) {
      self$persistence$projectEdit(status, priority, description)
    },
    projectDelete = function() {
      self$persistence$projectDelete()
    },
    projectSelect = function(name = 'default') {
      self$persistence$projectSelect(name)
    },
    addRoof = function(map, lng, lat, ...) {
      self$tile_mgr$addRoof(map, lng, lat, ...)
    },
    removeRoof = function(map, roof_id, ...) {
      self$tile_mgr$removeRoof(map, roof_id, ...)
    },
    roofsAddOSM = function(map, progress, ...) {
      roofs_sf <- NULL

      map |>
        leaflet::clearGroup('tiles')

      polygons <- self$polygons

      for (i in seq_len(nrow(self$polygons))) {
        polygon_sf <- self$polygons[i, ]

        map |>
          fitToSpatialFeatureBounds(polygon_sf)

        bbox <- sf::st_bbox(polygon_sf)

        tile_grid <- slippymath::bbox_to_tile_grid(bbox = bbox, zoom = ZOOM_OSM)
        tile_grid_sf <- tile_grid_to_sf(tile_grid)
        tile_grid_intersect <- sf::st_intersects(
          tile_grid_sf,
          polygon_sf,
          sparse = FALSE
        )

        tile_polygons <- tibble::tibble(sf_polygon_roofs = list())

        tiles <- tibble::as_tibble(tile_grid$tiles)
        tiles$building <- NA

        sf_polygon_roofs <- NULL

        tiles_dir <- fs::path(self$cache, 'imageries')

        for (t in seq_len(OSM_MAX_TILES)) {
          progress(
            value = 100L *
              ((i - 1L) /
                nrow(self$polygons) +
                t / nrow(tiles) / nrow(self$polygons))
          )

          tile <- tiles[t, ]

          if (tile_grid_intersect[t]) {
            tile_sfc <- st_bbox_polygon(tile_bbox_ll(tile$x, tile$y, ZOOM_OSM))

            leaflet::addPolygons(
              map,
              data = tile_sfc,
              color = 'yellow',
              fill = TRUE,
              group = 'tiles',
              weight = 0.5
            )

            osm <-
              osmdata::opq(bbox = sf::st_bbox(tile_sfc)) |>
              osmdata::add_osm_feature(key = 'building') |>
              osmdata::osmdata_sf() |>
              cleanOSM()

            sf_tile_roof <- osm$osm_polygons |>
              dplyr::select(osm_id, building)

            if (t == 1L) {
              sf_polygon_roofs <- sf_tile_roof
            } else {
              sf_polygon_roofs <- rbind(sf_polygon_roofs, sf_tile_roof)
            }
          }
        }

        polygon_roofs_intersect <- sf::st_intersects(
          sf_polygon_roofs,
          sf_polygon_roofs,
          sparse = FALSE
        )

        sf_polygon_roofs_intersect <- sf_polygon_roofs[
          rowSums(polygon_roofs_intersect) > 1L,
        ]
        sf_polygon_roofs_intersect <- sf::st_union(sf_polygon_roofs_intersect)
        sf_polygon_roofs_intersect <- sf::st_cast(
          sf_polygon_roofs_intersect,
          'POLYGON'
        )

        sf_polygon_roofs <- sf_polygon_roofs[
          rowSums(polygon_roofs_intersect) == 1L,
        ]
        sf_polygon_roofs <- dplyr::bind_rows(
          sf_polygon_roofs,
          sf::st_sf(sf_polygon_roofs_intersect)
        )
        # sf_polygon_roofs <- sf::st_union(sf_polygon_roofs)

        if (i == 1L) {
          roofs_sf <- sf_polygon_roofs
        } else {
          roofs_sf <- rbind(roofs_sf, sf_polygon_roofs)
        }
      }

      for (i in seq_len(nrow(self$polygons))) {
        progress(value = 100L * (i - 1L) / nrow(self$polygons))

        polygon_sf <- self$polygons[i, ]

        map |>
          fitToSpatialFeatureBounds(polygon_sf)

        osm <-
          osmdata::opq(bbox = sf::st_bbox(polygon_sf)) |>
          osmdata::add_osm_feature(key = 'building') |>
          osmdata::osmdata_sf() |>
          cleanOSM()

        sf_polygon_roofs <-
          sf::st_intersection(osm$osm_polygons, sf::st_geometry(polygon_sf)) |>
          dplyr::mutate(
            polygons = i
          )

        if (i == 1L) {
          roofs_sf <- sf_polygon_roofs
        } else {
          roofs_sf <- rbind(roofs_sf, sf_polygon_roofs)
        }
      }

      progress(value = 100L)

      map |>
        fitToSpatialFeatureBounds(self$polygons)

      roofs_sf <- roofs_sf |>
        sf::st_set_crs('EPSG:3857') |>
        sf::st_set_agr('constant') |>
        sf::st_centroid() |>
        sf::st_transform(4326L)

      private$roofs_uploaded <- NULL
      private$roofs_uploaded <- roofs_sf
    },
    addRoofs = function(map, roofs, ...) {
      self$tile_mgr$addRoofs(map, roofs, ...)
    },
    addSample = function(map = NULL) {
      self$sample_mgr$addSample(map)
    },
    addSamples = function(map, sf, ...) {
      self$sample_mgr$addSamples(map, sf, ...)
    },
    displayRoofs = function(
      map,
      roofs,
      tile_x = roofs$x[1L],
      tile_y = roofs$y[1L]
    ) {
      self$tile_mgr$displayRoofs(map, roofs, tile_x, tile_y)
    },
    generateSamples = function(
      map = NULL,
      type = 'SP_QDR',
      method = 'random',
      count = 35L,
      size = 25L,
      polygon_idx = NULL,
      samples = NULL
    ) {
      self$sample_mgr$generateSamples(
        map,
        type,
        method,
        count,
        size,
        polygon_idx,
        samples
      )
    },
    save = function(
      delimit = FALSE,
      identify = FALSE,
      sample = FALSE,
      result = FALSE,
      project = FALSE
    ) {
      self$persistence$save(delimit, identify, sample, result, project)
    },
    clear = function(
      delimit = FALSE,
      identify = FALSE,
      sample = FALSE,
      result = FALSE
    ) {
      self$persistence$clear(delimit, identify, sample, result)
    },
    rollback = function(
      delimit = FALSE,
      identify = FALSE,
      sample = FALSE,
      result = FALSE
    ) {
      self$persistence$rollback(delimit, identify, sample, result)
    },
    readFromCache = function(file) {
      self$persistence$readFromCache(file)
    },
    saveToCache = function(rds, file) {
      self$persistence$saveToCache(rds, file)
    },
    sampleAppend = function(map = NULL, polygon_idx = NULL) {
      self$sample_mgr$sampleAppend(map, polygon_idx)
    },
    calculateSample = function(progress = NULL) {
      self$sample_mgr$calculateSample(progress)
    },
    displaySample = function(
      map,
      polygon_idx,
      samples_idx = NULL,
      plan = FALSE,
      fit = FALSE,
      tooltip = FALSE,
      status = TRUE,
      ...
    ) {
      self$sample_mgr$displaySample(
        map,
        polygon_idx,
        samples_idx,
        plan,
        fit,
        tooltip,
        status,
        ...
      )
    },
    sampleDisplayToTable = function(
      table_id,
      polygon_idx,
      samples_idx = NULL,
      plan = FALSE,
      tooltip = FALSE,
      ...
    ) {
      self$sample_mgr$sampleDisplayToTable(
        table_id,
        polygon_idx,
        samples_idx,
        plan,
        tooltip,
        ...
      )
    },
    sampleExportFilename = function(extension) {
      self$exporter$sampleExportFilename(extension)
    },
    sampleExportToDoc = function(file, open = TRUE, polygon_idx = NULL) {
      self$exporter$sampleExportToDoc(file, open, polygon_idx)
    },
    sampleExportToDocEach = function(
      file,
      open = TRUE,
      polygon_idx = NULL,
      samples_idxs = NULL
    ) {
      self$exporter$sampleExportToDocEach(file, open, polygon_idx, samples_idxs)
    },
    # TODO: #7 Implement random sampling mode management
    sampleQuadratEdit = function(map, id, ...) {
      self$sample_mgr$sampleQuadratEdit(map, id, ...)
    },
    sampleQuadratPost = function(map, feature) {
      self$sample_mgr$sampleQuadratPost(map, feature)
    },
    sampleLoadTable = function(tbl_id) {
      self$sample_mgr$sampleLoadTable(tbl_id)
    },
    sampleToggleSelect = function(map, sample) {
      self$sample_mgr$sampleToggleSelect(map, sample)
    },
    samplesSetValueFromTable = function(map, recs, vars, values) {
      self$sample_mgr$samplesSetValueFromTable(map, recs, vars, values)
    },
    samplesSetValueFromMap = function(table_id, id, var, value) {
      self$sample_mgr$samplesSetValueFromMap(table_id, id, var, value)
    },
    addCell = function(
      map,
      token,
      cell = NULL,
      inverse = FALSE,
      toggle = TRUE
    ) {
      self$tile_mgr$addCell(map, token, cell, inverse, toggle)
    },
    searchSwipeTile = function(
      map,
      direction,
      map_size,
      token,
      mapped = FALSE,
      valid = FALSE,
      invalid = FALSE,
      lat = NULL,
      lon = NULL
    ) {
      self$tile_mgr$searchSwipeTile(
        map,
        direction,
        map_size,
        token,
        mapped,
        valid,
        invalid,
        lat,
        lon
      )
    },
    validAll = function(map) {
      self$tile_mgr$validAll(map)
    },
    triggerProjectName = function() {
      private$.project_name_trigger$trigger()
    },
    invalidateGrid = function() {
      private$.tiles_trigger$trigger()
    }
  ),
  private = list(
    .bbox_default = NULL,
    .db = NULL,
    .exporter = NULL,
    .tile_mgr = NULL,
    .sample_mgr = NULL,
    .polygon_mgr = NULL,
    .persistence = NULL,
    .grid_status_invalidated = TRUE,
    .guide_polygon = NULL,
    .guide_polygon_trigger = NULL,
    .guide_point = NULL,
    .guide_point_trigger = NULL,
    .project_active = NULL,
    .project_name_trigger = NULL,
    .project_status = NULL,
    .project_priority = NULL,
    .project_method = NULL,
    .project_method_trigger = NULL,
    .project_name = NULL,
    .project_description = NULL,
    .polygons = NULL,
    .polygons_memento = NULL,
    .polygons_trigger = NULL,
    .roofs_changed = NULL,
    .tile = NULL,
    .tile_trigger = NULL,
    .tiles = NULL,
    .tiles_trigger = NULL,
    .tiles_pal = NULL,
    .settings = NULL,
    .step = NULL,
    .selector = NULL,
    getServer = function(input, output, session) {
      super$getServer(input, output, session)

      # Data
      # .................................................

      # global reactive values to communication between modules
      vars <- shiny::reactiveValues()

      # Session
      # .................................................

      .globals$session$tokens <- c(.globals$session$tokens, session$token)

      profvis_file <- fs::path(
        self$cache,
        strftime(Sys.time(), '%Y-%m-%d_%H-%M-%S.Rprof')
      )

      # Shiny modules
      # .................................................

      mdl_about <- modalAbout(vars = vars, project = project)

      application$addModule('steps', Steps$new(parent = self))

      private$.db$bind()

      # Bookmarks (Save states)
      # .................................................

      shiny::setBookmarkExclude(c('act_save', 'act_help'))

      shiny::onBookmark(function(state) {
        state$values$project <- project
      })

      shiny::onRestore(function(state) {
        project <- state$values$project
      })

      # Shiny events
      # .................................................

      shiny::observeEvent(input$act_profvis_stop, {
        utils::Rprof(NULL)
        rlang::check_installed('profvis', reason = 'to view profiling results')
        p <- profvis::profvis(
          interval = 0.01,
          prof_input = profvis_file,
          simplify = FALSE
        )
        fp <- fs::file_temp(ext = 'html')
        fp <- 'profile.html'
        htmlwidgets::saveWidget(p, file = 'profile.html')
        fs::file_show(path = fp)
      })

      shiny::observeEvent(input$resize, {
        vars$width <- input$resize[1L]
        vars$height <- input$resize[2L]
      })

      shiny::observeEvent(input$act_help, {
        current_tab <- input$navbar
        current_view <- project$view

        steps <-
          tibble::tribble(
            ~tab,
            ~view,
            ~selector,
            ~intro,
            NA,
            NA,
            NA,
            '<h4>Welcome</h4><p>Welcome to the Geo-Sampler tutorial.</p><p></p><p>Click to buttons below to understand how this app works.</p>',
            NA,
            NA,
            '.navbar',
            '<b>Menu bar</b>: This menu bar allows to navigate through the different pages of this app.',
            'mod_steps',
            'mod_step_extent',
            'map',
            'STEP 1: Start to add spatial features (boundaries polygons)',
            'mod_steps',
            'mod_step_extent',
            'map .leaflet-pm-icon-rectangle',
            'STEP 1: Start to add spatial features (boundaries polygons)',
            'mod_steps',
            'mod_step_extent',
            'act_ok_btn',
            '<b>STEP 2</b>: When you have a polygons selected, you can generate a new sample point.',
            'mod_steps',
            'mod_step_extent',
            'act_clear_btn',
            '<b>STEP 3</b>: When you have a generated sample point, click here to keep it.',
            'mod_steps',
            'mod_step_extent',
            'map_zoom_in_btn',
            '<b>Map</b>: Zoom In. <br><br>The zooming can also be done by moving the mouse wheel.',
            'mod_steps',
            'mod_step_extent',
            'map_zoom_out_btn',
            '<b>Map</b>: Zoom Out. <br><br>The zooming can also be done by moving the mouse wheel.',
            'mod_steps',
            'mod_step_extent',
            'map_zoom_extent_btn',
            "<b>Map</b>: Zoom to layer's extent",
            'mod_steps',
            'mod_step_extent',
            'map .leaflet-control-fullscreen',
            '<b>Full Screen</b>: Click here to switch to Full screen mode. Click Esc to Exit Full Screen mode.'
          )

        steps <- steps |>
          dplyr::filter(
            tab == current_tab,
            view == current_view
          ) |>
          dplyr::transmute(
            element = sprintf('#%s-%s-%s', tab, view, selector),
            intro = intro
          )

        rintrojs::introjs(session, options = list(steps = steps))
      })
    }
  )
)
