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
      roofs_sf <- db$dbGetQuery('SELECT * FROM roofs')
      roofs_sf <- roofs_sf |>
        sf::st_as_sf(wkt = 'geometry') |>
        sf::st_set_crs(4326L)
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

      # c(13.02915, 11.76533, 13.24897, 11.89713)
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
      self$db$dbExecute(
        sql = paste0(
          'UPDATE tiles SET locked = NULL WHERE locked NOT IN (',
          placeholders,
          ')'
        ),
        params = as.list(tokens)
      )

      private$.settings <- Settings$new()

      private$.settings$addSetting('csi_circle_color', '#80FF00')
      private$.settings$addSetting('sli_circle_radius', 2L)
      private$.settings$addSetting('sli_circle_opacity', 25L)
      private$.settings$addSetting('sli_identify_zoom', 18L)

      private$.settings$addSetting('sli_sample_radius', 25L)
      private$.settings$addSetting('sli_sample_zoom', 20L)

      private$.step <- 'none'

      private$.tile <- NULL
      private$.tile_trigger <- reactiveTrigger()

      private$.tiles <- terra::rast()
      private$.tiles_trigger <- reactiveTrigger()
      private$.tiles_pal <- leaflet::colorNumeric(
        palette = c(
          scales::alpha('red', 0.5),
          scales::alpha('black', 0L),
          scales::alpha('white', 0.5),
          scales::alpha('yellow', 0.5),
          scales::alpha('green', 0.5),
          scales::alpha('red', 0.5)
        ),
        domain = -2:3,
        na.color = scales::alpha('black', 0L),
        alpha = TRUE
      )

      private$.exporter <- DocumentExporter$new(data = self)
      private$.tile_mgr <- TileManager$new(data = self)
      private$.sample_mgr <- SampleManager$new(data = self)

      if (load) {
        self$load()
      }
    },
    #' Update Cells
    #'
    #' This method updates the cells of a given set of polygons.
    #'
    #' @param polygons_sf A sf object containing the polygons to update.
    #'
    #' @return The updated polygons_sf object with the cells information added.
    #'
    updateCells = function(polygons_sf) {
      polygons_sf$cells_vect <- list(sf_empty())
      polygons_sf$cells_vect_rev <- list(sf_empty())

      for (i in seq_len(nrow(polygons_sf))) {
        polygon <- polygons_sf[i, ]

        if (polygon$selected) {
          tiles <- db$dbGetQuery(
            'SELECT x, y, cells FROM tiles WHERE polygon = ? AND cells <> "000000000"',
            params = list(polygon$id_n)
          )

          if (nrow(tiles)) {
            cells <- tiles |>
              dplyr::mutate(key = seq_len(dplyr::n())) |>
              tidyr::crossing(data.frame(cell = 9:1)) |>
              dplyr::arrange(key, cell) |>
              dplyr::mutate(
                builded = as.integer(stringr::str_split_fixed(
                  string = paste(tiles$cells, collapse = ''),
                  pattern = '',
                  n = Inf
                ))
              ) |>
              dplyr::filter(builded == 1L) |>
              dplyr::select(-cells)

            rg_x <- range(cells$x)
            rg_y <- range(cells$y)

            cells_rst <-
              terra::rast(
                names = 'builded',
                nrows = (rg_y[2L] - rg_y[1L] + 1L) * 3L,
                ncols = (rg_x[2L] - rg_x[1L] + 1L) * 3L,
                nlyrs = 1L,
                vals = NA,
                xmin = rg_x[1L],
                xmax = rg_x[2L] + 1L,
                ymin = rg_y[1L],
                ymax = rg_y[2L] + 1L
              )

            cells_rst_df <-
              tibble::as_tibble(as.data.frame(
                cells_rst,
                xy = TRUE,
                na.rm = FALSE
              )) |>
              dplyr::transmute(
                x.cell = floor(abs(x - as.integer(x)) * 3L),
                y.cell = floor(abs(y - as.integer(y)) * 3L),
                cell = 6L - y.cell * 3L + x.cell + 1L,
                x = as.integer(x),
                y = as.integer(y)
              )

            cells_rst_df <- dplyr::left_join(
              cells_rst_df,
              cells,
              by = c('cell', 'x', 'y')
            )

            terra::values(cells_rst) <- cells_rst_df$builded

            cells_rst <- cells_rst |>
              terra::flip(direction = 'vertical')

            # terra::plot(cells_rst)

            rg_ll_tl <- slippymath::tilenum_to_lonlat(
              x = rg_x[1L],
              y = rg_y[2L] + 1L,
              zoom = 18L
            )
            rg_ll_br <- slippymath::tilenum_to_lonlat(
              x = rg_x[2L] + 1L,
              y = rg_y[1L],
              zoom = 18L
            )

            terra::ext(cells_rst) <- c(
              rg_ll_tl$lon,
              rg_ll_br$lon,
              rg_ll_tl$lat,
              rg_ll_br$lat
            )

            terra::crs(cells_rst) <- 'epsg:4326'

            # terra::plot(r)

            # cells_mp <- lapply(
            #   1:nrow(cells),
            #   function(r) {
            #     bbox <- slippymath::tile_bbox(
            #       x = cells$x[r],
            #       y = cells$y[r], 18
            #     )
            #     # bbox_sfg <- sf::st_polygon(list(matrix(bbox[c(1, 2, 3, 2, 3, 4, 1, 4, 1, 2)], ncol = 2, byrow = TRUE)))
            #     # plot(bbox_sfg)
            #     bbox <- as.list(bbox)
            #     cell_w <- (bbox$xmax - bbox$xmin) / 3
            #     cell_h <- (bbox$ymax - bbox$ymin) / 3
            #     cell_xmin <- bbox$xmin + cell_w * (cells$cell[r] - 1) %% 3
            #     cell_xmax <- bbox$xmin + cell_w * (cells$cell[r] - 1) %% 3 + cell_w
            #     cell_ymin <- bbox$ymin + cell_h * (cells$cell[r] - 1) %/% 3
            #     cell_ymax <- bbox$ymin + cell_h * (cells$cell[r] - 1) %/% 3 + cell_h
            #     cell_bbox <- sf::st_bbox(c(xmin = cell_xmin, xmax = cell_xmax, ymax = cell_ymax, ymin = cell_ymin))
            #     cell_sfg <- sf::st_polygon(list(matrix(cell_bbox[c(1, 2, 3, 2, 3, 4, 1, 4, 1, 2)], ncol = 2, byrow = TRUE)))
            #     # plot(cell_sfg, add=TRUE)
            #     return(cell_sfg)
            #   }
            # )
          } else {
            cells_rst <- rast_empty()
          }

          cells_vect <- rasterCellsToPolygon(cells_rst)
          cells_vect <- terra::crop(
            cells_vect,
            sf::st_geometry(polygon) |> terra::vect()
          ) |>
            sf::st_as_sf()

          polygons_sf[i, ]$cells_vect[[1L]] <- cells_vect
          polygons_sf[i, ]$cells_count <- sum(
            terra::values(cells_rst),
            na.rm = TRUE
          )

          cells_rst_rev <- polygonToRasterCells(polygon)
          cells_rst_rs <- terra::resample(cells_rst, cells_rst_rev)
          terra::values(cells_rst_rev) <- ifelse(
            !is.na(terra::values(cells_rst_rev)),
            0L,
            1L
          )
          terra::values(cells_rst_rs) <- ifelse(
            !is.na(terra::values(cells_rst_rs)),
            0L,
            2L
          )

          cells_rst_rev <- terra::merge(cells_rst_rs, cells_rst_rev)

          terra::values(cells_rst_rev) <- ifelse(
            terra::values(cells_rst_rev) == 2L,
            NA,
            1L
          )

          cells_vect_rev <- rasterCellsToPolygon(cells_rst_rev)
          cells_vect_rev <- terra::crop(
            cells_vect_rev,
            sf::st_geometry(polygon) |> terra::vect()
          ) |>
            sf::st_as_sf()

          polygons_sf[i, ]$cells_vect_rev[[1L]] <- cells_vect_rev
        }
      }

      polygons_sf
    },
    drawNewFeature = function(map, feature) {
      feature_sf <-
        st_as_sf.feature(feature)
      # sf::st_crop(project$getExtent()$getExtent())

      if (sf::st_is(feature_sf, 'LINESTRING')) {
        cutPolygons(map, line = feature_sf)
      } else {
        addPolygon(map, polygon = feature_sf)
      }
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
      map <- map |>
        leaflet::clearGroup('guide_polygon')

      if (!is.null(self$guide_polygon)) {
        map <- map |>
          leaflet::addPolygons(
            data = self$guide_polygon,
            color = 'white',
            fill = FALSE,
            group = 'guide_polygon',
            label = ~lf_name,
            labelOptions = leaflet::labelOptions(
              noHide = TRUE,
              direction = 'center',
              textOnly = FALSE,
              textsize = '10px',
              style = list(
                `background-color` = '#ffffff22',
                border = 'none',
                `box-shadow` = 'none',
                color = 'white',
                `font-size` = '10px',
                `text-align` = 'center',
                `text-shadow` = '0px 0px 1px #000, 0px 0px 1px #000;'
              )
            ),
            layerId = ~lf_key,
            options = leaflet::pathOptions(pane = 'polygon_1'),
            opacity = 1L,
            stroke = TRUE,
            weight = 1L
          )
      }

      map
    },
    displayGuidePoint = function(map) {
      map <- map |>
        leaflet::clearGroup('guide_point')

      if (!is.null(self$guide_point)) {
        map <- map |>
          leaflet::addCircleMarkers(
            data = self$guide_point,
            fill = TRUE,
            fillColor = 'white',
            fillOpacity = 1L,
            group = 'guide_point',
            label = ~lf_name,
            labelOptions = leaflet::labelOptions(
              direction = 'bottom',
              noHide = TRUE,
              offset = c(0L, 7L),
              textOnly = FALSE,
              textsize = '10px',
              style = list(
                'background-color' = '#ffffff22',
                # 'border' = 'none',
                'border-color' = 'white',
                'box-shadow' = 'none',
                # "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                color = 'white',
                'font-size' = '12px',
                padding = '0 3px 0 3px',
                'text-align' = 'center',
                'text-shadow' = '0px 0px 1px #000, 0px 0px 1px #000;'
              )
            ),
            layerId = ~lf_key,
            options = leaflet::pathOptions(pane = 'polygon_1'),
            opacity = 1L,
            radius = 4L,
            stroke = FALSE
          )
      }

      map
    },
    export = function() {
      self$exporter$export()
    },
    import = function() {
      self$exporter$import()
    },
    invertSelection = function(map) {
      private$.polygons$selected <- !self$polygons$selected

      map |>
        setStyleFast(
          group = 'polygons',
          fill_opacities = as.list(ifelse(self$polygons$selected, 0.2, 0.01))
        )

      step_delimit$state <- utils::modifyList(
        step_delimit$state,
        list(modified = TRUE)
      )
    },
    load = function(project_active = NULL, force = FALSE) {
      if (is.null(project_active)) {
        fp <- fs::path(cache, 'project.rds')
        if (fs::file_exists(fp)) {
          private$.project_active <- readFromCache('project.rds')$name
        } else {
          private$.project_active <- 'default'

          project <- list(
            status = private$.project_status,
            priority = private$.project_priority,
            method = private$.project_method,
            name = private$.project_name,
            description = private$.project_description
          )

          saveToCache(project, file = 'project.rds')
        }
      } else {
        if (private$.project_active == project_active && !force) {
          return()
        }

        private$.project_active <- project_active
      }

      project <- readFromCache(file = 'project.rds')

      if (!is.null(project)) {
        self$project_status <- project$status
        self$project_priority <- project$priority
        self$project_method <- project$method
        self$project_name <- project$name
        self$project_description <- project$description
      }

      private$.project_name_trigger$trigger()

      guide_polygon_sf <- readFromCache(file = 'guide_polygon.rds')
      guide_point_sf <- readFromCache(file = 'guide_point.rds')

      self$guide_polygon <- guide_polygon_sf
      self$guide_point <- guide_point_sf

      polygons_sf <- readFromCache(file = 'polygons.rds')

      if (!is.null(polygons_sf) && (nrow(polygons_sf) > 0L)) {
        polygons_sf$focused_sample <- FALSE
        polygons_sf$focused_result <- FALSE
      }

      self$polygons <- polygons_sf
    },
    cutPolygons = function(map, line) {
      intersect <- sf::st_intersects(self$polygons, line, FALSE)

      if (any(intersect)) {
        polygons_intersect <- self$polygons[intersect, ]

        for (i in seq_len(nrow(polygons_intersect))) {
          polygon_intersect <- polygons_intersect[i, ]

          # Cast MULTIPOLYGON to POLYGON before splitting
          geom_type <- sf::st_geometry_type(
            polygon_intersect,
            by_geometry = TRUE
          )
          if (any(geom_type == 'MULTIPOLYGON')) {
            polygon_intersect <- sf::st_cast(polygon_intersect, 'POLYGON')
          }

          all_split <- list()
          for (j in seq_len(nrow(polygon_intersect))) {
            sub_polygon <- polygon_intersect[j, ]
            sub_split <- tryCatch(
              {
                result <- lwgeom::st_split(sub_polygon, sf::st_geometry(line))
                sf::st_collection_extract(result, 'POLYGON')
              },
              error = function(e) sub_polygon
            )
            all_split[[j]] <- sub_split
          }
          polygons_split <- do.call(rbind, all_split)

          if (nrow(polygons_split) > 1L) {
            polygons_split <- makeValidPolygons(
              polygons_split,
              id_max = st_get_id_max(self$polygons, 'ZN')
            )

            self$polygons <- self$polygons |>
              dplyr::filter(id != polygons_intersect$id[i]) |>
              dplyr::bind_rows(polygons_split)

            leaflet::removeShape(map, layerId = polygons_intersect$id[i])
            map |>
              displayPolygons(sf = polygons_split, fit = FALSE)
          }
        }

        step_delimit$state <- utils::modifyList(
          step_delimit$state,
          list(modified = TRUE, mode = 'select')
        )
      }

      map |>
        leafpm::clearFeatures()
    },
    makeValidPolygons = function(sf, id_max = 0L) {
      if (is.null(sf)) {
        sf <- sf_empty()
      }

      sf <- sf::st_make_valid(sf)

      # sf <- sf[sf::st_is_valid(sf), ]

      sf <- sf::st_set_crs(sf, 4326L)
      sf <- dplyr::mutate(
        sf,
        id_n = seq_len(dplyr::n()) + id_max,
        id = st_id(id_n, 'ZN'),
        area = sf::st_area(sf),
        name = sprintf('Polygon %s', id_n),
        label = sprintf(
          '<b>%s</b><br>%s (km\u00b2)',
          name,
          round(units::set_units(area, km^2L), 2L)
        ),
        label_short = id_n
      )

      key_name <- getKeyName(sf)

      if ('lf_name' %in% names(sf)) {
        key_name <- 'lf_name'
      }

      if (is.null(key_name)) {
        sf <- dplyr::mutate(sf, name = sprintf('Polygon %s', id_n))
      } else {
        key_name <- rlang::sym(key_name)
        sf <- dplyr::mutate(sf, name = as.character(!!key_name))
      }

      sf <- sf |>
        dplyr::mutate(
          label = sprintf(
            '<b>%s</b><br>%s (km\u00b2)',
            name,
            round(units::set_units(area, km^2L), 2L)
          ),
          label_short = id_n
        )

      if (is.null(sf$selected)) {
        sf <- sf |>
          dplyr::mutate(
            selected = FALSE,
            focused_sample = FALSE,
            focused_result = FALSE,
            type = 'SP_QDR',
            method = 'random',
            count = 35L,
            size = 200L,
            confidence = 95L,
            tvalue = NA_real_,
            t = NA_real_,
            z = NA_real_,
            z_p = NA_real_,
            mc = NA_real_,
            mc_p = NA_real_,
            d1 = NA_integer_,
            d2 = NA_integer_,
            pop_u5_1 = NA_integer_,
            pop_u5_2 = NA_integer_,
            pop_u5 = NA_integer_,
            pop_u5_i = NA_integer_,
            pop_u5_il = NA_integer_,
            pop_u5_iu = NA_integer_,
            pop_a5_1 = NA_integer_,
            pop_a5_2 = NA_integer_,
            pop_a5 = NA_integer_,
            pop_a5_i = NA_integer_,
            pop_a5_il = NA_integer_,
            pop_a5_iu = NA_integer_,
            pop = NA_integer_,
            pop_i = NA_integer_,
            pop_il = NA_integer_,
            pop_iu = NA_integer_,
            comment = NA_character_,
            roofs_count = NA_integer_,
            sample_df = list(tibble::tibble()),
            samples_sf = list(sf_empty()),
            samples_quadrat_sf = list(sf_empty()),
            cells_count = NA_integer_,
            cells_vect = list(sf_empty()),
            cells_vect_rev = list(sf_empty())
          )
      }

      sf
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
      if (is.null(sf)) {
        sf <- self$polygons

        map <- map |>
          leaflet::clearGroup('polygons')
      }

      if (nrow(sf) > 0L) {
        if (only_selected) {
          sf <- dplyr::filter(sf, selected)
          fill_opacity <- rep(0.01, nrow(sf))
        } else {
          fill_opacity <- ifelse(sf$selected, 0.2, 0.01)
        }

        map <- map |>
          addPolygons(
            highlight = FALSE,
            fill_opacity = fill_opacity,
            fit = fit,
            label = TRUE,
            sf = sf,
            group = 'polygons',
            pane = 'polygon_2',
            ...
          )

        map <- leaflet::clearGroup(map, 'cells')

        if (cells & data$project_method != 'RS_SMP') {
          for (polygon_idx in seq_len(nrow(self$polygons))) {
            if (self$polygons$selected[polygon_idx]) {
              cells_vect_rev <- self$polygons$cells_vect_rev[[polygon_idx]]

              if (nrow(cells_vect_rev) > 0L) {
                # FIXME: #8 l'image est decalee

                leaflet::addPolygons(
                  map,
                  data = cells_vect_rev,
                  color = 'white',
                  fill = TRUE,
                  group = 'cells',
                  options = leaflet::pathOptions(pane = 'polygon_1'),
                  weight = 0.5
                )

                # map <- map |>
                #   addPolygons(
                #     highlight = FALSE,
                #     fill_opacity = fill_opacity,
                #     fit = fit,
                #     label = TRUE,
                #     sf = sf,
                #     group = 'polygons',
                #     pane = 'polygon_2',
                #     ...
                #   )

                # map |>
                #   leafem::addStarsImage(
                #     stars::st_as_stars(r_polygon),
                #     colors = c(
                #       scales::alpha('white', 0.4)
                #     ),
                #     layerId = sprintf('cells_%s', polygon_idx),
                #     group = 'cells',
                #     project = FALSE,
                #     na.color = '#00000000'
                #   )
              }
            }
          }
        }

        if (sample) {
          for (polygon_idx in seq_len(nrow(self$polygons))) {
            if (self$polygons$selected[polygon_idx]) {
              displaySample(
                map,
                polygon_idx = polygon_idx,
                plan = plan,
                status = status
              )
            }
          }
        }
      } else {
        sf <- sf::st_sf(sf::st_sfc(st_bbox_polygon(self$bbox_default)))

        fitToSpatialFeatureBounds(map, sf = sf)
      }

      leafpm::clearFeatures(map)

      map
    },
    addPolygon = function(map, polygon) {
      polygon <- makeValidPolygons(
        polygon,
        id_max = st_get_id_max(self$polygons, 'ZN')
      )

      if (nrow(self$polygons) == 0L) {
        private$.polygons <- polygon
      } else {
        join_covered <- sf::st_covered_by(self$polygons, polygon, FALSE)

        if (any(join_covered)) {
          map |>
            leaflet::removeShape(layerId = self$polygons$id[join_covered])

          polygons <- polygons |>
            dplyr::filter(id %in% self$polygons$id[!join_covered])
        }

        intersect <- sf::st_intersects(polygons, polygon, FALSE)

        if (any(intersect)) {
          polygons_intersect <- self$polygons[intersect, ]

          for (i in seq_len(nrow(polygons_intersect))) {
            polygon_diff <- sf::st_geometry(polygons_intersect[i, ])
            polygon_diff <- sf::st_sf(
              geometry = sf::st_difference(polygon_diff, polygon)
            )
            polygon_diff <- makeValidPolygons(
              polygon_diff,
              id_max = st_get_id_max(self$polygons, 'ZN') + 1L
            )

            private$.polygons <- private$.polygons |>
              dplyr::filter(id != polygons_intersect[i, ]$id) |>
              dplyr::bind_rows(polygon_diff)

            map |>
              leaflet::removeShape(layerId = polygons_intersect[i, ]$id) |>
              displayPolygons(sf = polygon_diff, fit = FALSE)
          }
        }

        private$.polygons <- dplyr::bind_rows(private$.polygons, polygon)
      }

      displayPolygons(map, sf = polygon, fit = FALSE)

      step_delimit$state <- utils::modifyList(
        step_delimit$state,
        list(modified = TRUE, mode = 'select')
      )
    },
    deletePolygon = function(map, id) {
      private$.polygons <- dplyr::filter(private$.polygons, id != !!id)

      map |>
        leaflet::removeShape(layerId = id)

      step_delimit$state <- utils::modifyList(
        step_delimit$state,
        list(modified = TRUE)
      )
    },
    editPolygon = function(map, feature) {
      polygon <-
        st_as_sf.feature(feature)

      m <- match(polygon$layerId, self$polygons$id)

      if (!is.na(m)) {
        # Ensure geometry type matches existing column type
        existing_type <- sf::st_geometry_type(
          private$.polygons[m, ],
          by_geometry = TRUE
        )
        new_geom <- sf::st_geometry(polygon)
        if (
          existing_type == 'MULTIPOLYGON' &&
            sf::st_geometry_type(polygon, by_geometry = TRUE) == 'POLYGON'
        ) {
          new_geom <- sf::st_cast(new_geom, 'MULTIPOLYGON')
        }

        private$.polygons[m, ] <- private$.polygons[m, ] |>
          sf::st_set_geometry(new_geom) |>
          dplyr::mutate(
            area = sf::st_area(private$.polygons[m, ]),
            label = sprintf(
              '<b>Polygon %s</b><br>%s (km\u00b2)',
              label_short,
              round(units::set_units(area, km^2L), 2L)
            )
          )
      }

      step_delimit$state <- utils::modifyList(
        step_delimit$state,
        list(modified = TRUE)
      )
    },
    togglePolygonSelected = function(map, id) {
      m <- match(id, self$polygons$id)

      private$.polygons$selected[m] <- !self$polygons$selected[m]

      map |>
        setStyleFast(
          group = 'polygons',
          fill_opacities = as.list(ifelse(self$polygons$selected, 0.2, 0.01))
        )

      step_delimit$state <- utils::modifyList(
        step_delimit$state,
        list(modified = TRUE)
      )
    },
    togglePolygonFocused = function(map, id, group, step) {
      m <- match(id, self$polygons$id)

      if (step == 'sample') {
        private$.polygons$focused_sample[-m] <- FALSE
        private$.polygons$focused_sample[m] <- !self$polygons$focused_sample[m]

        focused <- polygons_selected$focused_sample
      } else if (step == 'result') {
        private$.polygons$focused_result[-m] <- FALSE
        private$.polygons$focused_result[m] <- !self$polygons$focused_result[m]

        focused <- polygons_selected$focused_result
      } else {
        focused <- rep(FALSE, nrow(polygons_selected))
      }

      setStylePolygonFocused(map, focused)

      for (i in seq_len(nrow(self$polygons))) {
        if (self$polygons$selected[i]) {
          leafpm::editStop(map, targetGroup = sprintf('QDR_%s', i))
        }
      }
    },
    setStylePolygonFocused = function(map, focused, group = 'polygons') {
      map |>
        setStyleFast(
          group = 'polygons',
          colors = as.list(ifelse(
            focused,
            col2hex('yellow'),
            col2hex('yellow')
          )),
          # fills = as.list(ifelse(focused, col2hex('white'), col2hex('yellow'))),
          # fill_opacities = as.list(ifelse(focused, 0.1, 0.01))
          weights = as.list(ifelse(focused, 4L, 2L))
        )
    },
    addProject = function(status, priority, name, description) {
      private$.project_status <- status
      private$.project_priority <- priority
      private$.project_name <- name
      private$.project_description <- description
      private$.project_active <- name

      project <- list(
        status = private$.project_status,
        priority = private$.project_priority,
        name = private$.project_name,
        description = private$.project_description
      )

      saveToCache(project, file = 'project.rds')

      self$load(name, force = TRUE)
    },
    projectClone = function(status, priority, name, description) {
      fs::file_copy(
        path = fs::dir_ls(
          getDirAppUsers(application$user$email, private$.project_name),
          full.names = TRUE
        ),
        new_path = getDirAppUsers(application$user$email, name)
      )

      addProject(status, priority, name, description)
    },
    projectEdit = function(status, priority, description) {
      private$.project_status <- status
      private$.project_priority <- priority
      private$.project_description <- description

      save()
    },
    projectDelete = function() {
      if (private$.project_name == 'default') {
        return()
      }

      fs::dir_delete(cache)

      projectSelect()
    },
    projectSelect = function(name = 'default') {
      self$load(name)

      step_delimit$state <- utils::modifyList(
        step_delimit$state,
        list(modified = FALSE)
      )

      saveRDS(
        list(project = private$.project_name),
        fs::path(fs::path_dir(path = data$cache), 'project.rds')
      )
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

        tile_grid <- slippymath::bbox_to_tile_grid(bbox = bbox, zoom = 13L)
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

        for (t in seq_len(70L)) {
          progress(
            value = 100L *
              ((i - 1L) /
                nrow(self$polygons) +
                t / nrow(tiles) / nrow(self$polygons))
          )

          tile <- tiles[t, ]

          if (tile_grid_intersect[t]) {
            tile_sfc <- st_bbox_polygon(tile_bbox_ll(tile$x, tile$y, 13L))

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
      if (delimit) {
        if (nrow(self$polygons) > 0L) {
          tiles <- data.frame()

          for (i in seq_len(nrow(self$polygons))) {
            polygon <- self$polygons[i, ]

            if (polygon$selected) {
              polygon_tiles <- polygonToTiles(polygon)
              polygon_tiles$polygon <- i

              if (nrow(tiles) == 0L) {
                tiles <- polygon_tiles
              } else {
                tiles <- dplyr::bind_rows(tiles, polygon_tiles)
              }
            }
          }

          if (nrow(tiles) > 0L) {
            tiles_0 <- dplyr::filter(tiles, status == 0L)
            tiles_1 <- dplyr::filter(tiles, status == -1L)

            dup <- duplicated(tiles_0 |> dplyr::select(x, y, status))

            # tiles_0  <- tiles_0 |>
            #   dplyr::mutate(
            #     status = ifelse(dup, -1L, 0L)
            #   )

            tiles <-
              dplyr::bind_rows(tiles_0, tiles_1) |>
              dplyr::arrange(polygon, n)

            if (self$data$project_method %in% c('SP_QDR', 'SP_TSQ')) {
              tiles$status[tiles$status != -1L] <- 2L
              tiles$cells[tiles$status != -1L] <- '111111111'
            }

            self$db$dbWriteTable(
              name = 'tiles',
              value = tiles,
              overwrite = TRUE
            )

            self$db$dbWriteTable(
              name = 'roofs',
              value = data.frame(
                id = character(),
                polygon = integer(),
                x = integer(),
                y = integer(),
                geometry = character(),
                stringsAsFactors = FALSE
              ),
              overwrite = TRUE
            )

            self$db$dbExecute(
              sql = 'CREATE INDEX idx_1 ON tiles (x, y, polygon)'
            )
            self$db$dbExecute(sql = 'CREATE INDEX idx_2 ON tiles (status)')
            self$db$dbExecute(sql = 'CREATE INDEX idx_3 ON tiles (locked)')
          }
        }

        step_delimit$state <- utils::modifyList(
          step_delimit$state,
          list(modified = FALSE)
        )

        step_identify$invalidatePolygons()
        step_identify$invalidateGridIdentifyStatus(force = TRUE)
        step_sample$invalidatePolygons()
        step_result$invalidatePolygons()
      }

      if (identify) {
        private$.polygons <- updateCells(private$.polygons)
        private$.polygons$roofs_count <- 0L

        roofs_sf <- self$roofs

        for (polygon_idx in as.integer(unique(roofs_sf$polygon))) {
          private$.polygons$roofs_count[polygon_idx] <- nrow(dplyr::filter(
            roofs_sf,
            polygon == polygon_idx
          ))
        }

        step_identify$state <- utils::modifyList(
          step_identify$state,
          list(modified = FALSE)
        )

        step_identify$invalidatePolygons()
        step_identify$invalidateGridIdentifyStatus(force = TRUE)
        step_sample$invalidatePolygons()
        step_result$invalidatePolygons()
      }

      if (sample) {
        step_sample$state <- utils::modifyList(
          step_sample$state,
          list(modified = FALSE)
        )

        step_sample$invalidatePolygons()
        step_result$invalidatePolygons()
      }

      if (result) {
        step_result$state <- utils::modifyList(
          step_result$state,
          list(modified = FALSE)
        )

        step_result$invalidatePolygons()
      }

      self$polygons_memento <- self$polygons

      project <- list(
        status = private$.project_status,
        priority = private$.project_priority,
        method = private$.project_method,
        name = private$.project_name,
        description = private$.project_description
      )

      saveToCache(project, file = 'project.rds')
      saveToCache(self$guide_polygon, file = 'guide_polygon.rds')
      saveToCache(self$guide_point, file = 'guide_point.rds')
      saveToCache(self$polygons, file = 'polygons.rds')
    },
    clear = function(
      delimit = FALSE,
      identify = FALSE,
      sample = FALSE,
      result = FALSE
    ) {
      if (delimit) {
        self$guide_polygon <- NULL
        self$guide_point <- NULL
        self$polygons <- NULL

        shinyjs::delay(500L, application$dlg_method$show(ns = shiny::NS(NULL)))
      }

      if (identify) {
        private$.polygons$roofs_count <- 0L
        private$.polygons$cells_count <- 0L

        db$dbExecute('DELETE FROM roofs')
        db$dbExecute(
          'UPDATE tiles SET status = 0, cells = "000000000" WHERE status <> -1'
        )

        invalidate(self$roofs_changed)
      }

      if (sample) {
        private$.polygons <- private$.polygons |>
          dplyr::mutate(
            focused_sample = FALSE,
            sample_df = list(tibble::tibble()),
            samples_sf = list(sf_empty()),
            samples_quadrat_sf = list(sf_empty())
          )
      }

      if (result) {
        for (i in seq_len(nrow(self$polygons))) {
          if (nrow(self$polygons[i, ]$samples_sf[[1L]]) > 0L) {
            private$.polygons[i, ]$samples_sf[[1L]]$d1 <- NA
            private$.polygons[i, ]$samples_sf[[1L]]$d2 <- NA
            private$.polygons[i, ]$samples_sf[[1L]]$pop_u5_1 <- NA
            private$.polygons[i, ]$samples_sf[[1L]]$pop_a5_1 <- NA
            private$.polygons[i, ]$samples_sf[[1L]]$pop_u5_2 <- NA
            private$.polygons[i, ]$samples_sf[[1L]]$pop_a5_2 <- NA
            private$.polygons[i, ]$samples_sf[[1L]]$pop_u5 <- NA
            private$.polygons[i, ]$samples_sf[[1L]]$pop_a5 <- NA
            private$.polygons[i, ]$samples_sf[[1L]]$comment <- NA
          }
        }
      }

      save(
        delimit = delimit,
        identify = identify,
        sample = sample,
        result = result
      )
    },
    rollback = function(
      delimit = FALSE,
      identify = FALSE,
      sample = FALSE,
      result = FALSE
    ) {
      self$polygons <- private$.polygons_memento

      if (delimit) {
        step_delimit$state <- utils::modifyList(
          step_delimit$state,
          list(modified = FALSE)
        )
      }

      if (identify) {
        step_identify$state <- utils::modifyList(
          step_identify$state,
          list(modified = FALSE)
        )
      }

      if (sample) {
        step_sample$state <- utils::modifyList(
          step_sample$state,
          list(modified = FALSE)
        )
      }

      if (result) {
        step_result$state <- utils::modifyList(
          step_result$state,
          list(modified = FALSE)
        )
      }
    },
    readFromCache = function(file) {
      fp <- fs::path(cache, file)
      safe_read_rds(fp, default = NULL, msg = "readFromCache")
    },
    saveToCache = function(rds, file) {
      fp <- fs::path(cache, file)
      if (is.null(rds)) {
        tryCatch(
          if (fs::file_exists(fp)) fs::file_delete(fp),
          error = function(e) {
            logWarn(
              "[saveToCache] Failed to delete %s: %s",
              fp,
              conditionMessage(e)
            )
          }
        )
      } else {
        safe_save_rds(rds, fp, msg = "saveToCache")
      }
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
    # TODO: #7 Gestion du mode de tirage aleatoire
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

      # Events
      # .................................................

      # shiny::observe({
      # if (shiny::getShinyOption('about')) {
      #   shiny::showModal(modalAboutUI(vars = vars, project = project))
      # }
      # })

      # shiny::observe({
      #   query <- shiny::parseQueryString(session$clientData$url_search)
      #   if (
      #     !is.null(query[['project']]) &
      #       !is.null(query[['user']])) {
      #     project$select(query[['project']], query[['user']])
      #   }
      # })

      profvis_file <- fs::path(
        self$cache,
        strftime(Sys.time(), '%Y-%m-%d_%H-%M-%S.Rprof')
      )

      # Shiny modules
      # .................................................

      mdl_about <- modalAbout(vars = vars, project = project)

      # tab modules
      # tabSplash(vars = vars, project = project)
      # tabSteps(vars = vars, project = project)

      application$addModule('steps', Steps$new(parent = self))

      private$.db$bind()

      # hintjs(session)

      # Bookmarks (Save states)
      # .................................................

      shiny::setBookmarkExclude(c('act_save', 'act_help'))

      shiny::onBookmark(function(state) {
        state$values$project <- project
      })

      # onBookmarked(function(url) {
      #   shiny::updateQueryString(url)
      # })

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

      # shiny::observeEvent(input$recalculated, {
      # project$recalculated <- input$recalculated
      # })

      # shiny::observe({
      # project$width <- input$dimension[1]
      # project$height <- input$dimension[2]
      # })

      # --------------------------------------------------------------------------------------
      # Inslegendtion

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
