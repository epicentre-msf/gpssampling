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
      if (is.null(polygon)) {
        status <- db$dbGetQuery(
          '
        SELECT status, COUNT(status) AS n FROM tiles WHERE status >=0 GROUP BY status UNION
        SELECT      3, COUNT()       AS n FROM tiles WHERE locked IS NOT NULL UNION
        SELECT      4, COUNT()       AS n FROM roofs'
        )
      } else {
        status <- db$dbGetQuery(
          '
        SELECT status, COUNT(status) AS n FROM tiles WHERE polygon = ? AND status >=0 GROUP BY status UNION
        SELECT      3, COUNT()       AS n FROM tiles WHERE polygon = ? AND locked IS NOT NULL UNION
        SELECT      4, COUNT()       AS n FROM roofs WHERE polygon = ?',
          params = list(polygon, polygon, polygon)
        )
      }
      status <- status |>
        dplyr::right_join(data.frame(status = 0:4), by = 'status') |>
        dplyr::mutate(n = dplyr::coalesce(n, 0L)) |>
        dplyr::arrange(status)
      status
    },
    displayGridIdentifyStatus = function(map, token, force = FALSE, ...) {
      if (self$grid_status_invalidated || force) {
        self$grid_status_invalidated <- FALSE

        map |>
          leaflet::clearGroup(group = 'grid_status')

        for (i in seq_len(nrow(self$polygons))) {
          if (self$polygons$selected[i]) {
            tx <- self$tiles |>
              dplyr::filter(polygon == i) |>
              dplyr::select(x) |>
              dplyr::distinct() |>
              dplyr::summarize(min = min(x), max = max(x), n = dplyr::n()) |>
              dplyr::collect()
            ty <- self$tiles |>
              dplyr::filter(polygon == i) |>
              dplyr::select(y) |>
              dplyr::distinct() |>
              dplyr::summarize(min = min(y), max = max(y), n = dplyr::n()) |>
              dplyr::collect()

            tilebbox1 <- tile_bbox_ll(tx$min, ty$min, zoom = 18L)
            tilebbox2 <- tile_bbox_ll(tx$max, ty$max, zoom = 18L)

            tile_sf <-
              sf::st_union(
                st_bbox_polygon(tilebbox1),
                st_bbox_polygon(tilebbox2)
              ) |>
              sf::st_sfc() |>
              sf::st_set_crs(4326L)
            # sf::st_transform(3857)

            tile_bbox <- sf::st_bbox(tile_sf)

            # FIXME: #13 Cause flash avec afficahge de la carte
            sql_df <- self$db$dbGetQuery(
              'SELECT IIF(locked IS NOT NULL AND locked <> ?, -2, status) AS status FROM tiles WHERE polygon = ?',
              params = list(token, i)
            )

            pal <- leaflet::colorNumeric(
              palette = c(
                scales::alpha('red', 0.2),
                scales::alpha('black', 0L),
                scales::alpha('white', 0.2),
                scales::alpha('yellow', 0.2),
                scales::alpha('green', 0.2),
                scales::alpha('red', 0.2)
              ),
              domain = -2:3,
              na.color = scales::alpha('black', 0L),
              alpha = TRUE
            )

            r <- terra::rast(
              names = 'status',
              xmin = tile_bbox$xmin,
              xmax = tile_bbox$xmax,
              ymin = tile_bbox$ymin,
              ymax = tile_bbox$ymax,
              ncols = tx$n,
              nrows = ty$n,
              nlyrs = 1L,
              vals = sql_df$status
            )

            terra::crs(r) <- 'epsg:4326'

            suppressWarnings(
              map |>
                addStarsImage(
                  stars::st_as_stars(r),
                  colors = pal,
                  layerId = sprintf('grid_status_%s', i),
                  group = 'grid_status',
                  project = FALSE,
                  na.color = '#00000000'
                )
              # leafem:::addGeoRaster(map,
              #   stars::st_as_stars(r),
              #     autozoom = FALSE,
              #    group = "grid_status",
              #    layerId = sprintf('grid_status_%s', i),
              #    project = FALSE
              # , colorOptions = leafem:::colorOptions(
              #   palette = pal #viridisLite::cividis
              #   , na.color = "transparent"
              # )
              # )
            )
          }
        }
      }
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
    },
    import = function() {
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
      tilenum <- slippymath::lonlat_to_tilenum(lng, lat, zoom = 18L)

      roof <-
        sf::st_sf(
          id = uuid::UUIDgenerate(),
          x = tilenum$x,
          y = tilenum$y,
          geometry = sf::st_sfc(sf::st_point(c(lng, lat)))
        ) |>
        sf::st_set_crs(4326L)

      intersect <- sf::st_intersects(roof, self$polygons, sparse = FALSE)

      if (any(intersect)) {
        roof$polygon <- match(TRUE, intersect)

        self$db$dbExecute(
          sql = 'INSERT INTO roofs VALUES(?, ?, ?, ?, ?)',
          params = list(
            roof$id,
            roof$polygon,
            roof$x,
            roof$y,
            sf::st_as_text(roof$geometry)
          )
        )

        displayRoofs(map, roofs = roof)

        invalidate(self$roofs_changed)
      }

      roof
    },
    removeRoof = function(map, roof_id, ...) {
      roof_id <- gsub('_bck', '', roof_id, fixed = TRUE)

      self$db$dbExecute(
        sql = 'DELETE FROM roofs WHERE id = ?',
        params = list(roof_id)
      )

      map |>
        leaflet::removeMarker(layerId = roof_id) |>
        leaflet::removeShape(layerId = sprintf('%s_bck', roof_id))

      invalidate(self$roofs_changed)
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
      roofs <- dplyr::bind_rows(roofs, self$roofs)

      roofs <- roofs |>
        geos::as_geos_geometry() |>
        geos::geos_make_valid() |>
        geos::geos_unique_points() |>
        geos::geos_unnest(keep_multi = FALSE) |>
        sf::st_as_sf() |>
        dplyr::bind_cols(sf::st_set_geometry(roofs, NULL))

      roofs_cc <- sf::st_coordinates(roofs)
      roofs_cc[, 1L] <- round(roofs_cc[, 1L], 6L)
      roofs_cc[, 2L] <- round(roofs_cc[, 2L], 6L)
      roofs_cc <- roofs_cc |> as.data.frame() |> dplyr::distinct()

      roofs <- roofs[as.integer(row.names(roofs_cc)), ]

      intersect <- sf::st_intersects(roofs, self$polygons, sparse = FALSE)
      intersect <- apply(intersect, 1L, any)

      roofs <- roofs[intersect, ]

      if (nrow(roofs) > 0L) {
        roofs$polygon <- as.integer(sf::st_within(roofs, self$polygons))

        tiles_roofs <- pointsToTiles(self$polygons, roofs)

        tiles <- db$dbGetQuery('SELECT * from tiles')
        tiles <- tiles |>
          dplyr::left_join(
            dplyr::select(tiles_roofs, x, y, cells),
            by = c('x', 'y')
          ) |>
          dplyr::mutate(
            cells = ifelse(status == 0L, cells.y, cells.x),
            status = ifelse(status == 0L, 1L, status)
          ) |>
          dplyr::select(-cells.x, -cells.y) |>
          tibble::as_tibble()

        self$db$dbWriteTable(name = 'tiles', value = tiles, overwrite = TRUE)

        roofs_xy <- sf::st_coordinates(roofs)

        tilenum <- slippymath::lonlat_to_tilenum(
          roofs_xy[, 1L],
          roofs_xy[, 2L],
          zoom = 18L
        )

        roofs <-
          tibble::tibble(
            id = uuid::UUIDgenerate(n = nrow(roofs)),
            polygon = roofs$polygon,
            x = as.integer(tilenum$x),
            y = as.integer(tilenum$y),
            geometry = sf::st_as_text(roofs$geometry)
          )

        self$db$dbWriteTable(name = 'roofs', value = roofs, overwrite = TRUE)

        step_identify$state <- utils::modifyList(
          step_identify$state,
          list(modified = TRUE)
        )
        step_identify$invalidatePolygons()
        step_identify$invalidateGridIdentifyStatus(force = TRUE)
      }
    },
    addSample = function(map = NULL) {
      cells_vect <- self$polygon_focused$cells_vect[[1L]]

      if (nrow(cells_vect) == 0L) {
        return()
      }

      polygon_pt <- st_sample(polygons = cells_vect, n = 1L, type = 'random')

      sample_sf <-
        polygon_pt |>
        sf::st_as_sf()

      sample_sf$id <- 1L
      sample_sf$id <- 1L

      if ('x' %in% names(sample_sf)) {
        sample_sf <- sample_sf |>
          dplyr::rename(geometry = x)
      }

      if (!is.null(map)) {
        map |>
          leaflet::clearGroup(group = 'sample') |>
          leaflet::addCircles(
            color = 'white',
            data = sample_sf,
            group = 'sample',
            fill = TRUE,
            fillColor = 'white',
            fillOpacity = 0.1,
            layerId = 'sample_pt_buffer',
            options = leaflet::pathOptions(pane = 'marker'),
            opacity = 0.5,
            radius = data$settings$getValue('sli_sample_radius'),
            stroke = TRUE,
            weight = 1L
          ) |>
          leaflet::addCircles(
            data = sample_sf,
            group = 'sample',
            fill = TRUE,
            fillColor = 'white',
            fillOpacity = 1L,
            layerId = 'sample_pt',
            options = leaflet::pathOptions(pane = 'marker'),
            radius = 1L,
            stroke = FALSE
          ) |>
          leaflet::setView(
            lng = sample_sf$geometry[[1L]][1L],
            lat = sample_sf$geometry[[1L]][2L],
            zoom = data$settings$getValue('sli_sample_zoom'),
            options = list(animate = FALSE)
          )
      }

      private$.sample <- sample_sf

      TRUE
    },
    addSamples = function(map, sf, ...) {
      generateSamples(map, samples = sf)
    },
    displayRoofs = function(
      map,
      roofs,
      tile_x = roofs$x[1L],
      tile_y = roofs$y[1L]
    ) {
      grid_tile_bbox <- tile_bbox_ll(tile_x, tile_y, 18L)
      grid_tile_bbox[1L] <- grid_tile_bbox[1L] - 0.0001
      grid_tile_bbox[2L] <- grid_tile_bbox[2L] - 0.0001
      grid_tile_bbox[3L] <- grid_tile_bbox[3L] + 0.0001
      grid_tile_bbox[4L] <- grid_tile_bbox[4L] + 0.0001

      identify_tile_bbox_sf <- st_bbox_polygon(grid_tile_bbox)
      identify_tile_intersect <- sf::st_intersects(
        identify_tile_bbox_sf,
        roofs,
        FALSE
      )

      roofs_back <- roofs[identify_tile_intersect, ] |>
        dplyr::mutate(
          id = ifelse(x == tile_x & y == tile_y, sprintf('%s_bck', id), id)
        )

      map |>
        leaflet::addCircles(
          data = roofs_back,
          group = 'identify_building',
          layerId = ~id,
          color = settings$getValue('csi_circle_color'),
          fillColor = settings$getValue('csi_circle_color'),
          fillOpacity = 0.01 + settings$getValue('sli_circle_opacity') / 100L,
          options = leaflet::pathOptions(pane = 'marker'),
          radius = settings$getValue('sli_circle_radius'),
          stroke = TRUE,
          weight = 3L
        )

      map
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
      if (is.null(polygon_idx)) {
        polygon_idx <- match(TRUE, self$polygons$focused_sample)
      }

      if (is.na(polygon_idx)) {
        return()
      }

      if (!is.null(samples)) {
        intersect <- sf::st_intersects(
          samples,
          private$.polygons[polygon_idx, ],
          sparse = FALSE
        )
        intersect <- apply(intersect, 1L, any)

        samples <- samples[intersect, ]

        type <- data$project_method

        size <- nrow(samples)

        if (size == 0L) {
          return()
        }
      }

      private$.polygons$type[polygon_idx] <- type
      private$.polygons$method[polygon_idx] <- method
      private$.polygons$count[polygon_idx] <- count

      if (type == 'SP_QDR') {
        private$.polygons$size[polygon_idx] <- size
      }

      polygon <- self$polygons[polygon_idx, ]
      polygon_id_n <- polygon$id_n

      polygon <- polygon |>
        sf::st_transform(3857L)

      if (type == 'SP_QDR') {
        polygon_buffered <- sf::st_buffer(polygon, dist = -size / 2L)
        if (!sf::st_is_empty(polygon_buffered)) {
          polygon <- polygon_buffered
        }
      }

      if (is.null(samples)) {
        if (type == 'RS_SMP') {
          polygon_roofs <- dplyr::filter(roofs, polygon == polygon_idx)
          polygon_roofs <- polygon_roofs |>
            sf::st_set_crs(4326L) |>
            sf::st_transform(3857L) |>
            dplyr::select(-id)

          polygon_pts <- polygon_roofs[
            sample(seq_len(nrow(polygon_roofs)), count),
          ]
        } else {
          if (type %in% c('SP_QDR', 'SP_TSQ')) {
            cells_vect <- self$polygons[polygon_idx, ]
          } else {
            cells_vect <- self$polygons[polygon_idx, ]$cells_vect[[1L]]
          }

          cells_r_pts_sf <- st_sample(
            polygons = cells_vect,
            n = count,
            type = method
          )
          # cells_r_res <- terra::res(cells_rst) / 2
          # cells_r_pts <- tibble::as_tibble(terra::spatSample(cells_vect, size = count, method = 'random', replace = TRUE, na.rm = TRUE, values = FALSE, xy = TRUE))
          # cells_r_pts$x <- cells_r_pts$x + runif(count, -cells_r_res[1], cells_r_res[1])
          # cells_r_pts$y <- cells_r_pts$y + runif(count, -cells_r_res[2], cells_r_res[2])
          # cells_r_pts_sf <- sf::st_sf(geometry = sf::st_sfc(sf::st_multipoint(x = as.matrix(cells_r_pts), dim = 'XY')), crs = sf::st_crs(4326))

          polygon_pts <-
            sf::st_cast(cells_r_pts_sf, 'POINT') |>
            sf::st_transform(3857L)
          polygon_pts_n <- nrow(polygon_pts)
        }
      } else {
        if (!(type %in% c('SP_QDR', 'SP_TSQ'))) {
          equals <- sf::st_equals_exact(
            samples,
            private$.polygons$samples_sf[[polygon_idx]],
            sparse = FALSE,
            par = 0.000001
          )
          equals <- apply(equals, 1L, any)

          samples <- samples[!equals, ]
        }

        size <- nrow(samples)

        if (size == 0L) {
          return()
        }

        polygon_pts <- samples |>
          sf::st_transform(3857L)
      }

      samples_sf <-
        polygon_pts |>
        sf::st_as_sf() |>
        sf::st_set_crs(3857L) |>
        sf::st_transform(4326L)

      # if ('x' %in% names(samples_sf)) {
      #   samples_sf <- samples_sf |>
      #     dplyr::rename(geometry = x)
      # }

      samples_sf_names <- names(samples_sf)

      if (!('id_n' %in% samples_sf_names))
        samples_sf <- dplyr::mutate(samples_sf, id_n = seq_len(dplyr::n()))
      if (!('id_key' %in% samples_sf_names))
        samples_sf <- dplyr::mutate(samples_sf, id_key = NA_character_)
      if (!('id_key_calc' %in% samples_sf_names))
        samples_sf <- dplyr::mutate(
          samples_sf,
          id_key_calc = key(polygon_idx, id_n, project_method)
        )
      if (!('id' %in% samples_sf_names))
        samples_sf <- dplyr::mutate(
          samples_sf,
          id = sprintf('%0.2d/%0.4d', polygon_idx, id_n)
        )
      if (!('id_user' %in% samples_sf_names))
        samples_sf <- dplyr::mutate(samples_sf, id_user = id)
      if (!('color' %in% samples_sf_names))
        samples_sf <- dplyr::mutate(samples_sf, color = 'yellow')
      if (!('area' %in% samples_sf_names))
        samples_sf <- dplyr::mutate(samples_sf, area = NA_integer_)
      if (!('d1' %in% samples_sf_names))
        samples_sf <- dplyr::mutate(samples_sf, d1 = NA_integer_)
      if (!('d2' %in% samples_sf_names))
        samples_sf <- dplyr::mutate(samples_sf, d2 = NA_integer_)
      if (!('pop_u5_1' %in% samples_sf_names))
        samples_sf <- dplyr::mutate(samples_sf, pop_u5_1 = NA_integer_)
      if (!('pop_a5_1' %in% samples_sf_names))
        samples_sf <- dplyr::mutate(samples_sf, pop_a5_1 = NA_integer_)
      if (!('pop_u5_2' %in% samples_sf_names))
        samples_sf <- dplyr::mutate(samples_sf, pop_u5_2 = NA_integer_)
      if (!('pop_a5_2' %in% samples_sf_names))
        samples_sf <- dplyr::mutate(samples_sf, pop_a5_2 = NA_integer_)
      if (!('pop_u5' %in% samples_sf_names))
        samples_sf <- dplyr::mutate(samples_sf, pop_u5 = NA_integer_)
      if (!('pop_a5' %in% samples_sf_names))
        samples_sf <- dplyr::mutate(samples_sf, pop_a5 = NA_integer_)
      if (!('status' %in% samples_sf_names))
        samples_sf <- dplyr::mutate(samples_sf, status = 'C1')
      if (!('comment' %in% samples_sf_names))
        samples_sf <- dplyr::mutate(samples_sf, comment = NA_character_)

      samples_sf <- samples_sf |>
        dplyr::mutate(
          type = type,
          polygon = polygon_id_n
        ) |>
        dplyr::select(
          id_n,
          id_key,
          id_key_calc,
          id,
          id_user,
          color,
          area,
          d1,
          d2,
          pop_u5_1,
          pop_a5_1,
          pop_u5_2,
          pop_a5_2,
          pop_u5_1,
          pop_u5,
          pop_a5,
          status,
          comment
        )

      polygon_xy <- sf::st_coordinates(samples_sf)

      samples_sf <- sf::st_set_geometry(samples_sf, NULL)
      samples_sf$lon <- round(polygon_xy[, 1L], 6L)
      samples_sf$lat <- round(polygon_xy[, 2L], 6L)

      samples_sf <- samples_sf |>
        sf::st_as_sf(coords = c('lon', 'lat'), remove = FALSE) |>
        sf::st_set_crs(4326L)

      if (type == 'SP_QDR') {
        quadrat <-
          matrix(
            c(
              0L,
              0L,
              size,
              0L,
              size,
              size,
              0L,
              size,
              0L,
              0L
            ),
            ncol = 2L,
            byrow = TRUE
          ) -
          size / 2L

        polygon_xy <- samples_sf |>
          sf::st_transform(3857L) |>
          sf::st_coordinates()

        sample_quadrats_sfc <-
          lapply(seq_len(nrow(polygon_xy)), function(i) {
            quadrat[, 1L] <- quadrat[, 1L] + polygon_xy[i, 1L]
            quadrat[, 2L] <- quadrat[, 2L] + polygon_xy[i, 2L]
            sf::st_polygon(list(quadrat))
          }) |>
          sf::st_sfc() |>
          sf::st_set_crs(3857L) |>
          sf::st_transform(4326L)

        samples_quadrat_sf <-
          sf::st_sf(
            geometry_type = 'square',
            geometry = sample_quadrats_sfc
          ) |>
          dplyr::mutate(
            area = size^2L,
            id_n = seq_len(dplyr::n()),
            id = sprintf('Q%0.2d/%0.4d', polygon_idx, id_n),
            id_key = NA_character_,
            color = 'yellow'
          )

        samples_sf$area <- samples_quadrat_sf$area
        #
        # addQuadrats(map, sf = dplyr::filter(private$.polygons, selected)rats_sf, clear = TRUE)
      } else {
        samples_quadrat_sf <- sf_empty()
      }

      # # dplyr::filter(private$.polygons, selected)roids_sf <-
      # #   sf::st_sample(vars$polygon_sf, rep(n, nrow(vars$polygon_sf))) |>
      # #   sf::st_sf(polygons = rep(seq_len(nrow(vars$polygon_sf), each = n), geometry = .) |>
      # #   sf::st_transform(4326)
      # vars$sample_invalidate <- vars$sample_invalidate + 1

      # shinyjs::enable(id = 'act_generate_ok')

      private$.polygons$samples_sf[[polygon_idx]] <- samples_sf
      private$.polygons$samples_quadrat_sf[[polygon_idx]] <- samples_quadrat_sf

      if (!is.null(map)) {
        displaySample(map, polygon_idx = polygon_idx, status = FALSE)
      }

      step_sample$state <- utils::modifyList(
        step_sample$state,
        list(modified = TRUE)
      )

      list(
        count = nrow(samples_sf),
        quadrat = samples_quadrat_sf
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
      if (fs::file_exists(fp)) {
        rds <- readRDS(fp)
      } else {
        rds <- NULL
      }
      rds
    },
    saveToCache = function(rds, file) {
      fp <- fs::path(cache, file)
      if (is.null(rds)) {
        if (fs::file_exists(fp)) {
          fs::file_delete(fp)
        }
      } else {
        saveRDS(rds, fp)
      }
    },
    sampleAppend = function(map = NULL, polygon_idx = NULL) {
      if (is.null(polygon_idx)) {
        polygon_idx <- match(TRUE, self$polygons$focused_sample)
      }

      if (is.na(polygon_idx)) {
        return()
      }

      if (is.null(private$.sample)) {
        return()
      }

      private$.polygons$type[polygon_idx] <- 'SP_SPV'
      # private$.polygons$method[polygon_idx] <- 'SP_SPV'
      # private$.polygons$count[polygon_idx] <- count
      # private$.polygons$size[polygon_idx] <- size

      samples_count <- nrow(self$polygons$samples_sf[[polygon_idx]])

      sample_xy <- sf::st_coordinates(private$.sample)

      sample_sf <- private$.sample
      sample_sf$lon <- round(sample_xy[, 1L], 6L)
      sample_sf$lat <- round(sample_xy[, 2L], 6L)
      sample_sf <- sample_sf |>
        dplyr::mutate(
          type = 'SP_SPV',
          polygon = polygon_idx,
          id_n = samples_count + 1L,
          id_key = NA_character_,
          id_key_calc = key(polygon_idx, id_n, project_method),
          id = sprintf('%0.2d/%0.4d', polygon_idx, id_n),
          id_user = id,
          color = 'yellow',
          area = NA_integer_,
          d1 = NA_integer_,
          d2 = NA_integer_,
          pop_u5_1 = NA_integer_,
          pop_a5_1 = NA_integer_,
          pop_u5_2 = NA_integer_,
          pop_a5_2 = NA_integer_,
          pop_u5_1 = NA_integer_,
          pop_u5 = NA_integer_,
          pop_a5 = NA_integer_,
          status = 'C1',
          comment = NA_character_
        )

      if (samples_count >= 1L) {
        private$.polygons$samples_sf[[
          polygon_idx
        ]] <- private$.polygons$samples_sf[[polygon_idx]] |>
          dplyr::bind_rows(sample_sf)
      } else {
        private$.polygons$samples_sf[[polygon_idx]] <- sample_sf
      }

      if (!is.null(map)) {
        displaySample(
          map,
          polygon_idx = polygon_idx,
          samples_idx = samples_count + 1L,
          status = FALSE
        )
      }

      step_sample$state <- utils::modifyList(
        step_sample$state,
        list(modified = TRUE)
      )

      private$.sample <- NULL

      TRUE
    },
    calculateSample = function(progress = NULL) {
      polygon_selected <- match(TRUE, self$polygons$focused_result)

      if (is.na(polygon_selected)) {
        return()
      }

      df <- self$polygons$samples_sf[[polygon_selected]]

      if (nrow(df) > 0L) {
        if (project_method == 'SP_QDR') {
          r_u5 <- calculateQuadrat(
            qdr_pop = as.integer(df$pop_u5),
            qdr_size = as.integer(self$polygons[polygon_selected, ]$size),
            area = as.integer(self$polygons[polygon_selected, ]$area),
            error_confidence = 95L
          )

          r_a5 <- calculateQuadrat(
            qdr_pop = as.integer(df$pop_a5),
            qdr_size = as.integer(self$polygons[polygon_selected, ]$size),
            area = as.integer(self$polygons[polygon_selected, ]$area),
            error_confidence = 95L
          )

          r_t <- calculateQuadrat(
            qdr_pop = as.integer(df$pop_u5) + as.integer(df$pop_a5),
            qdr_size = as.integer(self$polygons[polygon_selected, ]$size),
            area = as.integer(self$polygons[polygon_selected, ]$area),
            error_confidence = 95L
          )

          private$.polygons[polygon_selected, ]$pop_u5_i <- r_u5$pop_i
          private$.polygons[polygon_selected, ]$pop_u5 <- r_u5$pop
          private$.polygons[polygon_selected, ]$pop_a5_i <- r_a5$pop_i
          private$.polygons[polygon_selected, ]$pop_a5 <- r_a5$pop
          private$.polygons[polygon_selected, ]$pop_i <- r_t$pop_i
          private$.polygons[polygon_selected, ]$pop <- r_t$pop
          private$.polygons[polygon_selected, ]$tvalue <- r_u5$tvalue
          private$.polygons[polygon_selected, ]$t <- NA
          private$.polygons[polygon_selected, ]$z <- NA
          private$.polygons[polygon_selected, ]$z_p <- NA
          private$.polygons[polygon_selected, ]$mc <- NA
          private$.polygons[polygon_selected, ]$mc_p <- NA
        } else if (project_method == 'POL') {
          r_u5 <- calculatePolygonat(
            qdr_pop = as.integer(df$pop_u5),
            qdr_area = as.integer(df$area),
            area = as.integer(self$polygons[polygon_selected, ]$area),
            error_confidence = 95L
          )

          r_a5 <- calculatePolygonat(
            qdr_pop = as.integer(df$pop_a5),
            qdr_area = as.integer(df$area),
            area = as.integer(self$polygons[polygon_selected, ]$area),
            error_confidence = 95L
          )

          r_t <- calculatePolygonat(
            qdr_pop = as.integer(df$pop_u5) + as.integer(df$pop_a5),
            qdr_area = as.integer(df$area),
            area = as.integer(self$polygons[polygon_selected, ]$area),
            error_confidence = 95L
          )

          private$.polygons[polygon_selected, ]$pop_u5_i <- r_u5$pop_i
          private$.polygons[polygon_selected, ]$pop_u5 <- r_u5$pop
          private$.polygons[polygon_selected, ]$pop_a5_i <- r_a5$pop_i
          private$.polygons[polygon_selected, ]$pop_a5 <- r_a5$pop
          private$.polygons[polygon_selected, ]$pop_i <- r_t$pop_i
          private$.polygons[polygon_selected, ]$pop <- r_t$pop
          private$.polygons[polygon_selected, ]$tvalue <- r_u5$tvalue
          private$.polygons[polygon_selected, ]$t <- NA
          private$.polygons[polygon_selected, ]$z <- NA
          private$.polygons[polygon_selected, ]$z_p <- NA
          private$.polygons[polygon_selected, ]$mc <- NA
          private$.polygons[polygon_selected, ]$mc_p <- NA
        } else if (project_method == 'SP_TSQ') {
          r_u5 <- calculateTSquare(
            d1 = df$d1,
            d2 = df$d2,
            p1 = df$pop_u5_1,
            p2 = df$pop_u5_2,
            area = as.integer(self$polygons[polygon_selected, ]$area),
            error_confidence = 95L
          )

          r_a5 <- calculateTSquare(
            d1 = df$d1,
            d2 = df$d2,
            p1 = df$pop_a5_1,
            p2 = df$pop_a5_2,
            area = as.integer(self$polygons[polygon_selected, ]$area),
            error_confidence = 95L
          )

          r_t <- calculateTSquare(
            d1 = df$d1,
            d2 = df$d2,
            p1 = df$pop_u5_1 + df$pop_a5_1,
            p2 = df$pop_u5_1 + df$pop_a5_2,
            area = as.integer(self$polygons[polygon_selected, ]$area),
            error_confidence = 95L
          )

          private$.polygons[polygon_selected, ]$pop_u5_i <- r_u5$pop_i
          private$.polygons[polygon_selected, ]$pop_u5 <- r_u5$pop
          private$.polygons[polygon_selected, ]$pop_a5_i <- r_a5$pop_i
          private$.polygons[polygon_selected, ]$pop_a5 <- r_a5$pop
          private$.polygons[polygon_selected, ]$pop_i <- r_t$pop_i
          private$.polygons[polygon_selected, ]$pop <- r_t$pop
          private$.polygons[polygon_selected, ]$tvalue <- NA
          private$.polygons[polygon_selected, ]$t <- r_u5$t
          private$.polygons[polygon_selected, ]$z <- r_u5$z
          private$.polygons[polygon_selected, ]$z_p <- r_u5$z_p
          private$.polygons[polygon_selected, ]$mc <- r_u5$mc
          private$.polygons[polygon_selected, ]$mc_p <- r_u5$mc_p
        } else {
          count <- self$polygons$roofs_count[polygon_selected]

          if (count == 0L) {
            return()
          }

          shiny::withProgress(
            message = 'Calculation in progress',
            max = 3000L,
            {
              pop_u5 <- df$pop_u5[!is.na(df$pop_u5) & !is.na(df$pop_a5)]
              pop_a5 <- df$pop_a5[!is.na(df$pop_u5) & !is.na(df$pop_a5)]

              if (length(pop_u5) == 0L) {
                pop_u5 <- 0L
              }

              if (length(pop_a5) == 0L) {
                pop_a5 <- 0L
              }

              r_u5 <- calculateSampleZeroInflatedDistribution(
                smp_pop = pop_u5,
                n = count,
                error_confidence = 95L,
                progress = progress
              )

              r_a5 <- calculateSampleZeroInflatedDistribution(
                smp_pop = pop_a5,
                n = count,
                error_confidence = 95L,
                progress = progress
              )

              r_t <- calculateSampleZeroInflatedDistribution(
                smp_pop = pop_u5 + pop_a5,
                n = count,
                error_confidence = 95L,
                progress = progress
              )
            }
          )

          private$.polygons[
            polygon_selected,
          ]$pop_u5_il <- as.integer(r_u5$pop_i[1L])
          private$.polygons[
            polygon_selected,
          ]$pop_u5_iu <- as.integer(r_u5$pop_i[2L])
          private$.polygons[polygon_selected, ]$pop_u5 <- r_u5$pop
          private$.polygons[
            polygon_selected,
          ]$pop_a5_il <- as.integer(r_a5$pop_i[1L])
          private$.polygons[
            polygon_selected,
          ]$pop_a5_iu <- as.integer(r_a5$pop_i[2L])
          private$.polygons[polygon_selected, ]$pop_a5 <- r_a5$pop
          private$.polygons[polygon_selected, ]$pop_il <- as.integer(r_t$pop_i[
            1L
          ])
          private$.polygons[polygon_selected, ]$pop_iu <- as.integer(r_t$pop_i[
            2L
          ])
          private$.polygons[polygon_selected, ]$pop <- r_t$pop
          private$.polygons[polygon_selected, ]$tvalue <- NA
          private$.polygons[polygon_selected, ]$t <- NA
          private$.polygons[polygon_selected, ]$z <- NA
          private$.polygons[polygon_selected, ]$z_p <- NA
          private$.polygons[polygon_selected, ]$mc <- NA
          private$.polygons[polygon_selected, ]$mc_p <- NA
        }
      }

      step_result$state <- utils::modifyList(
        step_result$state,
        list(modified = TRUE, void = rnorm(1L))
      )
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
      if (is.na(polygon_idx)) {
        return()
      }

      group_centroid <- sprintf('PTS_%s', polygon_idx)
      group_quadrat <- sprintf('QDR_%s', polygon_idx)

      samples <- self$polygons$samples_sf[[polygon_idx]]
      samples_quadrats <- self$polygons$samples_quadrat_sf[[polygon_idx]]

      if (is.defined(samples_idx)) {
        samples <- samples[samples_idx, ]
        samples_quadrats <- samples_quadrats[samples_idx, ]

        map |>
          leaflet::removeShape(layerId = samples$id) |>
          leaflet::removeMarker(layerId = samples$id)
      } else {
        map |>
          leaflet::clearGroup(group_centroid) |>
          leaflet::clearGroup(sprintf('%s_lbl', group_centroid)) |>
          leaflet::clearGroup(group_quadrat)
      }

      if (nrow(samples) > 0L) {
        if (!status) {
          samples$color <- 'yellow'
        }

        map |>
          leaflet::clearGroup(group = 'sample') |>
          leaflet::addCircleMarkers(
            data = samples,
            group = group_centroid,
            color = 'yellow',
            fill = TRUE,
            fillColor = ~color,
            fillOpacity = 1L,
            label = ~id_user,
            labelOptions = leaflet::labelOptions(
              noHide = FALSE,
              direction = 'top',
              offset = c(0L, -6L)
            ),
            layerId = ~id,
            opacity = 1L,
            options = leaflet::pathOptions(pane = 'marker'),
            radius = 5L,
            stroke = TRUE,
            weight = 1L
          )

        if (self$polygons$type[polygon_idx] == 'SP_QDR') {
          map |>
            leaflet::addPolygons(
              data = samples_quadrats,
              color = 'yellow',
              fill = TRUE,
              fillColor = 'yellow',
              fillOpacity = 0.2,
              # label = ~ id_key,
              layerId = ~id,
              stroke = TRUE,
              weight = 1L,
              opacity = 1L,
              group = group_quadrat
            )
        }

        if (fit) {
          map |>
            fitToSpatialFeatureBounds(sf = self$polygons[polygon_idx, ])
        }
      }

      map
    },
    sampleDisplayToTable = function(
      table_id,
      polygon_idx,
      samples_idx = NULL,
      plan = FALSE,
      tooltip = FALSE,
      ...
    ) {
      if (is.na(polygon_idx)) {
        return()
      }

      samples <- self$polygons$samples_sf[[polygon_idx]]

      if (is.defined(samples_idx)) {
        samples <- samples[samples_idx, ]
      } else {
      }

      if (nrow(sample) > 0L) {
        if (nrow(sample) > 1L) {
        } else {
          rhandsontable::set_data(
            id = table_id,
            row = 1L,
            col = 4L,
            val = 1L,
            session,
            zero_indexed = FALSE
          )
        }
      }

      map
    },
    sampleExportFilename = function(extension) {
      polygon <- polygons[1L, ]
      if (is.defined(polygon)) {
        file <- df_sampling_method[polygon$type, 'label']
      } else {
        file <- df_sampling_method['SP_SMP', 'label']
      }
      file <- sprintf(
        '%s %s - %s.%s',
        file,
        tolower(polygon$id),
        Sys.Date(),
        extension
      )
      file
    },
    sampleExportToDoc = function(file, open = TRUE, polygon_idx = NULL) {
      if (is.null(polygon_idx)) {
        sf_pol <- polygon_focused
        sf_pts <- samples
      } else {
        sf_pol <- polygons[polygon_idx, ]
        sf_pts <- sf_pol$samples_sf[[1L]]
      }

      if (nrow(sf_pts) == 0L) {
        return()
      }

      sf_pts_cc <- sf::st_coordinates(sf_pts)

      # samples <- polygon$samples_sf[[1]] |> dplyr::filter(focused)
      # samples_sf <- sf::st_as_sf(samples_sf, coords = c('lon', 'lat'), crs = 4326)

      # samples_sf <- samples_sf |>
      #   dplyr::select(
      # polygons,
      # type,
      # id_n,
      # id,
      # id_key,
      # color,
      # lon,
      # lat,
      # area,
      # d1,
      # d2,
      # pop_u5_1,
      # pop_a5_1,
      # pop_u5_2,
      # pop_a5_2,
      #   pop_u5,
      #   pop_a5,
      #   comment
      # )

      bb <- sf::st_bbox(sf_pts)

      # Calculate A4 bbox
      bbw <- abs(bb[['xmin']] - bb[['xmax']])
      bbh <- abs(bb[['ymin']] - bb[['ymax']])

      if (bbw > bbh) {
        # Landscape
        bbh_a4 <- (bbw / 297L) * 210L
        bb[['ymin']] <- bb[['ymin']] - (bbh_a4 - bbh) / 2L
        bb[['ymax']] <- bb[['ymax']] + (bbh_a4 - bbh) / 2L
      } else {
        # Portrait
      }

      grDevices::png(
        'my_plot.png',
        width = 297L,
        height = 210L,
        units = 'mm',
        res = 96L
      )

      # p <- ggmap(bb, max_tiles = 100)
      plotMap(bb)
      plot(
        st_geometry(sf_pol),
        border = scales::alpha('yellow', 0.5),
        lwd = 5L,
        add = TRUE
      )
      plot(st_geometry(sf_pol), border = 'yellow', lwd = 2L, add = TRUE)
      plot(sf_pts, add = TRUE, pch = 16L, col = 'yellow')

      for (i in seq_len(nrow(sf_pts_cc))) {
        text(
          x = sf_pts_cc[i, 1L],
          y = sf_pts_cc[i, 2L],
          labels = i,
          pos = 3L,
          cex = 0.75,
          col = 'white'
        )
      }

      grDevices::dev.off()

      # header <- data.frame(col = names(samples_sf))
      # header$header.1 <- c(
      #   rep(.('Sample'), 3),
      #   rep(.('Geometry'), 3),
      #   rep(.('Population'), 2),
      #   rep(.('Comment'), 1)
      # )
      # header$header.2 <- c(
      #   'Type', 'N<U+FFFD>', 'ID',
      #   'Lat.', 'Lon.', 'Area',
      #   '<5y', '>=5y',
      #   ''
      # )

      doc <- createDocument(template = 'default.portrait')
      doc <- doc |>
        flextable::body_add_flextable(value = tbl, align = 'left') |>
        generateReport(filename = 'doc.docx', open = TRUE)

      #   flextable::set_header_df(mapping = header, key = 'col') |>
      #   flextable::merge_h(part = 'header', i = 1)
      #   flextable::merge_v(part = 'body', j = 'adm_1_name')
      # #    |> flextable::merge_v(part = 'body', j = 'cse_ti_spkl')

      doc <- createDocument(template = 'samples')
      doc <- doc |>
        officer::body_replace_img_at_bkm(
          bookmark = 'MAP',
          value = officer::external_img(
            src = fs::path(getwd(), 'my_plot.jpg'),
            width = units::set_units(units::set_units(145L, mm), inches),
            height = units::set_units(units::set_units(145L, mm), inches)
          )
        ) |>
        generateReport(filename = 'doc.docx', open = TRUE)

      # tbl <- tbl |>
      #   flextable::colformat_num(col_keys = c('lon', 'lat'), digits = 5) |>
      #   flextable::colformat_num(col_keys = 'area', digits = 0) |>
      #   flextable::color(part = 'footer', color = 'grey') |>
      #   flextable::fontsize(part = 'all', size = 7) |>
      #   flextable::fontsize(part = 'footer', size = 6) |>
      #   flextable::padding(part = 'all', padding = 2) |>
      #   flextable::rotate(part = 'header', align = 'top', rotation = 'lrtb') |>
      #   flextable::rotate(part = 'body', align = 'top', rotation = 'lrtb') |>
      #   flextable::align(part = 'all', align = 'right') |>
      #   flextable::align(part = 'all', align = 'left', j = 1:2) |>
      #   flextable::border(part = 'all', border = officer::fp_border(width = 0)) |>
      #   flextable::border(part = 'body', border.bottom = officer::fp_border(width = .5, color = 'grey', style = 'dotted')) |>
      #   flextable::border(part = 'header', border.bottom = officer::fp_border(width = 1, color = 'grey', style = 'dotted'), i = 1, j = 1:2) |>
      #   # flextable::border(part = 'header', border.bottom = officer::fp_border(width = 1, color = 'grey', style = 'dotted'), i = 1, j = 4:9) |>
      #   # flextable::border(part = 'header', border.bottom = officer::fp_border(width = 1, color = 'grey', style = 'dotted'), i = 1, j = 11:12) |>
      #   # flextable::border(part = 'header', border.bottom = officer::fp_border(width = 1, color = 'grey', style = 'dotted'), i = 1, j = 14:15) |>
      #   # flextable::border(part = 'header', border.bottom = officer::fp_border(width = 1, color = 'grey', style = 'dotted'), i = 1, j = c('dth_cfr')) |>
      #   # flextable::border(part = 'header', border.bottom = officer::fp_border(width = 1, color = 'grey'), i = 2) |>
      #   flextable::bold(part = 'header', i = 1) |>
      #   flextable::italic(part = 'footer', italic = TRUE) |>
      #   flextable::height(height = 0.3, part = 'header', i = 1) |>
      #   flextable::height(height = 0.18, part = 'header', i = 2) |>
      #   flextable::height(height = 0.18, part = 'body') |>
      #   #  flextable::width(width = 0.9, j = 'adm_1_name') |>
      #   #  flextable::width(width = 0.9, j = 'adm_2_name') |>
      #   #   flextable::width(width = 0.45, j = 4:7) |>
      #   #   flextable::width(width = 0.4, j = 'cse_n_1_4') |>
      #   #   flextable::width(width = 0.7, j = 'all') |>
      #   #   flextable::width(width = 0.4, j = c('cse_ta_old', 'cse_ta', 'cse_ti')) |>
      #   #   flextable::width(width = 0.3, j = c('dth_cfr')) |>
      #   #   flextable::width(width = 0.1, j = c('sep_1', 'sep_2', 'sep_3', 'sep_4'))

      doc <- createDocument(template = 'samples')
      doc <- doc |>
        officer::body_replace_img_at_bkm(
          bookmark = 'MAP',
          value = officer::external_img(
            src = fs::path(getwd(), 'my_plot.jpg'),
            width = units::set_units(units::set_units(145L, mm), inches),
            height = units::set_units(units::set_units(145L, mm), inches)
          )
        ) |>
        generateReport(filename = 'doc.docx', open = TRUE)
      # doc <- officer::body_end_section_landscape(doc)

      # log_debug('Report %s|Analyse globale...|10', disease_s, progress = progress)

      # grDevices::graphics.off()

      # doc <- addTitle(doc, 'Situation des districts en alerte')
      # doc <- addTitle(doc, sprintf("Taux d'attaque par district en alerte lors de la derni<U+FFFD>re semaine (%s-W%s)", year, week), level = 2)
      # doc <- addMap(doc, plotMapOverall(data = data, disease = disease, year = year, week = week), width = 100, height = 85, add_para = TRUE)
      # doc <- addBreak(doc)

      # doc <- addTitle(doc, 'D<U+FFFD>finitions')
      # doc <- addTitle(doc, 'Donn<U+FFFD>es sources', level = 2)
      # doc <- addParagraphe(doc, 'Ce rapport utilise les donn<U+FFFD>es IDS qui sont les donn<U+FFFD>es de surveillance <U+FFFD>pid<U+FFFD>miologique hebdomadaire du Minist<U+FFFD>e de la sant<U+FFFD>.', style = 'Normal')

      # doc <- addTitle(doc, 'Indicateurs', level = 2)
      # doc <- addTitle(doc, 'Incidence :', level = 3)
      # doc <- addParagraphe(doc, "C'est nombre de cas pour 100,000 personnes pour une semaine donn<U+FFFD>e", style = 'Normal')
      # doc <- addTitle(doc, 'Taux d<U+FFFD>attaque : ', level = 3)
      # doc <- addParagraphe(doc, '', style = 'Normal')
      # doc <- addParagraphe(doc, 'Les tables pr<U+FFFD>sentent les taux d<U+FFFD>attaques de l<U+FFFD>ann<U+FFFD>e en cours et l<U+FFFD>ann<U+FFFD>e pr<U+FFFD>c<U+FFFD>dente pour la m<U+FFFD>me p<U+FFFD>riode. Par exemple pour la semaine 48 en 2019, la table pr<U+FFFD>sente les taux d<U+FFFD>attaque de la semaine 1 <U+FFFD> la semaine 48 pour les ann<U+FFFD>es 2018 et 2019.', style = 'Normal')
      # doc <- addParagraphe(doc, "Dans le contexte de ce rapport c'est l'incidence cumul<U+FFFD>e depuis le d<U+FFFD>but de l<U+FFFD>ann<U+FFFD>e pour 100,000 personnes", style = 'Normal')
      # doc <- addTitle(doc, 'L<U+FFFD>talit<U+FFFD> (CFR)', level = 3)
      # doc <- addParagraphe(doc, "C'est le pourcentage de d<U+FFFD>c<U+FFFD>s parmis les cas pour une semaine donn<U+FFFD>e", style = 'Normal')

      # doc <- addTitle(doc, 'Les graphiques', level = 3)
      # doc <- addParagraphe(doc, "Les courbes <U+FFFD>pid<U+FFFD>miques pr<U+FFFD>sentent les incidences pour l'ann<U+FFFD>e en cours (en rouge) et pour l'ann<U+FFFD>e pr<U+FFFD>c<U+FFFD>dente (en yellow)", style = 'Normal')

      # if (disease == 'MSL') {
      #   doc <- addTitle(doc, 'Districts avec au moins 5 cas au cours des 4 derni<U+FFFD>res semaines', level = 2)
      #   doc <- addTable(doc, tableWeekAlerts(data = data, disease = disease, year = year, week = week))
      #   doc <- addBreak(doc)
      #   doc <- addPlot(doc, plotIncidenceWithAlerts(data = data, disease = disease, year = year, week = week), width = 100, height = 90, add_para = TRUE)
      #   doc <- addBreak(doc)
      # }

      # doc <- addTitle(doc, sprintf('Evolution du nombre de cas - Tendance par district semaine %s-W%s', year, week), level = 2)
      # doc <- addParagraphe(doc, 'Le nombre de cas :', style = 'Normal')
      # doc <- addParagraphe(doc, '  - diminue pour les districts en bleu :', style = 'Normal')
      # doc <- addParagraphe(doc, '  - augmente pour les districts en rouge :', style = 'Normal')
      # doc <- addParagraphe(doc, '  - est stable pour les districts en blanc :', style = 'Normal')
      # doc <- addParagraphe(doc, '', style = 'Normal')
      # doc <- addParagraphe(doc, "L'intensit<U+FFFD> de la couleur est proportionnelle <U+FFFD> l'intensit<U+FFFD> de l'<U+FFFD>volution.", style = 'Normal')

      # #  doc <- addMap(doc, plotMapOverallCaseDeath(data = data, disease = disease, year = year, week = week), width = 100, height = 45, add_para = TRUE)
      # doc <- addMap(doc, plotMapOverallTrend(data = data, disease = disease, year = year, week = week), width = 100, height = 85, add_para = TRUE)
      # #  doc <- addMap(doc, plotMapOverallMetric(data = data, disease = disease, year = year, week = week, facet_week = FALSE, metric = 'ind_cse_ta'), width = 45, height = 45, add_para = FALSE)
      # doc <- addBreak(doc)

      # n <- df |>
      #   dplyr::pull(adm_1) |>
      #   unique() |>
      #   length()
      # n <- ceiling(n / 4)
      # height1 <- ifelse(n == 1, n * 15 + 6, ifelse(n == 2, n * 15 + 4, n * 15 + 2))

      # doc <- addTitle(doc, 'Situation par r<U+FFFD>gion')
      # doc <- addTable(doc, tbl)
      # # doc <- addParagraphe(doc, '')
      # # doc <- addPlot(doc, plotMapWeeks( data = data, disease = disease, year = year, week = week, metric = 'ind_cse_ti', adm_level = 'adm_1'), width = 100, height = 35, add_para = TRUE)
      # doc <- addBreak(doc)
      # doc <- addPlot(doc, plotIncidence(data = data, disease = disease, year = year, week = week, adm_level = 'adm_1'), width = 100, height1, add_para = TRUE)
      # doc <- addBreak(doc)

      # doc <- addTitle(doc, 'Situation par district')

      # df <- dplyr::filter(df, adm_1 != 'TRUE')

      # adms <- sortu(df$adm_1)
      # for (i in seq_len(length(adms))) {
      #   log_debug('Report %s|Analyse District %s...|%s', disease_s, adms[i], round(20 + i / length(adms) * 80), progress = progress)

      #   n <- dplyr::filter(df, adm_1 == adms[i] & adm_2 != 'TRUE') |>
      #     dplyr::pull(adm_2) |>
      #     unique() |>
      #     length()
      #   n <- ceiling(n / 4)
      #   height1 <- ifelse(n == 1, n * 15 + 6, ifelse(n == 2, n * 15 + 4, n * 15 + 2))
      #   admTitle <- unique(dplyr::select(data$data_adm_1, adm_1, adm_1_name))
      #   admTitle <- (filter(admTitle, adm_1 == adms[i]))$adm_1_name
      #   doc <- addTitle(doc, admTitle, level = 2)
      #   doc <- addTable(doc, tableWeeks(data = data, disease = disease, year = year, week = week, adm_level = 'adm_2', adm_1 = adms[i]))
      #   doc <- addParagraphe(doc, '')
      #   doc <- addPlot(doc, plotIncidence(data = data, disease = disease, year = year, week = week, adm_level = 'adm_2', adm_1 = adms[i]), width = 100, height = height1, add_para = TRUE)
      #   doc <- addBreak(doc)
      # }

      generateReport(doc, filename = fs::file_temp(), open = open)
    },
    sampleExportToDocEach = function(
      file,
      open = TRUE,
      polygon_idx = NULL,
      samples_idxs = NULL
    ) {
      if (is.null(polygon_idx)) {
        sf_pol <- polygon_focused
        sf_pts <- samples
      } else {
        sf_pol <- polygons[polygon_idx, ]
        sf_pts <- sf_pol$samples_sf[[1L]]
      }

      if (nrow(sf_pts) == 0L) {
        return()
      }

      if (!is.null(samples_idxs)) {
        sf_pts <- sf_pts[samples_idxs, ]
      }

      sf_pts_cc <- sf::st_coordinates(sf_pts)

      for (i in seq_len(nrow(sf_pts_cc))) {
        bb <- sf::st_bbox(sf_pts[i, ], crs = sf::st_crs(4326L))
        bb <- sf::st_bbox(sf::st_transform(sf::st_as_sfc(bb), 3857L))

        # Calculate bbox around 200m
        bb_200 <- bb
        bb_200[['xmin']] <- bb_200[['xmin']] - 200L
        bb_200[['ymin']] <- bb_200[['ymin']] - 200L
        bb_200[['xmax']] <- bb_200[['xmax']] + 200L
        bb_200[['ymax']] <- bb_200[['ymax']] + 200L

        # Calculate bbox around 400m
        bb_400 <- bb
        bb_400[['xmin']] <- bb_400[['xmin']] - 400L
        bb_400[['ymin']] <- bb_400[['ymin']] - 400L
        bb_400[['xmax']] <- bb_400[['xmax']] + 400L
        bb_400[['ymax']] <- bb_400[['ymax']] + 400L

        bb_200 <- sf::st_bbox(sf::st_transform(sf::st_as_sfc(bb_200), 4326L))
        bb_400 <- sf::st_bbox(sf::st_transform(sf::st_as_sfc(bb_400), 4326L))

        file_map_200 <- sprintf('%s/pts_%s_200.png', fs::path_temp(), i)
        file_map_400 <- sprintf('%s/pts_%s_400.png', fs::path_temp(), i)
        file_map_400_road <- sprintf(
          '%s/pts_%s_400_road.png',
          fs::path_temp(),
          i
        )
        file_map_qr <- sprintf('%s/pts_%s_qr.png', fs::path_temp(), i)

        grDevices::png(
          file_map_200,
          width = 140L,
          height = 140L,
          units = 'mm',
          res = 96L
        )
        plotMap(bb_200)
        plot(sf_pts[i, ], add = TRUE, pch = 16L, col = 'white', cex = 2L)
        plot(sf_pts[i, ], add = TRUE, pch = 16L, col = 'yellow')
        text(
          x = sf_pts_cc[i, 1L],
          y = sf_pts_cc[i, 2L],
          labels = i,
          pos = 3L,
          cex = 0.75,
          col = 'white'
        )
        grDevices::dev.off()

        grDevices::png(
          file_map_400,
          width = 70L,
          height = 70L,
          units = 'mm',
          res = 96L
        )
        plotMap(bb_400)
        plot(sf_pts[i, ], add = TRUE, pch = 16L, col = 'white', cex = 2L)
        plot(sf_pts[i, ], add = TRUE, pch = 16L, col = 'yellow')
        text(
          x = sf_pts_cc[i, 1L],
          y = sf_pts_cc[i, 2L],
          labels = i,
          pos = 3L,
          cex = 0.75,
          col = 'white'
        )
        grDevices::dev.off()

        grDevices::png(
          file_map_400_road,
          width = 70L,
          height = 70L,
          units = 'mm',
          res = 96L
        )
        plotMap(bb_400, road = TRUE)
        plot(sf_pts[i, ], add = TRUE, pch = 16L, col = 'white', cex = 2L)
        plot(sf_pts[i, ], add = TRUE, pch = 16L, col = 'yellow')
        text(
          x = sf_pts_cc[i, 1L],
          y = sf_pts_cc[i, 2L],
          labels = i,
          pos = 3L,
          cex = 0.75,
          col = 'white'
        )
        grDevices::dev.off()

        qr <- qrcode::qr_code(sprintf(
          'geo:%s,%s',
          sf_pts_cc[i, 1L],
          sf_pts_cc[i, 2L]
        ))

        grDevices::png(
          file_map_qr,
          width = 70L,
          height = 70L,
          units = 'mm',
          res = 96L
        )
        plot(qr)
        grDevices::dev.off()
      }

      tbl <-
        flextable::flextable(data.frame(
          a = rep('', 3L),
          b = rep('', 3L),
          stringsAsFactors = FALSE
        )) |>
        flextable::width(width = 130L, j = 'a', unit = 'mm') |>
        flextable::width(width = 70L, j = 'b', unit = 'mm') |>
        flextable::height(height = 130L, i = 1L, unit = 'mm') |>
        flextable::delete_part(part = 'header') |>
        flextable::delete_part(part = 'footer') |>
        flextable::border_remove() |>
        flextable::valign(i = 1L, j = 1L, valign = 'top')

      if (sf_pol$type == 'SP_QDR') {
        tbl <- tbl |>
          flextable::compose(
            i = 1L,
            j = 1L,
            value = flextable::as_paragraph(
              flextable::as_chunk(
                sprintf(
                  '%s (%s)\n\n',
                  sf_pts[i, ]$id_user,
                  sf_pts[i, ]$id_key_calc
                ),
                props = flextable::fp_text_default(font.size = 18L)
              ),
              flextable::as_chunk(
                'Population:',
                props = flextable::fp_text_default(font.size = 12L)
              ),
              flextable::as_chunk('\t\t< 5 years: ..........'),
              flextable::as_chunk('\t\t>= 5 years: ..........')
            ),
            part = 'body'
          )
      } else if (sf_pol$type == 'SP_TSQ') {
      }

      tbl <- tbl |>
        flextable::compose(
          i = 2L,
          j = 1L,
          value = flextable::as_paragraph(
            flextable::as_image(
              src = file_map_200,
              width = units::set_units(units::set_units(125L, mm), inches),
              height = units::set_units(units::set_units(125L, mm), inches)
            )
          ),
          part = 'body'
        ) |>
        flextable::compose(
          i = 2L,
          j = 2L,
          value = flextable::as_paragraph(
            flextable::as_image(
              src = file_map_400,
              width = units::set_units(units::set_units(65L, mm), inches),
              height = units::set_units(units::set_units(65L, mm), inches)
            )
          ),
          part = 'body'
        ) |>
        flextable::compose(
          i = 2L,
          j = 3L,
          value = flextable::as_paragraph(
            flextable::as_image(
              src = file_map_400_road,
              width = units::set_units(units::set_units(65L, mm), inches),
              height = units::set_units(units::set_units(65L, mm), inches)
            )
          ),
          part = 'body'
        ) |>
        flextable::compose(
          i = 1L,
          j = 2L,
          value = flextable::as_paragraph(
            flextable::as_image(
              src = file_map_qr,
              width = units::set_units(units::set_units(65L, mm), inches),
              height = units::set_units(units::set_units(65L, mm), inches)
            )
          ),
          part = 'body'
        )

      doc <- createDocument(template = 'default.portrait')
      doc |>
        flextable::body_add_flextable(value = tbl, align = 'left') |>
        generateReport(filename = 'doc.docx', open = TRUE)
    },
    # TODO: #7 Gestion du mode de tirage aleatoire
    sampleQuadratEdit = function(map, id, ...) {
      for (i in seq_len(nrow(self$polygons))) {
        m <- match(id, self$polygons$samples_quadrat_sf[[i]]$id)

        if (!is.na(m)) {
          sample_quadrat <- self$polygons$samples_quadrat_sf[[i]][m, ]

          map |>
            leafpm::editFeature(
              targetGroup = sprintf('QDR_%s', i),
              targetId = id,
              editOptions = leafpm::pmEditOptions(
                snapDistance = 10L
              )
            ) |>
            flyToSpatialFeature(sample_quadrat, zoom = 16L)

          break
        }
      }

      map
    },
    sampleQuadratPost = function(map, feature) {
      quadrat <-
        st_as_sf.feature(feature)

      for (i in seq_len(nrow(self$polygons))) {
        if (self$polygons$selected[i]) {
          m <- match(quadrat$layerId, self$polygons$samples_quadrat_sf[[i]]$id)

          if (!is.na(m)) {
            private$.polygons$samples_quadrat_sf[[i]][
              m,
            ] <- private$.polygons$samples_quadrat_sf[[i]][m, ] |>
              sf::st_set_geometry(sf::st_geometry(quadrat))

            private$.polygons$samples_sf[[i]][m, ]$type <- 'POL'
            private$.polygons$samples_sf[[
              i
            ]]$area <- as.integer(sf::st_area(self$polygons$samples_quadrat_sf[[
              i
            ]]))
            private$.polygons$samples_quadrat_sf[[i]][m, ]$type <- 'POL'
            private$.polygons$samples_quadrat_sf[[
              i
            ]]$area <- as.integer(sf::st_area(self$polygons$samples_quadrat_sf[[
              i
            ]]))

            step_sample$state <- utils::modifyList(
              step_sample$state,
              list(modified = TRUE)
            )

            break
          }
        }
      }
    },
    sampleLoadTable = function(tbl_id) {
      if (is.null(self$polygons)) {
        return()
      }

      polygon <- dplyr::filter(self$polygons, focused_result)

      samples_empty <- FALSE

      if (nrow(polygon) == 0L) {
        samples_empty <- TRUE
      } else {
        samples <- polygon$samples_sf[[1L]]
        if (nrow(samples) >= 1L) {
          samples <- samples |>
            dplyr::select(
              id_n,
              id_user,
              id_key,
              id_key_calc,
              status,
              d1,
              d2,
              pop_u5_1,
              pop_a5_1,
              pop_u5_2,
              pop_a5_2,
              pop_u5,
              pop_a5,
              comment
            ) |>
            dplyr::mutate(
              id_void = NA,
              .after = id_key_calc
            ) |>
            dplyr::mutate(dplyr::across(where(is.numeric), as.character))

          samples[is.na(samples)] <- ''
        } else {
          samples_empty <- TRUE
        }
      }

      if (samples_empty) {
        samples <-
          tibble::tibble(
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
          )
      }

      rhandsontable::load_data(
        id = tbl_id,
        data = samples,
        session = application$session
      )
      rhandsontable::render(id = tbl_id, session = application$session)
    },
    sampleToggleSelect = function(map, sample) {
      displaySample(
        map,
        polygon_idx = match(sample$polygon, self$polygons$id_n),
        samples_idx = sample$id
      )

      step_result$state <- utils::modifyList(
        step_result$state,
        list(modified = TRUE)
      )
    },
    samplesSetValueFromTable = function(map, recs, vars, values) {
      if (length(recs) == 0L) {
        return()
      }

      values <- unlist(values)
      values[is.null(values)] <- NA

      polygon_idx <- match(TRUE, self$polygons$focused_result)

      for (var in unique(vars)) {
        private$.polygons[polygon_idx, ]$samples_sf[[1L]][
          recs[vars == var],
          var
        ] <- values[vars == var]
      }

      private$.polygons[polygon_idx, ]$samples_sf[[1L]] <- private$.polygons[
        polygon_idx,
      ]$samples_sf[[1L]] |>
        dplyr::mutate(
          color = dplyr::case_when(
            status == df_selectors['C1', 'label'] ~ df_selectors['C1', 'color'],
            status == df_selectors['C2', 'label'] ~ df_selectors['C2', 'color'],
            status == df_selectors['C3', 'label'] ~ df_selectors['C3', 'color'],
            status == df_selectors['C4', 'label'] ~ df_selectors['C4', 'color'],
            status == df_selectors['C5', 'label'] ~ df_selectors['C5', 'color'],
            status == df_selectors['C6', 'label'] ~ df_selectors['C6', 'color'],
            status == df_selectors['C7', 'label'] ~ df_selectors['C7', 'color'],
            status == df_selectors['C8', 'label'] ~ df_selectors['C8', 'color'],
            status == df_selectors['TRUE', 'label'] ~
              df_selectors['TRUE', 'color'],
            TRUE ~ 'yellow'
          )
        )

      displaySample(map, polygon_idx = polygon_idx, samples_idx = recs)

      step_result$state <- utils::modifyList(
        step_result$state,
        list(modified = TRUE)
      )
    },
    samplesSetValueFromMap = function(table_id, id, var, value) {
      if (is.null(value)) {
        value <- NA
      }

      polygon_idx <- match(TRUE, self$polygons$focused_result)

      private$.polygons[polygon_idx, ]$samples_sf[[1L]][
        as.integer(id),
        var
      ] <- value
      private$.polygons[polygon_idx, ]$samples_sf[[1L]] <-
        dplyr::mutate(
          private$.polygons[polygon_idx, ]$samples_sf[[1L]],
          color = dplyr::case_when(
            status == 'Excluded' ~ 'gray',
            status == 'To Be Done' ~ 'orange',
            status == 'In Progress' ~ 'cyan',
            status == 'Done' ~ 'red',
            TRUE ~ 'yellow'
          )
        )

      sampleDisplayToTable(
        table_id,
        polygon_idx = polygon_idx,
        samples_idx = as.integer(id)
      )

      step_result$state <- utils::modifyList(
        step_result$state,
        list(modified = TRUE)
      )
    },
    addCell = function(
      map,
      token,
      cell = NULL,
      inverse = FALSE,
      toggle = TRUE
    ) {
      if (is.defined(private$.tile)) {
        cells <- db$dbGetQuery(
          'SELECT cells FROM tiles WHERE x = ? AND y = ?',
          params = list(private$.tile$x, private$.tile$y)
        )
        cells_bool <- as.logical(as.integer(stringr::str_split_fixed(
          string = cells,
          pattern = '',
          n = Inf
        )))
        if (cell == -1L) {
          cells_bool <- !cells_bool
        } else {
          if (cell == 0L) {
            if (all(cells_bool)) {
              cells_bool <- rep(FALSE, 9L)
            } else {
              cells_bool <- rep(TRUE, 9L)
            }
          } else {
            if (toggle) {
              cells_bool[cell] <- !cells_bool[cell]
            } else {
              cells_bool[cell] <- TRUE
            }
          }
        }
        cells <- paste(as.integer(cells_bool), collapse = '')

        self$db$dbExecute(
          sql = 'UPDATE tiles SET cells = ? WHERE x = ? AND y = ?',
          params = list(cells, private$.tile$x, private$.tile$y)
        )

        map |>
          setStyleFast(
            group = 'identify_grid',
            fill_opacities = as.list(ifelse(cells_bool, 0.2, 0.01))
          )
      }
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
      if (is.defined(private$.tile)) {
        if (valid) {
          if (private$.tile$status != 2L) {
            private$.tile$status <- 2L

            tile_key <- paste(private$.tile$x, private$.tile$y, sep = '_')
            self$db$dbExecute(
              sql = 'UPDATE tiles SET status = 2 WHERE (x || "_" || y) = ? AND polygon = ?',
              params = list(tile_key, private$.tile$polygon)
            )

            direction <- ''

            invalidate(self$roofs_changed)

            self$grid_status_invalidated <- TRUE

            step_identify$state <- utils::modifyList(
              step_identify$state,
              list(modified = TRUE)
            )
          }
        } else if (invalid) {
          if (private$.tile$status != 0L) {
            private$.tile$status <- 0L

            tile_key <- paste(private$.tile$x, private$.tile$y, sep = '_')
            self$db$dbExecute(
              sql = 'UPDATE tiles SET status = 0 WHERE (x || "_" || y) = ? AND polygon = ?',
              params = list(tile_key, private$.tile$polygon)
            )

            direction <- ''

            invalidate(self$roofs_changed)

            self$grid_status_invalidated <- TRUE

            step_identify$state <- utils::modifyList(
              step_identify$state,
              list(modified = TRUE)
            )
          }
        } else if (mapped) {
          if (private$.tile$status != 1L) {
            private$.tile$status <- 1L

            tile_key <- paste(private$.tile$x, private$.tile$y, sep = '_')
            self$db$dbExecute(
              sql = 'UPDATE tiles SET status = 1 WHERE (x || "_" || y) = ? AND polygon = ?',
              params = list(tile_key, private$.tile$polygon)
            )

            direction <- ''

            invalidate(self$roofs_changed)

            self$grid_status_invalidated <- TRUE

            step_identify$state <- utils::modifyList(
              step_identify$state,
              list(modified = TRUE)
            )
          }
        }
      }

      # polygons_idx <- sf::st_intersection(polygons, sf::st_sfc(sf::st_point(c(lon, lat)), crs = 4326))$lf_key

      grid_tile_xn <- map_size$x %/% 256L
      grid_tile_yn <- map_size$y %/% 256L

      grid_tile_xn <- 1L
      grid_tile_yn <- 1L

      if (direction == 'next') {
        if (is.null(private$.tile)) {
          current_tile_x <- 0L
          current_tile_y <- 0L
          current_tile_polygon <- 1L
        } else {
          current_tile_x <- private$.tile$x
          current_tile_y <- private$.tile$y
          current_tile_polygon <- private$.tile$polygon
        }

        if (valid) {
          status_target <- 1L
        } else {
          status_target <- 0L
        }

        grid_tile_0 <-
          db$dbGetQuery(
            paste(
              'SELECT * FROM tiles',
              'WHERE locked IS NULL AND',
              '(((status = ?) AND (polygon = ?) AND (y = ?) AND (x > ?)) OR',
              ' ((status = ?) AND (polygon = ?) AND (y > ?)) OR',
              ' ((status = ?) AND (polygon = ?) AND (y < ?)) OR',
              '  (status = ?))',
              'ORDER BY ROWID ASC LIMIT 1'
            ),
            params = list(
              status_target,
              current_tile_polygon,
              current_tile_y,
              current_tile_x,
              status_target,
              current_tile_polygon,
              current_tile_y,
              status_target,
              current_tile_polygon,
              current_tile_y,
              status_target
            )
          )
      } else if (direction == 'coordinates') {
        tilenum <- slippymath::lonlat_to_tilenum(
          lon,
          lat,
          settings$getValue('sli_identify_zoom')
        )
        tilenum_x <- tilenum$x
        tilenum_y <- tilenum$y

        grid_tile_0 <- self$db$dbGetQuery(
          'SELECT * FROM tiles WHERE locked IS NULL AND x = ? AND y = ? AND status != -1 LIMIT 1',
          params = list(tilenum_x, tilenum_y)
        )
      } else if (direction != '') {
        if (is.null(private$.tile)) {
          return()
        }

        grid_tile_0 <- private$.tile[1L, ]

        if (direction == 'left') {
          grid_tile_0$x <- grid_tile_0$x - grid_tile_xn
        } else if (direction == 'right') {
          grid_tile_0$x <- grid_tile_0$x + grid_tile_xn
        } else if (direction == 'top') {
          grid_tile_0$y <- grid_tile_0$y - grid_tile_yn
        } else if (direction == 'bottom') {
          grid_tile_0$y <- grid_tile_0$y + grid_tile_yn
        }

        tilenum_x <- grid_tile_0$x
        tilenum_y <- grid_tile_0$y

        grid_tile_0 <- self$db$dbGetQuery(
          'SELECT * FROM tiles WHERE locked IS NULL AND x = ? AND y = ? AND status != -1 LIMIT 1',
          params = list(tilenum_x, tilenum_y)
        )
      } else {
        grid_tile_0 <- private$.tile[1L, ]
      }

      if (nrow(grid_tile_0) == 0L) {
        return()
      }

      polygon <- grid_tile_0$polygon

      grid_tile_0_bbox <- tile_bbox_ll(
        grid_tile_0$x,
        grid_tile_0$y,
        settings$getValue('sli_identify_zoom')
      )

      grid_tile_1_bbox <- tile_bbox_ll(
        grid_tile_0$x + grid_tile_xn - 1L,
        grid_tile_0$y + grid_tile_yn - 1L,
        settings$getValue('sli_identify_zoom')
      )

      grid_bbox <- grid_tile_0_bbox
      grid_bbox[2L] <- grid_tile_1_bbox[2L] + 0.0001
      grid_bbox[3L] <- grid_tile_1_bbox[3L] - 0.0001

      tile_grid <- slippymath::bbox_to_tile_grid(
        bbox = grid_bbox[c(1L, 3L, 2L, 4L)],
        zoom = settings$getValue('sli_identify_zoom')
      )

      tilenum_x <- tile_grid$tiles$x
      tilenum_y <- tile_grid$tiles$y

      tile_grid$tiles <- self$tiles |>
        dplyr::filter(
          x == tilenum_x,
          y == tilenum_y,
          status != -1L
        ) |>
        dplyr::collect() |>
        dplyr::slice(1L)

      tile_grid_sf <- tile_grid_to_sf(tile_grid)
      tile_grid_sf <- tile_grid_sf |>
        dplyr::mutate(
          id_n = seq_len(dplyr::n()),
          id = st_id(id_n, 'tile')
          # label_short = id_n,
          # label = shiny::HTML(sprintf('<b>polygon %s</b><br>%s (ha)', label_short, area))
        )

      self$db$dbExecute(
        sql = 'UPDATE tiles SET locked = NULL WHERE locked = ?',
        params = list(token)
      )
      self$db$dbExecute(
        sql = 'UPDATE tiles SET locked = ? WHERE x = ? AND y = ?',
        params = list(token, tile_grid_sf$x, tile_grid_sf$y)
      )

      invalidateGrid()

      if (is.null(tile_grid_sf)) {
        return()
      }

      col <- switch(
        tile_grid_sf$status + 3L,
        '#0000ff',
        '#ffffff00',
        'white',
        'yellow',
        '#00ff00'
      )

      tiles_x <- tile_grid_sf$x[1L] + c(-1L, -1L, -1L, 0L, 0L, 0L, 1L, 1L, 1L)
      tiles_y <- tile_grid_sf$y[1L] + c(-1L, 0L, 1L, -1L, 0L, 1L, -1L, 0L, 1L)
      tiles <- paste(tiles_x, tiles_y, sep = '_')

      if (data$project_method == 'RS_SMP') {
        # roof method. just tile
        grid_sf <- tile_grid_sf
        grid_sf$id <- 'identify_grid'
        grid_sf$cell <- FALSE
      } else {
        # Urban method. Divide tile by 9 cells
        grid_bbox <- sf::st_bbox(tile_grid_sf)

        xmin0 <- as.double(grid_bbox$xmin)
        ymin0 <- as.double(grid_bbox$ymin)
        w <- as.double((grid_bbox$xmax - grid_bbox$xmin) / 3L)
        h <- as.double((grid_bbox$ymax - grid_bbox$ymin) / 3L)

        grid_sf <- sf::st_sfc()

        for (c in 0:2) {
          for (r in 0:2) {
            cell_sf <-
              sf::st_sf(
                r = r,
                c = c,
                geometry = sf::st_sfc(
                  st_bbox_polygon(
                    bbox = sf::st_bbox(
                      c(
                        xmin = xmin0 + w * r,
                        xmax = xmin0 + w * r + w,
                        ymax = ymin0 + h * c + h,
                        ymin = ymin0 + h * c
                      ),
                      crs = sf::st_crs(4326L)
                    )
                  )
                )
              )
            grid_sf <- rbind(grid_sf, cell_sf)
          }
        }

        grid_sf$id <- sprintf('identify_cell_%s', 1:9)

        cells <- db$dbGetQuery(
          'SELECT cells FROM tiles WHERE (x || "_" || y) = ?',
          params = list(tiles[5L])
        )
        cells <- as.logical(as.integer(stringr::str_split_fixed(
          string = cells,
          pattern = '',
          n = Inf
        )))

        grid_sf$cell <- cells
      }

      grid_bbox_sf <-
        sf::st_sf(
          id = '0',
          geometry = sf::st_sfc(st_bbox_polygon(bbox = sf::st_bbox(grid_sf)))
        ) |>
        sf::st_buffer(0.00001, endCapStyle = 'FLAT')

      # bbox around to fit screen
      grid_bbox_sf_2 <-
        sf::st_sf(
          id = '0',
          geometry = sf::st_sfc(st_bbox_polygon(bbox = sf::st_bbox(grid_sf)))
        ) |>
        sf::st_buffer(0.00005, endCapStyle = 'FLAT')

      map |>
        leaflet::clearGroup('identify_grid') |>
        leaflet::clearGroup('identify_grid_border') |>
        leaflet::clearGroup('identify_building') |>
        fitToSpatialFeatureBounds(grid_bbox_sf_2) |>
        leaflet::addPolygons(
          data = grid_sf,
          color = col,
          fill = TRUE,
          fillOpacity = ~ ifelse(cell, 0.20, 0.01),
          fillColor = col,
          layerId = ~id,
          smoothFactor = 0.5,
          stroke = TRUE,
          weight = 1L,
          opacity = 1.0,
          group = 'identify_grid'
        ) |>
        leaflet::addPolygons(
          data = grid_bbox_sf,
          color = col,
          fill = FALSE,
          layerId = 'identify_grid_border',
          stroke = TRUE,
          weight = 8L,
          opacity = 0.2,
          group = 'identify_grid_border'
        )

      if (data$project_method == 'RS_SMP') {
        placeholders <- paste(rep('?', length(tiles)), collapse = ', ')
        roofs_sf <- db$dbGetQuery(
          paste0(
            'SELECT * FROM roofs WHERE (x || "_" || y) IN (',
            placeholders,
            ')'
          ),
          params = as.list(tiles)
        )

        if (nrow(roofs_sf)) {
          roofs_sf <- sf::st_as_sf(roofs_sf, wkt = 'geometry')

          map |>
            displayRoofs(
              roofs = roofs_sf,
              tile_x = tiles_x[5L],
              tile_y = tiles_y[5L]
            )
        }
      }

      private$.tile <- tile_grid_sf
    },
    validAll = function(map) {
      self$db$dbExecute(
        sql = 'UPDATE tiles SET status = 2, cells = "111111111" WHERE status <> -1 '
      )
      step_identify$state <- utils::modifyList(
        step_identify$state,
        list(modified = TRUE)
      )
      step_identify$invalidatePolygons()
      step_identify$invalidateGridIdentifyStatus(force = TRUE)
    },
    invalidateGrid = function() {
      private$.tiles_trigger$trigger()
    }
  ),
  private = list(
    .bbox_default = NULL,
    .db = NULL,
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
    .sample = NULL,
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
