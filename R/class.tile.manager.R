#' @title TileManager
#' @description R6 class for tile/roof identification and grid management.
#'
#' Extracts tile and roof responsibilities from UserData.
#' Receives a reference to the UserData instance (`data`) for access
#' to shared state (db, polygons, settings, step references).
#'
#' @noRd
#'
TileManager <- R6::R6Class(
  classname = 'TileManager',
  portable = FALSE,
  public = list(
    #' @field data Reference to the parent UserData instance.
    data = NULL,

    #' @description Create a new TileManager.
    #' @param data The UserData instance owning shared state.
    initialize = function(data) {
      self$data <- data
    },

    #' @description Add a single roof point to the database.
    #' @param map Leaflet map proxy.
    #' @param lng Longitude of the roof.
    #' @param lat Latitude of the roof.
    #' @param ... Additional arguments.
    #' @return The roof sf object.
    addRoof = function(map, lng, lat, ...) {
      tilenum <- slippymath::lonlat_to_tilenum(lng, lat, zoom = ZOOM_TILE)

      roof <-
        sf::st_sf(
          id = uuid::UUIDgenerate(),
          x = tilenum$x,
          y = tilenum$y,
          geometry = sf::st_sfc(sf::st_point(c(lng, lat)))
        ) |>
        sf::st_set_crs(4326L)

      intersect <- sf::st_intersects(roof, data$polygons, sparse = FALSE)

      if (any(intersect)) {
        roof$polygon <- match(TRUE, intersect)

        safe_db(
          data$db$dbExecute(
            sql = 'INSERT INTO roofs VALUES(?, ?, ?, ?, ?)',
            params = list(
              roof$id,
              roof$polygon,
              roof$x,
              roof$y,
              sf::st_as_text(roof$geometry)
            )
          ),
          msg = "addRoof"
        )

        self$displayRoofs(map, roofs = roof)

        invalidate(data$roofs_changed)
      }

      roof
    },

    #' @description Remove a roof from the database and map.
    #' @param map Leaflet map proxy.
    #' @param roof_id The roof ID to remove.
    #' @param ... Additional arguments.
    removeRoof = function(map, roof_id, ...) {
      roof_id <- gsub('_bck', '', roof_id, fixed = TRUE)

      safe_db(
        data$db$dbExecute(
          sql = 'DELETE FROM roofs WHERE id = ?',
          params = list(roof_id)
        ),
        msg = "removeRoof"
      )

      map |>
        leaflet::removeMarker(layerId = roof_id) |>
        leaflet::removeShape(layerId = sprintf('%s_bck', roof_id))

      invalidate(data$roofs_changed)
    },

    #' @description Add multiple roofs from an sf object.
    #' @param map Leaflet map proxy.
    #' @param roofs An sf object of roof points.
    #' @param ... Additional arguments.
    addRoofs = function(map, roofs, ...) {
      roofs <- dplyr::bind_rows(roofs, data$roofs)

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

      intersect <- sf::st_intersects(roofs, data$polygons, sparse = FALSE)
      intersect <- apply(intersect, 1L, any)

      roofs <- roofs[intersect, ]

      if (nrow(roofs) > 0L) {
        roofs$polygon <- as.integer(sf::st_within(roofs, data$polygons))

        tiles_roofs <- pointsToTiles(data$polygons, roofs)

        tiles <- safe_db(
          data$db$dbGetQuery('SELECT * from tiles'),
          default = data.frame(),
          msg = "addRoofs:SELECT tiles"
        )
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

        safe_db(
          data$db$dbWriteTable(name = 'tiles', value = tiles, overwrite = TRUE),
          msg = "addRoofs:dbWriteTable(tiles)"
        )

        roofs_xy <- sf::st_coordinates(roofs)

        tilenum <- slippymath::lonlat_to_tilenum(
          roofs_xy[, 1L],
          roofs_xy[, 2L],
          zoom = ZOOM_TILE
        )

        roofs <-
          tibble::tibble(
            id = uuid::UUIDgenerate(n = nrow(roofs)),
            polygon = roofs$polygon,
            x = as.integer(tilenum$x),
            y = as.integer(tilenum$y),
            geometry = sf::st_as_text(roofs$geometry)
          )

        safe_db(
          data$db$dbWriteTable(name = 'roofs', value = roofs, overwrite = TRUE),
          msg = "addRoofs:dbWriteTable(roofs)"
        )

        data$step_identify$state <- utils::modifyList(
          data$step_identify$state,
          list(modified = TRUE)
        )
        data$step_identify$invalidatePolygons()
        data$step_identify$invalidateGridIdentifyStatus(force = TRUE)
      }
    },

    #' @description Display roofs on the map with styling.
    #' @param map Leaflet map proxy.
    #' @param roofs Roof sf object.
    #' @param tile_x Tile X coordinate (default from roofs).
    #' @param tile_y Tile Y coordinate (default from roofs).
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
          color = data$settings$getValue('csi_circle_color'),
          fillColor = data$settings$getValue('csi_circle_color'),
          fillOpacity = 0.01 +
            data$settings$getValue('sli_circle_opacity') / 100L,
          options = leaflet::pathOptions(pane = 'marker'),
          radius = data$settings$getValue('sli_circle_radius'),
          stroke = TRUE,
          weight = 3L
        )

      map
    },

    #' @description Query tile status counts from the database.
    #' @param polygon Optional polygon index to filter.
    #' @return Data frame with status counts.
    getTileStatus = function(polygon = NULL) {
      if (is.null(polygon)) {
        status <- safe_db(
          data$db$dbGetQuery(
            '
          SELECT status, COUNT(status) AS n FROM tiles WHERE status >=0 GROUP BY status UNION
          SELECT      3, COUNT()       AS n FROM tiles WHERE locked IS NOT NULL UNION
          SELECT      4, COUNT()       AS n FROM roofs'
          ),
          default = data.frame(status = integer(0), n = integer(0)),
          msg = "getTileStatus:SELECT status counts"
        )
      } else {
        status <- safe_db(
          data$db$dbGetQuery(
            '
          SELECT status, COUNT(status) AS n FROM tiles WHERE polygon = ? AND status >=0 GROUP BY status UNION
          SELECT      3, COUNT()       AS n FROM tiles WHERE polygon = ? AND locked IS NOT NULL UNION
          SELECT      4, COUNT()       AS n FROM roofs WHERE polygon = ?',
            params = list(polygon, polygon, polygon)
          ),
          default = data.frame(status = integer(0), n = integer(0)),
          msg = "getTileStatus:SELECT status counts by polygon"
        )
      }
      status <- status |>
        dplyr::right_join(data.frame(status = 0:4), by = 'status') |>
        dplyr::mutate(n = dplyr::coalesce(n, 0L)) |>
        dplyr::arrange(status)
      status
    },

    #' @description Display the identify grid status overlay on the map.
    #' @param map Leaflet map proxy.
    #' @param token Session token for lock checking.
    #' @param force Force refresh even if not invalidated.
    #' @param ... Additional arguments.
    displayGridIdentifyStatus = function(map, token, force = FALSE, ...) {
      if (data$grid_status_invalidated || force) {
        data$grid_status_invalidated <- FALSE

        map |>
          leaflet::clearGroup(group = 'grid_status')

        for (i in seq_len(nrow(data$polygons))) {
          if (data$polygons$selected[i]) {
            tx <- data$tiles |>
              dplyr::filter(polygon == i) |>
              dplyr::select(x) |>
              dplyr::distinct() |>
              dplyr::summarize(min = min(x), max = max(x), n = dplyr::n()) |>
              dplyr::collect()
            ty <- data$tiles |>
              dplyr::filter(polygon == i) |>
              dplyr::select(y) |>
              dplyr::distinct() |>
              dplyr::summarize(min = min(y), max = max(y), n = dplyr::n()) |>
              dplyr::collect()

            tilebbox1 <- tile_bbox_ll(tx$min, ty$min, zoom = ZOOM_TILE)
            tilebbox2 <- tile_bbox_ll(tx$max, ty$max, zoom = ZOOM_TILE)

            tile_sf <-
              sf::st_union(
                st_bbox_polygon(tilebbox1),
                st_bbox_polygon(tilebbox2)
              ) |>
              sf::st_sfc() |>
              sf::st_set_crs(4326L)

            tile_bbox <- sf::st_bbox(tile_sf)

            sql_df <- safe_db(
              data$db$dbGetQuery(
                'SELECT IIF(locked IS NOT NULL AND locked <> ?, -2, status) AS status FROM tiles WHERE polygon = ?',
                params = list(token, i)
              ),
              default = data.frame(status = integer(0)),
              msg = "displayGridIdentifyStatus:SELECT tile status"
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
            )
          }
        }
      }
    },

    #' @description Toggle cell state in the tile grid.
    #' @param map Leaflet map proxy.
    #' @param token Session token.
    #' @param cell Cell index (1-9), 0 for all, -1 for invert.
    #' @param inverse Whether to inverse.
    #' @param toggle Whether to toggle (vs set).
    addCell = function(
      map,
      token,
      cell = NULL,
      inverse = FALSE,
      toggle = TRUE
    ) {
      if (is.defined(private$.tile)) {
        cells <- safe_db(
          data$db$dbGetQuery(
            'SELECT cells FROM tiles WHERE x = ? AND y = ?',
            params = list(private$.tile$x, private$.tile$y)
          ),
          default = data.frame(cells = character(0)),
          msg = "addCell:SELECT cells"
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

        safe_db(
          data$db$dbExecute(
            sql = 'UPDATE tiles SET cells = ? WHERE x = ? AND y = ?',
            params = list(cells, private$.tile$x, private$.tile$y)
          ),
          msg = "addCell"
        )

        map |>
          setStyleFast(
            group = 'identify_grid',
            fill_opacities = as.list(ifelse(cells_bool, 0.2, 0.01))
          )
      }
    },

    #' @description Navigate tiles for the identify step.
    #' @param map Leaflet map proxy.
    #' @param direction Navigation direction.
    #' @param map_size Map dimensions.
    #' @param token Session token.
    #' @param mapped Mark as mapped.
    #' @param valid Mark as valid.
    #' @param invalid Mark as invalid.
    #' @param lat Latitude for coordinate navigation.
    #' @param lon Longitude for coordinate navigation.
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
        private$update_tile_status(map, valid, invalid, mapped, direction)
      }

      grid_tile_xn <- 1L
      grid_tile_yn <- 1L

      grid_tile_0 <- private$find_next_tile(
        direction,
        valid,
        lat,
        lon,
        grid_tile_xn,
        grid_tile_yn
      )

      if (is.null(grid_tile_0) || nrow(grid_tile_0) == 0L) {
        return()
      }

      polygon <- grid_tile_0$polygon

      grid_tile_0_bbox <- tile_bbox_ll(
        grid_tile_0$x,
        grid_tile_0$y,
        data$settings$getValue('sli_identify_zoom')
      )

      grid_tile_1_bbox <- tile_bbox_ll(
        grid_tile_0$x + grid_tile_xn - 1L,
        grid_tile_0$y + grid_tile_yn - 1L,
        data$settings$getValue('sli_identify_zoom')
      )

      grid_bbox <- grid_tile_0_bbox
      grid_bbox[2L] <- grid_tile_1_bbox[2L] + 0.0001
      grid_bbox[3L] <- grid_tile_1_bbox[3L] - 0.0001

      tile_grid <- slippymath::bbox_to_tile_grid(
        bbox = grid_bbox[c(1L, 3L, 2L, 4L)],
        zoom = data$settings$getValue('sli_identify_zoom')
      )

      tilenum_x <- tile_grid$tiles$x
      tilenum_y <- tile_grid$tiles$y

      tile_grid$tiles <- data$tiles |>
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
        )

      safe_db(
        data$db$dbExecute(
          sql = 'UPDATE tiles SET locked = NULL WHERE locked = ?',
          params = list(token)
        ),
        msg = "searchSwipeTile:unlock tiles"
      )
      safe_db(
        data$db$dbExecute(
          sql = 'UPDATE tiles SET locked = ? WHERE x = ? AND y = ?',
          params = list(token, tile_grid_sf$x, tile_grid_sf$y)
        ),
        msg = "searchSwipeTile:lock tile"
      )

      self$invalidateGrid()

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

      grid_result <- private$build_grid(tile_grid_sf, tiles)
      grid_sf <- grid_result$grid_sf

      grid_bbox_sf <-
        sf::st_sf(
          id = '0',
          geometry = sf::st_sfc(st_bbox_polygon(bbox = sf::st_bbox(grid_sf)))
        ) |>
        sf::st_buffer(0.00001, endCapStyle = 'FLAT')

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
        roofs_sf <- safe_db(
          data$db$dbGetQuery(
            paste0(
              'SELECT * FROM roofs WHERE (x || "_" || y) IN (',
              placeholders,
              ')'
            ),
            params = as.list(tiles)
          ),
          default = data.frame(),
          msg = "searchSwipeTile:SELECT roofs"
        )

        if (nrow(roofs_sf)) {
          roofs_sf <- sf::st_as_sf(roofs_sf, wkt = 'geometry')

          map |>
            self$displayRoofs(
              roofs = roofs_sf,
              tile_x = tiles_x[5L],
              tile_y = tiles_y[5L]
            )
        }
      }

      private$.tile <- tile_grid_sf
    },

    #' @description Mark all tiles as valid.
    #' @param map Leaflet map proxy.
    validAll = function(map) {
      safe_db(
        data$db$dbExecute(
          sql = 'UPDATE tiles SET status = 2, cells = "111111111" WHERE status <> -1 '
        ),
        msg = "validAll:UPDATE tiles status"
      )
      data$step_identify$state <- utils::modifyList(
        data$step_identify$state,
        list(modified = TRUE)
      )
      data$step_identify$invalidatePolygons()
      data$step_identify$invalidateGridIdentifyStatus(force = TRUE)
    },

    #' @description Add roof points from OpenStreetMap building data.
    #' Delegates to the private implementation.
    #' @param map Leaflet map proxy.
    #' @param progress A progress callback function(value).
    #' @param ... Additional arguments.
    #' @return An sf object of roof centroids.
    roofsAddOSM = function(map, progress, ...) {
      private$roofsAddOSM_impl(map, progress, ...)
    },

    #' @description Trigger tile grid invalidation.
    invalidateGrid = function() {
      data$invalidateGrid()
    }
  ),
  private = list(
    .tile = NULL,

    update_tile_status = function(map, valid, invalid, mapped, direction) {
      new_status <- NULL
      if (valid && private$.tile$status != 2L) {
        new_status <- 2L
      } else if (invalid && private$.tile$status != 0L) {
        new_status <- 0L
      } else if (mapped && private$.tile$status != 1L) {
        new_status <- 1L
      }

      if (!is.null(new_status)) {
        private$.tile$status <- new_status
        tile_key <- paste(private$.tile$x, private$.tile$y, sep = '_')
        safe_db(
          data$db$dbExecute(
            sql = sprintf(
              'UPDATE tiles SET status = %d WHERE (x || "_" || y) = ? AND polygon = ?',
              new_status
            ),
            params = list(tile_key, private$.tile$polygon)
          ),
          msg = "update_tile_status:UPDATE tile status"
        )
        invalidate(data$roofs_changed)
        data$grid_status_invalidated <- TRUE
        data$step_identify$state <- utils::modifyList(
          data$step_identify$state,
          list(modified = TRUE)
        )
      }
    },

    find_next_tile = function(
      direction,
      valid,
      lat,
      lon,
      grid_tile_xn,
      grid_tile_yn
    ) {
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

        status_target <- if (valid) 1L else 0L

        safe_db(
          data$db$dbGetQuery(
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
          ),
          default = data.frame(),
          msg = "find_next_tile:SELECT next tile"
        )
      } else if (direction == 'coordinates') {
        tilenum <- slippymath::lonlat_to_tilenum(
          lon,
          lat,
          data$settings$getValue('sli_identify_zoom')
        )
        safe_db(
          data$db$dbGetQuery(
            'SELECT * FROM tiles WHERE locked IS NULL AND x = ? AND y = ? AND status != -1 LIMIT 1',
            params = list(tilenum$x, tilenum$y)
          ),
          default = data.frame(),
          msg = "find_next_tile:SELECT tile by coordinates"
        )
      } else if (direction != '') {
        if (is.null(private$.tile)) {
          return(NULL)
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
        safe_db(
          data$db$dbGetQuery(
            'SELECT * FROM tiles WHERE locked IS NULL AND x = ? AND y = ? AND status != -1 LIMIT 1',
            params = list(grid_tile_0$x, grid_tile_0$y)
          ),
          default = data.frame(),
          msg = "find_next_tile:SELECT tile by direction"
        )
      } else {
        private$.tile[1L, ]
      }
    },

    build_grid = function(tile_grid_sf, tiles) {
      if (data$project_method == 'RS_SMP') {
        grid_sf <- tile_grid_sf
        grid_sf$id <- 'identify_grid'
        grid_sf$cell <- FALSE
      } else {
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

        cells <- safe_db(
          data$db$dbGetQuery(
            'SELECT cells FROM tiles WHERE (x || "_" || y) = ?',
            params = list(tiles[5L])
          ),
          default = data.frame(cells = "000000000"),
          msg = "build_grid:SELECT cells"
        )
        cells <- as.logical(as.integer(stringr::str_split_fixed(
          string = cells,
          pattern = '',
          n = Inf
        )))

        grid_sf$cell <- cells
      }
      list(grid_sf = grid_sf)
    },

    roofsAddOSM_impl = function(map, progress, ...) {
      roofs_sf <- NULL

      map |>
        leaflet::clearGroup('tiles')

      polygons <- self$data$polygons

      for (i in seq_len(nrow(polygons))) {
        polygon_sf <- polygons[i, ]

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

        tiles <- tibble::as_tibble(tile_grid$tiles)
        tiles$building <- NA

        sf_polygon_roofs <- NULL

        for (t in seq_len(OSM_MAX_TILES)) {
          progress(
            value = 100L *
              ((i - 1L) /
                nrow(polygons) +
                t / nrow(tiles) / nrow(polygons))
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

            if (is.null(sf_polygon_roofs)) {
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

        if (i == 1L) {
          roofs_sf <- sf_polygon_roofs
        } else {
          roofs_sf <- rbind(roofs_sf, sf_polygon_roofs)
        }
      }

      for (i in seq_len(nrow(polygons))) {
        progress(value = 100L * (i - 1L) / nrow(polygons))

        polygon_sf <- polygons[i, ]

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
        fitToSpatialFeatureBounds(polygons)

      roofs_sf <- roofs_sf |>
        sf::st_set_crs('EPSG:3857') |>
        sf::st_set_agr('constant') |>
        sf::st_centroid() |>
        sf::st_transform(4326L)

      roofs_sf
    }
  )
)
