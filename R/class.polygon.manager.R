#' @title PolygonManager
#' @description R6 class for polygon CRUD, spatial operations, and display.
#'
#' Extracts polygon management responsibilities from UserData.
#' Receives a reference to the UserData instance (`data`) for access
#' to shared state (db, polygons, settings, step references, map).
#'
#' @noRd
#'
PolygonManager <- R6::R6Class(
  classname = 'PolygonManager',
  portable = FALSE,
  public = list(
    #' @field data Reference to the parent UserData instance.
    data = NULL,

    #' @description Create a new PolygonManager.
    #' @param data The UserData instance owning shared state.
    initialize = function(data) {
      self$data <- data
    },

    #' @description Update grid cell coverage for polygons based on tile data.
    #' @param polygons_sf An sf object containing the polygons to update.
    #' @return The updated polygons_sf with cells information added.
    updateCells = function(polygons_sf) {
      polygons_sf$cells_vect <- list(sf_empty())
      polygons_sf$cells_vect_rev <- list(sf_empty())

      for (i in seq_len(nrow(polygons_sf))) {
        polygon <- polygons_sf[i, ]

        if (polygon$selected) {
          tiles <- safe_db(
            data$db$dbGetQuery(
              'SELECT x, y, cells FROM tiles WHERE polygon = ? AND cells <> "000000000"',
              params = list(polygon$id_n)
            ),
            default = data.frame(
              x = integer(),
              y = integer(),
              cells = character()
            ),
            msg = "updateCells:dbGetQuery(tiles)"
          )

          if (nrow(tiles)) {
            cells <- tiles |>
              dplyr::mutate(key = seq_len(dplyr::n())) |>
              tidyr::crossing(data.frame(cell = (GRID_SIZE * GRID_SIZE):1)) |>
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
                nrows = (rg_y[2L] - rg_y[1L] + 1L) * GRID_SIZE,
                ncols = (rg_x[2L] - rg_x[1L] + 1L) * GRID_SIZE,
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
                x.cell = floor(abs(x - as.integer(x)) * GRID_SIZE),
                y.cell = floor(abs(y - as.integer(y)) * GRID_SIZE),
                cell = (GRID_SIZE * (GRID_SIZE - 1L)) -
                  y.cell * GRID_SIZE +
                  x.cell +
                  1L,
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

            rg_ll_tl <- slippymath::tilenum_to_lonlat(
              x = rg_x[1L],
              y = rg_y[2L] + 1L,
              zoom = ZOOM_TILE
            )
            rg_ll_br <- slippymath::tilenum_to_lonlat(
              x = rg_x[2L] + 1L,
              y = rg_y[1L],
              zoom = ZOOM_TILE
            )

            terra::ext(cells_rst) <- c(
              rg_ll_tl$lon,
              rg_ll_br$lon,
              rg_ll_tl$lat,
              rg_ll_br$lat
            )

            terra::crs(cells_rst) <- 'epsg:4326'
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

    #' @description Route new drawn feature to cut or add method.
    #' @param map Leaflet map proxy.
    #' @param feature The drawn feature from leafpm.
    drawNewFeature = function(map, feature) {
      feature_sf <- st_as_sf.feature(feature)

      if (sf::st_is(feature_sf, 'LINESTRING')) {
        cutPolygons(map, line = feature_sf)
      } else {
        addPolygon(map, polygon = feature_sf)
      }
    },

    #' @description Display guide polygon on leaflet map.
    #' @param map Leaflet map proxy.
    #' @return The modified map.
    displayGuidePolygon = function(map) {
      map <- map |>
        leaflet::clearGroup('guide_polygon')

      if (!is.null(data$guide_polygon)) {
        map <- map |>
          leaflet::addPolygons(
            data = data$guide_polygon,
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

    #' @description Display guide point(s) on leaflet map.
    #' @param map Leaflet map proxy.
    #' @return The modified map.
    displayGuidePoint = function(map) {
      map <- map |>
        leaflet::clearGroup('guide_point')

      if (!is.null(data$guide_point)) {
        map <- map |>
          leaflet::addCircleMarkers(
            data = data$guide_point,
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
                'border-color' = 'white',
                'box-shadow' = 'none',
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

    #' @description Invert selection state of all polygons.
    #' @param map Leaflet map proxy.
    invertSelection = function(map) {
      data$polygons_raw$selected <- !data$polygons$selected

      map |>
        setStyleFast(
          group = 'polygons',
          fill_opacities = as.list(ifelse(data$polygons$selected, 0.2, 0.01))
        )

      data$step_delimit$state <- utils::modifyList(
        data$step_delimit$state,
        list(modified = TRUE)
      )
    },

    #' @description Split polygons by a drawn line using lwgeom.
    #' @param map Leaflet map proxy.
    #' @param line An sf LINESTRING to split polygons with.
    #' @return The modified map with cleared drawing features.
    cutPolygons = function(map, line) {
      intersect <- sf::st_intersects(data$polygons, line, FALSE)

      if (any(intersect)) {
        polygons_intersect <- data$polygons[intersect, ]

        for (i in seq_len(nrow(polygons_intersect))) {
          polygon_intersect <- polygons_intersect[i, ]

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
              id_max = st_get_id_max(data$polygons, 'ZN')
            )

            data$polygons_raw <- data$polygons_raw |>
              dplyr::filter(id != polygons_intersect$id[i]) |>
              dplyr::bind_rows(polygons_split)

            leaflet::removeShape(map, layerId = polygons_intersect$id[i])
            map |>
              displayPolygons(sf = polygons_split, fit = FALSE)
          }
        }

        data$step_delimit$state <- utils::modifyList(
          data$step_delimit$state,
          list(modified = TRUE, mode = 'select')
        )
      }

      map |>
        pm_clear_features()
    },

    #' @description Validate, fix geometry, and add metadata columns.
    #' @param sf An sf object or NULL.
    #' @param id_max Integer offset for generating sequential IDs.
    #' @return A valid sf object with required metadata columns.
    makeValidPolygons = function(sf, id_max = 0L) {
      if (is.null(sf)) {
        sf <- sf_empty()
      }

      # Empty sf has no rows — return early with required columns
      if (nrow(sf) == 0L) {
        sf <- sf::st_set_crs(sf, 4326L)
        sf$id_n <- integer(0L)
        sf$id <- character(0L)
        sf$area <- units::set_units(numeric(0L), "m^2")
        sf$name <- character(0L)
        sf$label <- character(0L)
        sf$label_short <- integer(0L)
        sf$selected <- logical(0L)
        sf$focused <- logical(0L)
        return(sf)
      }

      sf <- sf::st_make_valid(sf)

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
          round(units::set_units(area, "km^2"), 2L)
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
            round(units::set_units(area, "km^2"), 2L)
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
            count = DEFAULT_SAMPLE_COUNT,
            size = DEFAULT_SAMPLE_SIZE,
            confidence = DEFAULT_CONFIDENCE,
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

    #' @description Display polygons on the leaflet map.
    #' @param map Leaflet map proxy.
    #' @param only_selected Logical; show only selected polygons.
    #' @param sample Logical; also display samples.
    #' @param cells Logical; also display cell coverage.
    #' @param plan Logical; show plan mode styling.
    #' @param fit Logical; fit map to polygon bounds.
    #' @param status Logical; show status styling.
    #' @param sf Optional sf object to render (defaults to all polygons).
    #' @param ... Additional arguments passed to addPolygons.
    #' @return The modified map.
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
        sf <- data$polygons

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
          for (polygon_idx in seq_len(nrow(data$polygons))) {
            if (data$polygons$selected[polygon_idx]) {
              cells_vect_rev <- data$polygons$cells_vect_rev[[polygon_idx]]

              if (nrow(cells_vect_rev) > 0L) {
                # FIXME: #8 image offset/misalignment issue
                leaflet::addPolygons(
                  map,
                  data = cells_vect_rev,
                  color = 'white',
                  fill = TRUE,
                  group = 'cells',
                  options = leaflet::pathOptions(pane = 'polygon_1'),
                  weight = 0.5
                )
              }
            }
          }
        }

        if (sample) {
          for (polygon_idx in seq_len(nrow(data$polygons))) {
            if (data$polygons$selected[polygon_idx]) {
              data$displaySample(
                map,
                polygon_idx = polygon_idx,
                plan = plan,
                status = status
              )
            }
          }
        }
      } else {
        sf <- sf::st_sf(sf::st_sfc(st_bbox_polygon(data$bbox_default)))

        fitToSpatialFeatureBounds(map, sf = sf)
      }

      pm_clear_features(map)

      map
    },

    #' @description Add a polygon with overlap and intersection handling.
    #' @param map Leaflet map proxy.
    #' @param polygon An sf polygon to add.
    addPolygon = function(map, polygon) {
      polygon <- makeValidPolygons(
        polygon,
        id_max = st_get_id_max(data$polygons, 'ZN')
      )

      if (nrow(data$polygons) == 0L) {
        data$polygons_raw <- polygon
      } else {
        join_covered <- sf::st_covered_by(data$polygons, polygon, FALSE)

        if (any(join_covered)) {
          map |>
            leaflet::removeShape(layerId = data$polygons$id[join_covered])

          data$polygons_raw <- data$polygons_raw |>
            dplyr::filter(id %in% data$polygons$id[!join_covered])
        }

        intersect <- sf::st_intersects(data$polygons, polygon, FALSE)

        if (any(intersect)) {
          polygons_intersect <- data$polygons[intersect, ]

          for (i in seq_len(nrow(polygons_intersect))) {
            polygon_diff <- sf::st_geometry(polygons_intersect[i, ])
            polygon_diff <- sf::st_sf(
              geometry = sf::st_difference(polygon_diff, polygon)
            )
            polygon_diff <- makeValidPolygons(
              polygon_diff,
              id_max = st_get_id_max(data$polygons, 'ZN') + 1L
            )

            data$polygons_raw <- data$polygons_raw |>
              dplyr::filter(id != polygons_intersect[i, ]$id) |>
              dplyr::bind_rows(polygon_diff)

            map |>
              leaflet::removeShape(layerId = polygons_intersect[i, ]$id) |>
              displayPolygons(sf = polygon_diff, fit = FALSE)
          }
        }

        data$polygons_raw <- dplyr::bind_rows(data$polygons_raw, polygon)
      }

      displayPolygons(map, sf = polygon, fit = FALSE)

      data$step_delimit$state <- utils::modifyList(
        data$step_delimit$state,
        list(modified = TRUE, mode = 'select')
      )
    },

    #' @description Delete a polygon by ID.
    #' @param map Leaflet map proxy.
    #' @param id The polygon ID to remove.
    deletePolygon = function(map, id) {
      data$polygons_raw <- dplyr::filter(data$polygons_raw, id != !!id)

      map |>
        leaflet::removeShape(layerId = id)

      data$step_delimit$state <- utils::modifyList(
        data$step_delimit$state,
        list(modified = TRUE)
      )
    },

    #' @description Update polygon geometry from an edited map feature.
    #' @param map Leaflet map proxy.
    #' @param feature The edited feature from leafpm.
    editPolygon = function(map, feature) {
      polygon <- st_as_sf.feature(feature)

      m <- match(polygon$layerId, data$polygons$id)

      if (!is.na(m)) {
        existing_type <- sf::st_geometry_type(
          data$polygons_raw[m, ],
          by_geometry = TRUE
        )
        new_geom <- sf::st_geometry(polygon)
        if (
          existing_type == 'MULTIPOLYGON' &&
            sf::st_geometry_type(polygon, by_geometry = TRUE) == 'POLYGON'
        ) {
          new_geom <- sf::st_cast(new_geom, 'MULTIPOLYGON')
        }

        data$polygons_raw[m, ] <- data$polygons_raw[m, ] |>
          sf::st_set_geometry(new_geom) |>
          dplyr::mutate(
            area = sf::st_area(data$polygons_raw[m, ]),
            label = sprintf(
              '<b>Polygon %s</b><br>%s (km\u00b2)',
              label_short,
              round(units::set_units(area, "km^2"), 2L)
            )
          )
      }

      data$step_delimit$state <- utils::modifyList(
        data$step_delimit$state,
        list(modified = TRUE)
      )
    },

    #' @description Toggle selection state of a polygon.
    #' @param map Leaflet map proxy.
    #' @param id The polygon ID to toggle.
    togglePolygonSelected = function(map, id) {
      m <- match(id, data$polygons$id)

      data$polygons_raw$selected[m] <- !data$polygons$selected[m]

      map |>
        setStyleFast(
          group = 'polygons',
          fill_opacities = as.list(ifelse(data$polygons$selected, 0.2, 0.01))
        )

      data$step_delimit$state <- utils::modifyList(
        data$step_delimit$state,
        list(modified = TRUE)
      )
    },

    #' @description Toggle focus state of a polygon per workflow step.
    #' @param map Leaflet map proxy.
    #' @param id The polygon ID to toggle focus on.
    #' @param group The leaflet group name.
    #' @param step Either "sample" or "result".
    togglePolygonFocused = function(map, id, group, step) {
      m <- match(id, data$polygons$id)

      if (step == 'sample') {
        data$polygons_raw$focused_sample[-m] <- FALSE
        data$polygons_raw$focused_sample[m] <- !data$polygons$focused_sample[m]

        focused <- data$polygons_selected$focused_sample
      } else if (step == 'result') {
        data$polygons_raw$focused_result[-m] <- FALSE
        data$polygons_raw$focused_result[m] <- !data$polygons$focused_result[m]

        focused <- data$polygons_selected$focused_result
      } else {
        focused <- rep(FALSE, nrow(data$polygons_selected))
      }

      setStylePolygonFocused(map, focused)

      for (i in seq_len(nrow(data$polygons))) {
        if (data$polygons$selected[i]) {
          pm_edit_stop(map, targetGroup = sprintf('QDR_%s', i))
        }
      }
    },

    #' @description Set visual focus styling on polygons.
    #' @param map Leaflet map proxy.
    #' @param focused Logical vector of focus states.
    #' @param group The leaflet group name.
    setStylePolygonFocused = function(map, focused, group = 'polygons') {
      map |>
        setStyleFast(
          group = 'polygons',
          colors = as.list(ifelse(
            focused,
            col2hex('yellow'),
            col2hex('yellow')
          )),
          weights = as.list(ifelse(
            focused,
            POLYGON_WEIGHT_FOCUSED,
            POLYGON_WEIGHT_DEFAULT
          ))
        )
    }
  )
)
