#' @title SampleManager
#' @description R6 class for sample generation, display, and manipulation.
#'
#' Extracts sampling responsibilities from UserData.
#' Receives a reference to the UserData instance (`data`) for access
#' to shared state (polygons, db, settings, step references).
#'
#' @noRd
#'
SampleManager <- R6::R6Class(
  classname = 'SampleManager',
  portable = FALSE,
  public = list(
    #' @field data Reference to the parent UserData instance.
    data = NULL,

    #' @description Create a new SampleManager.
    #' @param data The UserData instance owning polygon/sample state.
    initialize = function(data) {
      self$data <- data
    },

    #' @description Generate a single random sample point from cells.
    #' @param map Optional leaflet map proxy for display.
    #' @return TRUE if a sample was generated, NULL otherwise.
    addSample = function(map = NULL) {
      cells_vect <- data$polygon_focused$cells_vect[[1L]]

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

    #' @description Wrapper for adding multiple samples.
    #' @param map Leaflet map proxy.
    #' @param sf Sample sf object.
    #' @param ... Additional arguments.
    addSamples = function(map, sf, ...) {
      self$generateSamples(map, samples = sf)
    },

    #' @description Generate sample points for a polygon.
    #' @param map Optional leaflet map proxy for display.
    #' @param type Sampling type (SP_QDR, SP_TSQ, RS_SMP, SP_SMP, SP_SPV).
    #' @param method Sampling method (random, etc.).
    #' @param count Number of samples to generate.
    #' @param size Quadrat size in meters.
    #' @param polygon_idx Optional polygon index.
    #' @param samples Optional pre-existing samples sf to add.
    #' @return A list with count and quadrat sf, or NULL.
    generateSamples = function(
      map = NULL,
      type = 'SP_QDR',
      method = 'random',
      count = 35L,
      size = 25L,
      polygon_idx = NULL,
      samples = NULL
    ) {
      polygons_raw <- data$polygons_raw

      if (is.null(polygon_idx)) {
        polygon_idx <- match(TRUE, data$polygons$focused_sample)
      }

      if (is.na(polygon_idx)) {
        return()
      }

      if (!is.null(samples)) {
        intersect <- sf::st_intersects(
          samples,
          polygons_raw[polygon_idx, ],
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

      polygons_raw$type[polygon_idx] <- type
      polygons_raw$method[polygon_idx] <- method
      polygons_raw$count[polygon_idx] <- count

      if (type == 'SP_QDR') {
        polygons_raw$size[polygon_idx] <- size
      }

      polygon <- data$polygons[polygon_idx, ]
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
          polygon_roofs <- dplyr::filter(data$roofs, polygon == polygon_idx)
          polygon_roofs <- polygon_roofs |>
            sf::st_set_crs(4326L) |>
            sf::st_transform(3857L) |>
            dplyr::select(-id)

          polygon_pts <- polygon_roofs[
            sample(seq_len(nrow(polygon_roofs)), count),
          ]
        } else {
          if (type %in% c('SP_QDR', 'SP_TSQ')) {
            cells_vect <- data$polygons[polygon_idx, ]
          } else {
            cells_vect <- data$polygons[polygon_idx, ]$cells_vect[[1L]]
          }

          cells_r_pts_sf <- st_sample(
            polygons = cells_vect,
            n = count,
            type = method
          )

          polygon_pts <-
            sf::st_cast(cells_r_pts_sf, 'POINT') |>
            sf::st_transform(3857L)
          polygon_pts_n <- nrow(polygon_pts)
        }
      } else {
        if (!(type %in% c('SP_QDR', 'SP_TSQ'))) {
          equals <- sf::st_equals_exact(
            samples,
            polygons_raw$samples_sf[[polygon_idx]],
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

      samples_sf_names <- names(samples_sf)

      if (!('id_n' %in% samples_sf_names))
        samples_sf <- dplyr::mutate(samples_sf, id_n = seq_len(dplyr::n()))
      if (!('id_key' %in% samples_sf_names))
        samples_sf <- dplyr::mutate(samples_sf, id_key = NA_character_)
      if (!('id_key_calc' %in% samples_sf_names))
        samples_sf <- dplyr::mutate(
          samples_sf,
          id_key_calc = key(polygon_idx, id_n, data$project_method)
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
      } else {
        samples_quadrat_sf <- sf_empty()
      }

      polygons_raw$samples_sf[[polygon_idx]] <- samples_sf
      polygons_raw$samples_quadrat_sf[[polygon_idx]] <- samples_quadrat_sf

      data$polygons_raw <- polygons_raw

      if (!is.null(map)) {
        self$displaySample(map, polygon_idx = polygon_idx, status = FALSE)
      }

      data$step_sample$state <- utils::modifyList(
        data$step_sample$state,
        list(modified = TRUE)
      )

      list(
        count = nrow(samples_sf),
        quadrat = samples_quadrat_sf
      )
    },

    #' @description Append the current temporary sample to a polygon.
    #' @param map Optional leaflet map proxy.
    #' @param polygon_idx Optional polygon index.
    #' @return TRUE on success, NULL otherwise.
    sampleAppend = function(map = NULL, polygon_idx = NULL) {
      if (is.null(polygon_idx)) {
        polygon_idx <- match(TRUE, data$polygons$focused_sample)
      }

      if (is.na(polygon_idx)) {
        return()
      }

      if (is.null(private$.sample)) {
        return()
      }

      polygons_raw <- data$polygons_raw

      polygons_raw$type[polygon_idx] <- 'SP_SPV'

      samples_count <- nrow(data$polygons$samples_sf[[polygon_idx]])

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
          id_key_calc = key(polygon_idx, id_n, data$project_method),
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
        polygons_raw$samples_sf[[
          polygon_idx
        ]] <- polygons_raw$samples_sf[[polygon_idx]] |>
          dplyr::bind_rows(sample_sf)
      } else {
        polygons_raw$samples_sf[[polygon_idx]] <- sample_sf
      }

      data$polygons_raw <- polygons_raw

      if (!is.null(map)) {
        self$displaySample(
          map,
          polygon_idx = polygon_idx,
          samples_idx = samples_count + 1L,
          status = FALSE
        )
      }

      data$step_sample$state <- utils::modifyList(
        data$step_sample$state,
        list(modified = TRUE)
      )

      private$.sample <- NULL

      TRUE
    },

    #' @description Calculate population estimates from sample data.
    #' @param progress Optional progress callback.
    calculateSample = function(progress = NULL) {
      polygon_selected <- match(TRUE, data$polygons$focused_result)

      if (is.na(polygon_selected)) {
        return()
      }

      polygons_raw <- data$polygons_raw

      df <- data$polygons$samples_sf[[polygon_selected]]

      if (nrow(df) > 0L) {
        if (data$project_method == 'SP_QDR') {
          r_u5 <- calculateQuadrat(
            qdr_pop = as.integer(df$pop_u5),
            qdr_size = as.integer(data$polygons[polygon_selected, ]$size),
            area = as.integer(data$polygons[polygon_selected, ]$area),
            error_confidence = 95L
          )

          r_a5 <- calculateQuadrat(
            qdr_pop = as.integer(df$pop_a5),
            qdr_size = as.integer(data$polygons[polygon_selected, ]$size),
            area = as.integer(data$polygons[polygon_selected, ]$area),
            error_confidence = 95L
          )

          r_t <- calculateQuadrat(
            qdr_pop = as.integer(df$pop_u5) + as.integer(df$pop_a5),
            qdr_size = as.integer(data$polygons[polygon_selected, ]$size),
            area = as.integer(data$polygons[polygon_selected, ]$area),
            error_confidence = 95L
          )

          polygons_raw[polygon_selected, ]$pop_u5_i <- r_u5$pop_i
          polygons_raw[polygon_selected, ]$pop_u5 <- r_u5$pop
          polygons_raw[polygon_selected, ]$pop_a5_i <- r_a5$pop_i
          polygons_raw[polygon_selected, ]$pop_a5 <- r_a5$pop
          polygons_raw[polygon_selected, ]$pop_i <- r_t$pop_i
          polygons_raw[polygon_selected, ]$pop <- r_t$pop
          polygons_raw[polygon_selected, ]$tvalue <- r_u5$tvalue
          polygons_raw[polygon_selected, ]$t <- NA
          polygons_raw[polygon_selected, ]$z <- NA
          polygons_raw[polygon_selected, ]$z_p <- NA
          polygons_raw[polygon_selected, ]$mc <- NA
          polygons_raw[polygon_selected, ]$mc_p <- NA
        } else if (data$project_method == 'POL') {
          r_u5 <- calculatePolygonat(
            qdr_pop = as.integer(df$pop_u5),
            qdr_area = as.integer(df$area),
            area = as.integer(data$polygons[polygon_selected, ]$area),
            error_confidence = 95L
          )

          r_a5 <- calculatePolygonat(
            qdr_pop = as.integer(df$pop_a5),
            qdr_area = as.integer(df$area),
            area = as.integer(data$polygons[polygon_selected, ]$area),
            error_confidence = 95L
          )

          r_t <- calculatePolygonat(
            qdr_pop = as.integer(df$pop_u5) + as.integer(df$pop_a5),
            qdr_area = as.integer(df$area),
            area = as.integer(data$polygons[polygon_selected, ]$area),
            error_confidence = 95L
          )

          polygons_raw[polygon_selected, ]$pop_u5_i <- r_u5$pop_i
          polygons_raw[polygon_selected, ]$pop_u5 <- r_u5$pop
          polygons_raw[polygon_selected, ]$pop_a5_i <- r_a5$pop_i
          polygons_raw[polygon_selected, ]$pop_a5 <- r_a5$pop
          polygons_raw[polygon_selected, ]$pop_i <- r_t$pop_i
          polygons_raw[polygon_selected, ]$pop <- r_t$pop
          polygons_raw[polygon_selected, ]$tvalue <- r_u5$tvalue
          polygons_raw[polygon_selected, ]$t <- NA
          polygons_raw[polygon_selected, ]$z <- NA
          polygons_raw[polygon_selected, ]$z_p <- NA
          polygons_raw[polygon_selected, ]$mc <- NA
          polygons_raw[polygon_selected, ]$mc_p <- NA
        } else if (data$project_method == 'SP_TSQ') {
          r_u5 <- calculateTSquare(
            d1 = df$d1,
            d2 = df$d2,
            p1 = df$pop_u5_1,
            p2 = df$pop_u5_2,
            area = as.integer(data$polygons[polygon_selected, ]$area),
            error_confidence = 95L
          )

          r_a5 <- calculateTSquare(
            d1 = df$d1,
            d2 = df$d2,
            p1 = df$pop_a5_1,
            p2 = df$pop_a5_2,
            area = as.integer(data$polygons[polygon_selected, ]$area),
            error_confidence = 95L
          )

          r_t <- calculateTSquare(
            d1 = df$d1,
            d2 = df$d2,
            p1 = df$pop_u5_1 + df$pop_a5_1,
            p2 = df$pop_u5_1 + df$pop_a5_2,
            area = as.integer(data$polygons[polygon_selected, ]$area),
            error_confidence = 95L
          )

          polygons_raw[polygon_selected, ]$pop_u5_i <- r_u5$pop_i
          polygons_raw[polygon_selected, ]$pop_u5 <- r_u5$pop
          polygons_raw[polygon_selected, ]$pop_a5_i <- r_a5$pop_i
          polygons_raw[polygon_selected, ]$pop_a5 <- r_a5$pop
          polygons_raw[polygon_selected, ]$pop_i <- r_t$pop_i
          polygons_raw[polygon_selected, ]$pop <- r_t$pop
          polygons_raw[polygon_selected, ]$tvalue <- NA
          polygons_raw[polygon_selected, ]$t <- r_u5$t
          polygons_raw[polygon_selected, ]$z <- r_u5$z
          polygons_raw[polygon_selected, ]$z_p <- r_u5$z_p
          polygons_raw[polygon_selected, ]$mc <- r_u5$mc
          polygons_raw[polygon_selected, ]$mc_p <- r_u5$mc_p
        } else {
          count <- data$polygons$roofs_count[polygon_selected]

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

          polygons_raw[
            polygon_selected,
          ]$pop_u5_il <- as.integer(r_u5$pop_i[1L])
          polygons_raw[
            polygon_selected,
          ]$pop_u5_iu <- as.integer(r_u5$pop_i[2L])
          polygons_raw[polygon_selected, ]$pop_u5 <- r_u5$pop
          polygons_raw[
            polygon_selected,
          ]$pop_a5_il <- as.integer(r_a5$pop_i[1L])
          polygons_raw[
            polygon_selected,
          ]$pop_a5_iu <- as.integer(r_a5$pop_i[2L])
          polygons_raw[polygon_selected, ]$pop_a5 <- r_a5$pop
          polygons_raw[polygon_selected, ]$pop_il <- as.integer(r_t$pop_i[
            1L
          ])
          polygons_raw[polygon_selected, ]$pop_iu <- as.integer(r_t$pop_i[
            2L
          ])
          polygons_raw[polygon_selected, ]$pop <- r_t$pop
          polygons_raw[polygon_selected, ]$tvalue <- NA
          polygons_raw[polygon_selected, ]$t <- NA
          polygons_raw[polygon_selected, ]$z <- NA
          polygons_raw[polygon_selected, ]$z_p <- NA
          polygons_raw[polygon_selected, ]$mc <- NA
          polygons_raw[polygon_selected, ]$mc_p <- NA
        }
      }

      data$polygons_raw <- polygons_raw

      data$step_result$state <- utils::modifyList(
        data$step_result$state,
        list(modified = TRUE, void = rnorm(1L))
      )
    },

    #' @description Display sample points on the leaflet map.
    #' @param map Leaflet map proxy.
    #' @param polygon_idx Index of the polygon whose samples to display.
    #' @param samples_idx Optional specific sample indices to display.
    #' @param plan Whether in plan mode.
    #' @param fit Whether to fit map bounds to polygon.
    #' @param tooltip Whether to show tooltips.
    #' @param status Whether to show status colors.
    #' @param ... Additional arguments.
    #' @return The modified map proxy.
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

      samples <- data$polygons$samples_sf[[polygon_idx]]
      samples_quadrats <- data$polygons$samples_quadrat_sf[[polygon_idx]]

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

        if (data$polygons$type[polygon_idx] == 'SP_QDR') {
          map |>
            leaflet::addPolygons(
              data = samples_quadrats,
              color = 'yellow',
              fill = TRUE,
              fillColor = 'yellow',
              fillOpacity = 0.2,
              layerId = ~id,
              stroke = TRUE,
              weight = 1L,
              opacity = 1L,
              group = group_quadrat
            )
        }

        if (fit) {
          map |>
            fitToSpatialFeatureBounds(sf = data$polygons[polygon_idx, ])
        }
      }

      map
    },

    #' @description Display sample data to a rhandsontable.
    #' @param table_id Table widget ID.
    #' @param polygon_idx Polygon index.
    #' @param samples_idx Optional sample indices.
    #' @param plan Whether in plan mode.
    #' @param tooltip Whether to show tooltips.
    #' @param ... Additional arguments.
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

      samples <- data$polygons$samples_sf[[polygon_idx]]

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

    #' @description Edit a quadrat on the map.
    #' @param map Leaflet map proxy.
    #' @param id Quadrat ID.
    #' @param ... Additional arguments.
    #' @return The modified map proxy.
    sampleQuadratEdit = function(map, id, ...) {
      for (i in seq_len(nrow(data$polygons))) {
        m <- match(id, data$polygons$samples_quadrat_sf[[i]]$id)

        if (!is.na(m)) {
          sample_quadrat <- data$polygons$samples_quadrat_sf[[i]][m, ]

          map |>
            pm_edit_feature(
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

    #' @description Post a quadrat edit from the map.
    #' @param map Leaflet map proxy (unused).
    #' @param feature The edited feature from leafpm.
    sampleQuadratPost = function(map, feature) {
      quadrat <-
        st_as_sf.feature(feature)

      polygons_raw <- data$polygons_raw

      for (i in seq_len(nrow(data$polygons))) {
        if (data$polygons$selected[i]) {
          m <- match(quadrat$layerId, data$polygons$samples_quadrat_sf[[i]]$id)

          if (!is.na(m)) {
            polygons_raw$samples_quadrat_sf[[i]][
              m,
            ] <- polygons_raw$samples_quadrat_sf[[i]][m, ] |>
              sf::st_set_geometry(sf::st_geometry(quadrat))

            polygons_raw$samples_sf[[i]][m, ]$type <- 'POL'
            polygons_raw$samples_sf[[
              i
            ]]$area <- as.integer(sf::st_area(data$polygons$samples_quadrat_sf[[
              i
            ]]))
            polygons_raw$samples_quadrat_sf[[i]][m, ]$type <- 'POL'
            polygons_raw$samples_quadrat_sf[[
              i
            ]]$area <- as.integer(sf::st_area(data$polygons$samples_quadrat_sf[[
              i
            ]]))

            data$polygons_raw <- polygons_raw

            data$step_sample$state <- utils::modifyList(
              data$step_sample$state,
              list(modified = TRUE)
            )

            break
          }
        }
      }
    },

    #' @description Load sample data into a rhandsontable.
    #' @param tbl_id Table widget ID.
    sampleLoadTable = function(tbl_id) {
      if (is.null(data$polygons)) {
        return()
      }

      polygon <- dplyr::filter(data$polygons, focused_result)

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

      hot_load_data(
        id = tbl_id,
        data = samples,
        session = data$application$session
      )
      hot_render(id = tbl_id, session = data$application$session)
    },

    #' @description Toggle selection of a sample point.
    #' @param map Leaflet map proxy.
    #' @param sample Sample object with polygon and id fields.
    sampleToggleSelect = function(map, sample) {
      self$displaySample(
        map,
        polygon_idx = match(sample$polygon, data$polygons$id_n),
        samples_idx = sample$id
      )

      data$step_result$state <- utils::modifyList(
        data$step_result$state,
        list(modified = TRUE)
      )
    },

    #' @description Set sample values from table edits.
    #' @param map Leaflet map proxy.
    #' @param recs Row indices.
    #' @param vars Variable names.
    #' @param values New values.
    samplesSetValueFromTable = function(map, recs, vars, values) {
      if (length(recs) == 0L) {
        return()
      }

      values <- unlist(values)
      values[is.null(values)] <- NA

      polygon_idx <- match(TRUE, data$polygons$focused_result)

      polygons_raw <- data$polygons_raw

      for (var in unique(vars)) {
        polygons_raw[polygon_idx, ]$samples_sf[[1L]][
          recs[vars == var],
          var
        ] <- values[vars == var]
      }

      polygons_raw[polygon_idx, ]$samples_sf[[1L]] <- polygons_raw[
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

      data$polygons_raw <- polygons_raw

      self$displaySample(map, polygon_idx = polygon_idx, samples_idx = recs)

      data$step_result$state <- utils::modifyList(
        data$step_result$state,
        list(modified = TRUE)
      )
    },

    #' @description Set a sample value from a map interaction.
    #' @param table_id Table widget ID.
    #' @param id Sample row index.
    #' @param var Variable name.
    #' @param value New value.
    samplesSetValueFromMap = function(table_id, id, var, value) {
      if (is.null(value)) {
        value <- NA
      }

      polygon_idx <- match(TRUE, data$polygons$focused_result)

      polygons_raw <- data$polygons_raw

      polygons_raw[polygon_idx, ]$samples_sf[[1L]][
        as.integer(id),
        var
      ] <- value
      polygons_raw[polygon_idx, ]$samples_sf[[1L]] <-
        dplyr::mutate(
          polygons_raw[polygon_idx, ]$samples_sf[[1L]],
          color = dplyr::case_when(
            status == 'Excluded' ~ 'gray',
            status == 'To Be Done' ~ 'orange',
            status == 'In Progress' ~ 'cyan',
            status == 'Done' ~ 'red',
            TRUE ~ 'yellow'
          )
        )

      data$polygons_raw <- polygons_raw

      self$sampleDisplayToTable(
        table_id,
        polygon_idx = polygon_idx,
        samples_idx = as.integer(id)
      )

      data$step_result$state <- utils::modifyList(
        data$step_result$state,
        list(modified = TRUE)
      )
    }
  ),
  private = list(
    .sample = NULL
  )
)
