# styler: block

addPolygon <- function(map, sf, ...) {
  addPolygons(
    map,
    highlight = FALSE,
    sf = sf,
    group = 'polygons',
    ...
  )
}

addPolygons <- function(
  map,
  sf,
  group = 'polygons',
  color = 'yellow',
  dash_array = NULL,
  fill = TRUE,
  fill_opacity = 0.01,
  fit = TRUE,
  highlight = TRUE,
  highlight_color = color,
  highlight_weight = 2L,
  label = FALSE,
  pane = NULL,
  weight = 2L
) {
  if (highlight) {
    highlight_options <-
      leaflet::highlightOptions(
        stroke = TRUE,
        color = highlight_color,
        weight = highlight_weight
        # bringToFront = TRUE,
        # sendToBack = FALSE
      )
  } else {
    highlight_options <- NULL
  }

  if (label && nrow(sf) > 0L) {
    label_html <- lapply(sf$label, shiny::HTML)
  } else {
    label_html <- NA
  }

  if (!is.null(pane)) {
    panel_options <- leaflet::pathOptions(pane = pane)
  } else {
    panel_options <- NULL
  }

  map <- map |>
    leaflet::addPolygons(
      data = sf,
      dashArray = dash_array,
      color = color,
      opacity = 1L,
      stroke = TRUE,
      weight = weight,
      fill = fill,
      fillColor = color,
      fillOpacity = fill_opacity,
      group = group,
      options = panel_options,
      label = label_html,
      labelOptions = leaflet::labelOptions(
        noHide = TRUE,
        direction = 'center',
        textOnly = FALSE,
        textsize = '12px',
        style = list(
          'background-color' = '#ffffff22',
          border = 'none',
          'box-shadow' = 'none',
          color = 'white',
          'font-size' = '12px',
          'text-align' = 'center',
          'text-shadow' = '0px 0px 1px #000, 0px 0px 1px #000;'
        )
      ),
      layerId = ~id
    )

  if (fit) {
    map <- fitToSpatialFeatureBounds(map, sf = sf)
  }

  map
}

#' Add Roofs using Google Maps API
#'
#' This function takes a set of polygons and retrieves roof data using the Google Maps API.
#'
#' @param polygons A set of polygons in 'sfc_POLYGON' or 'sfc_MULTIPOLYGON' format.
#' @param dir The directory to save the retrieved images.
#' @param async_queue An optional asynchronous queue for progress tracking.
#'
#' @return A list containing the tiles and roofs retrieved from the Google Maps API.
#'
#' @examples
#' \dontrun{
#'   polygon <-  sf::st_set_crs(sf::st_sfc(st_bbox_polygon(c(13.105, 11.825, 13.11, 11.83))), 4326L)
#'
#'   addRoofsGoogle(polygons = polygon)
#' }
#'
addRoofsGoogle <- function(
  polygons,
  dir = getDirAppTemp(),
  async_queue = NULL
) {
  if (methods::is(polygons, 'sfc')) {
    polygons <- sf::st_as_sf(polygons)
  }

  checkmate::assertClass(polygons, classes = 'sf')
  checkmate::assertTRUE(sf::st_crs(polygons)$input == 'EPSG:4326')
  checkmate::assertTRUE(length(polygons) >= 1L)
  checkmate::assert(
    checkmate::checkTRUE(sf::st_is(polygons, type = 'POLYGON')),
    checkmate::checkTRUE(sf::st_is(polygons, type = 'MULTIPOLYGON'))
  )

  roofs_sf <- NULL

  for (i in seq_len(length(polygons))) {
    polygon <- polygons[i]

    bbox <- sf::st_bbox(polygon)

    tile_grid <- slippymath::bbox_to_tile_grid(bbox = bbox, zoom = 18L)
    tile_grid_sf <- tile_grid_to_sf(tile_grid)
    tile_grid_intersect <- sf::st_intersects(
      tile_grid_sf,
      polygon,
      sparse = FALSE
    )

    tiles <- tibble::as_tibble(tile_grid$tiles)
    tiles$building <- NA

    sf_polygon_roofs <- NULL

    tiles_dir <- dir
    tiles_n <- nrow(tiles)

    if (!is.null(async_queue)) {
      async_queue$producer$fireEval(
        expr = progress_polygon(progress = progress, polygon = polygon),
        env = list(
          progress = 100L * ((i - 1L) / length(polygons)),
          polygon = i
        )
      )
    }

    # -- Phase 1: Batch download all missing tile images in parallel ----------
    url_template <- paste0(
      'https://mt%s.google.com/vt/lyrs=m@169000000',
      '&x=%s',
      '&y=%s',
      '&z=%s',
      '&s=Gal',
      '&apistyle=s.t%%3A0|s.e%%3Al|p.v%%3Aoff,s.t%%3A3|s.e%%3Ag|p.v%%3Aoff',
      ',s.t%%3A1297|s.e%%3Ag.s|p.c%%3A%%23ffffff00|p.w%%3A2',
      ',s.t%%3A1297|s.e%%3Ag.f|p.c%%3A%%23ffff0000'
    )

    download_urls <- character(0L)
    download_destfiles <- character(0L)
    download_tile_idx <- integer(0L)

    for (t in seq_len(tiles_n)) {
      if (!tile_grid_intersect[t]) next

      query_png <- sprintf('%s/18_%s_%s.png', tiles_dir, tiles$x[t], tiles$y[t])

      if (!fs::file_exists(query_png)) {
        url <- sprintf(
          url_template,
          round(stats::runif(1L, 0L, 3L)),
          tiles$x[t],
          tiles$y[t],
          18L
        )
        download_urls <- c(download_urls, url)
        download_destfiles <- c(download_destfiles, query_png)
        download_tile_idx <- c(download_tile_idx, t)
      }
    }

    if (length(download_urls) > 0L) {
      tryCatch(
        curl::multi_download(
          urls = download_urls,
          destfiles = download_destfiles,
          progress = FALSE
        ),
        error = function(e) {
          logWarn(sprintf("addRoofsGoogle batch download: %s", e$message))
        }
      )
    }

    # -- Phase 2: Process downloaded tile images sequentially ------------------
    for (t in seq_len(tiles_n)) {
      if (!tile_grid_intersect[t]) next

      tile <- tiles[t, ]
      query_png <- sprintf('%s/18_%s_%s.png', tiles_dir, tile$x, tile$y)

      if (!fs::file_exists(query_png)) next

      progress_value <- 100L *
        ((i - 1L) / length(polygons) + t / tiles_n / length(polygons))

      tile_bbox <- slippymath::tile_bbox(tile$x, tile$y, 18L)

      img <- suppressWarnings(terra::rast(query_png))

      terra::crs(img) <- 'epsg:3857'
      terra::ext(img) <- as.vector(tile_bbox)[c(1L, 3L, 2L, 4L)]

      if (!is.null(async_queue)) {
        async_queue$producer$fireEval(
          expr = progress_tile(progress = progress, tile = tile),
          env = list(
            progress = progress_value,
            tile = tile
          )
        )
      }

      img_ct <- terra::coltab(img)[[1L]]
      img_ct_red <- img_ct$value[
        img_ct$red == 255L &
          img_ct$green == 0L &
          img_ct$blue == 0L
      ]

      if (length(img_ct_red) && (img_ct_red %in% terra::values(img))) {
        terra::values(img)[terra::values(img) != img_ct_red] <- NA

        sf_tile_roof <- terra::as.polygons(img)
        sf_tile_roof <- sf_tile_roof |>
          sf::st_as_sf() |>
          sf::st_set_agr('constant') |>
          sf::st_set_crs(3857L) |>
          sf::st_make_valid() |>
          sf::st_cast('POLYGON') |>
          st_add_coordinates()

        sf_tile_roof <- sf_tile_roof[, -1L]
        sf_tile_roof$x <- tile$x
        sf_tile_roof$y <- tile$y
        sf_tile_roof$polygon <- i

        sf_tile_roof_cell_x <- floor(
          ((sf_tile_roof$centroid_lon - tile_bbox$xmin) /
            (tile_bbox$xmax - tile_bbox$xmin)) *
            3L
        )
        sf_tile_roof_cell_y <- floor(
          ((sf_tile_roof$centroid_lat - tile_bbox$ymin) /
            (tile_bbox$ymax - tile_bbox$ymin)) *
            3L
        )
        sf_tile_roof_cell_xy <- sf_tile_roof_cell_y *
          3L +
          sf_tile_roof_cell_x +
          1L

        cells <- rep('0', 9L)
        cells[sort(unique(sf_tile_roof_cell_xy))] <- '1'

        tiles[t, 'cells'] <- paste(cells, collapse = '')

        sf_tile_roof_border <-
          st_bbox_polygon(tile_bbox) |>
          sf::st_cast('MULTILINESTRING')

        sf_tile_roof$within <- !sf::st_intersects(
          sf_tile_roof,
          sf_tile_roof_border,
          sparse = FALSE
        )

        if (is.null(sf_polygon_roofs)) {
          sf_polygon_roofs <- sf_tile_roof
        } else {
          sf_polygon_roofs <- dplyr::bind_rows(
            sf_polygon_roofs,
            sf_tile_roof
          )
        }
      }
    }

    if (!is.null(sf_polygon_roofs)) {
      polygon_roofs_geos_1 <- sf_polygon_roofs[sf_polygon_roofs$within, ] |>
        geos::as_geos_geometry() |>
        geos::geos_make_collection() |>
        geos::geos_make_valid() |>
        geos::geos_unary_union()

      polygon_roofs_geos_2 <- sf_polygon_roofs[!sf_polygon_roofs$within, ] |>
        geos::as_geos_geometry() |>
        geos::geos_make_collection() |>
        geos::geos_make_valid()

      polygon_roofs_geos <- c(
        polygon_roofs_geos_1,
        polygon_roofs_geos_2
      )

      polygon_roofs_geos <- geos::geos_unnest(
        polygon_roofs_geos,
        keep_multi = FALSE
      )
      polygon_roofs_geos_inside <-
        polygon_roofs_geos |>
        geos::geos_centroid() |>
        geos::geos_intersects(
          sf::st_transform(polygon, 3857L) |> geos::as_geos_geometry()
        )

      polygon_roofs_geos <- polygon_roofs_geos[polygon_roofs_geos_inside]

      sf_polygon_roofs <- polygon_roofs_geos |>
        sf::st_as_sf() |>
        dplyr::mutate(polygon = i) |>
        add_tilenum()
    }

    if (i == 1L) {
      roofs_sf <- sf_polygon_roofs
    } else {
      roofs_sf <- dplyr::bind_rows(roofs_sf, sf_polygon_roofs)
    }
  }

  if (!is.null(roofs_sf)) {
    roofs_sf <- roofs_sf |>
      sf::st_set_agr('constant') |>
      sf::st_centroid() |>
      sf::st_transform(4326L)
  }

  list(
    tiles = tiles,
    roofs = roofs_sf
  )
}

addRoofsOpenBuilding <- function(
  polygons,
  dir = getDirAppTemp(),
  async_queue = NULL
) {
  if (methods::is(polygons, 'sfc')) {
    polygons <- sf::st_as_sf(polygons)
  }

  checkmate::assertClass(polygons, classes = 'sf')
  checkmate::assertTRUE(sf::st_crs(polygons)$input == 'EPSG:4326')
  checkmate::assertTRUE(length(polygons) >= 1L)
  checkmate::assert(
    checkmate::checkTRUE(sf::st_is(polygons, type = 'POLYGON')),
    checkmate::checkTRUE(sf::st_is(polygons, type = 'MULTIPOLYGON'))
  )

  roofs_sf <- NULL

  for (i in seq_len(length(polygons))) {
    polygon <- polygons[i]

    bbox <- sf::st_bbox(polygon)

    s2_intersect <- sf::st_intersects(s2$layer, polygon, sparse = FALSE)
    s2_intersect_tile_urls <- s2$layer[s2_intersect, ]$tile_url

    sf_polygon_roofs <- NULL

    if (!is.null(async_queue)) {
      async_queue$producer$fireEval(
        expr = progress_polygon(progress = progress, polygon = polygon),
        env = list(
          progress = 100L * ((i - 1L) / length(polygons)),
          polygon = i
        )
      )
    }

    for (t in seq_along(s2_intersect_tile_urls)) {
      s2_intersect_tile_url <- s2_intersect_tile_urls[t]
      s2_intersect_tile_dir <- dir
      s2_intersect_tile_filename <- fs::path_file(s2_intersect_tile_url)
      s2_intersect_tile_path <- fs::path(dir, s2_intersect_tile_filename)

      download(
        url = s2_intersect_tile_url,
        destfile = s2_intersect_tile_filename,
        destdir = s2_intersect_tile_dir
      )

      if (!fs::file_exists(s2_intersect_tile_path)) next

      s2_intersect_tile <- arrow::open_csv_dataset(
        sources = s2_intersect_tile_path
      )

      tile_roofs <- s2_intersect_tile |>
        dplyr::filter(
          latitude >= bbox$ymin,
          latitude <= bbox$ymax,
          longitude >= bbox$xmin,
          longitude <= bbox$xmax
        ) |>
        dplyr::collect()

      if (nrow(tile_roofs) > 0L) {
        if (is.null(sf_polygon_roofs)) {
          sf_polygon_roofs <- tile_roofs
        } else {
          sf_polygon_roofs <- dplyr::bind_rows(sf_polygon_roofs, tile_roofs)
        }
      }
    }

    if (!is.null(sf_polygon_roofs) && nrow(sf_polygon_roofs) > 0L) {
      # Parse WKT geometry strings from CSV column
      polygon_roofs_geos <- sf_polygon_roofs$geometry |>
        geos::as_geos_geometry() |>
        geos::geos_make_collection() |>
        geos::geos_make_valid() |>
        geos::geos_unary_union()

      polygon_roofs_geos <- geos::geos_unnest(
        polygon_roofs_geos,
        keep_multi = FALSE
      )

      # Keep only roofs whose centroid falls inside the polygon (both WGS 84)
      polygon_roofs_geos <- wk::wk_set_crs(polygon_roofs_geos, 4326L)
      polygon_roofs_geos_inside <-
        polygon_roofs_geos |>
        geos::geos_centroid() |>
        geos::geos_intersects(
          geos::as_geos_geometry(polygon)
        )

      polygon_roofs_geos <- polygon_roofs_geos[polygon_roofs_geos_inside]

      sf_polygon_roofs <- polygon_roofs_geos |>
        sf::st_as_sf() |>
        dplyr::mutate(polygon = i)
    } else {
      sf_polygon_roofs <- NULL
    }

    if (i == 1L) {
      roofs_sf <- sf_polygon_roofs
    } else {
      roofs_sf <- dplyr::bind_rows(roofs_sf, sf_polygon_roofs)
    }
  }

  if (!is.null(roofs_sf)) {
    roofs_sf <- roofs_sf |>
      sf::st_make_valid() |>
      sf::st_set_agr('constant') |>
      sf::st_centroid() |>
      sf::st_transform(4326L)
  }

  list(
    roofs = roofs_sf
  )
}

calculatePolygonat <- function(
  qdr_pop,
  qdr_area,
  area = 10000L,
  error_confidence = 95L
) {
  qdr_pop <- dplyr::coalesce(qdr_pop, 0L)

  error <- 1L - error_confidence / 100L

  m <- length(qdr_pop)

  tvalue <- stats::qt(p = error / 2L, df = m - 1L, lower.tail = FALSE)

  pop_m2 <- qdr_pop / qdr_area
  pop_m2_sd <- stats::sd(pop_m2)
  pop_m2_se <- pop_m2_sd / sqrt(m)
  pop_m2_ci <- tvalue * pop_m2_se
  pop_m2_avg <- mean(pop_m2)

  pop <- pop_m2_avg * area
  pop_i <- pop_m2_ci * area

  # spatial dispersion
  spd <- stats::sd(qdr_pop)^2L / mean(qdr_pop)

  list(
    pop_m2_avg = round(pop_m2_avg, 6L),
    pop_m2_sd = round(pop_m2_sd, 6L),
    pop_m2_se = round(pop_m2_se, 6L),
    pop_m2_ci = round(pop_m2_ci, 6L),
    pop = round(pop),
    pop_i = round(pop_i),
    spd = round(spd, 6L),
    tvalue = round(tvalue, 6L)
  )
}

calculateQuadrat <- function(
  qdr_pop,
  qdr_size = 25L,
  area = 10000L,
  error_confidence = 95L
) {
  qdr_pop <- dplyr::coalesce(qdr_pop, 0L)

  error <- 1L - error_confidence / 100L

  m <- length(qdr_pop)

  tvalue <- stats::qt(p = error / 2L, df = m - 1L, lower.tail = FALSE)

  pop_m2 <- qdr_pop / (qdr_size^2L)
  pop_m2_sd <- stats::sd(pop_m2)
  pop_m2_se <- pop_m2_sd / sqrt(m)
  pop_m2_ci <- tvalue * pop_m2_se
  pop_m2_avg <- mean(pop_m2)

  pop <- pop_m2_avg * area
  pop_i <- pop_m2_ci * area

  # spatial dispersion
  spd <- stats::sd(qdr_pop)^2L / mean(qdr_pop)

  list(
    pop_m2_avg = round(pop_m2_avg, 6L),
    pop_m2_sd = round(pop_m2_sd, 6L),
    pop_m2_se = round(pop_m2_se, 6L),
    pop_m2_ci = round(pop_m2_ci, 6L),
    pop = round(pop),
    pop_i = round(pop_i),
    spd = round(spd, 6L),
    tvalue = round(tvalue, 6L)
  )
}

#' Calculate Sample Zero-Inflated Distribution
#'
#' This function calculates the population estimate and confidence interval for a sample zero-inflated distribution.
#'
#' @param smp_pop numeric vector of the sample data
#' @param n integer value indicating the number of bootstrap samples to take (default is 10000)
#' @param error_confidence integer value indicating the error confidence level (default is 95)
#' @param progress a progress bar object created by \code{\link[progress]{progress_bar}} (default is NULL)
#'
#' @return a list with the following elements:
#' \item{pop}{integer value of the estimated population}
#' \item{pop_i}{integer value of the population confidence interval}
#' \item{lambda}{numeric value of the estimated lambda parameter}
#' \item{prob}{numeric value of the estimated probability of zero inflation}
#'
#' @examples
#' data <- c(1L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 5L, 5L, 5L, 5L, 6L, 6L, 7L, 7L, 7L, 7L, 7L, 7L, 8L, 8L, 8L, 8L, 9L, 9L, 9L, 9L, 10L, 10L)
#' calculateSampleZeroInflatedDistribution(data)
#'
calculateSampleZeroInflatedDistribution <- function(
  smp_pop,
  n = 10000L,
  error_confidence = 95L,
  progress = NULL
) {
  if (length(smp_pop) == 0L) {
    return()
  }
  # On doit d'abord trouver les parametres de la distribution avec exces de Zero,
  # pour cela on doit faire une optimisation du maximum de vraisemblance

  # Optimisation du likelihood avec des parametres de depart arbitraires pour trouver les parametres optimaux
  otm_dzipois <- stats::optim(
    par = c(2.4, 0.2),
    # la fonction qui calcule la vraisemblance/likelihood
    fn = function(data, parameters) {
      lambda <- parameters[1L]
      prob_0 <- parameters[2L]
      vector_of_likelihoods <- suppressWarnings(dzipois(
        data,
        lambda,
        prob_0,
        log = TRUE
      ))
      ll <- -1L * sum(vector_of_likelihoods)
      ll
    },
    data = smp_pop
  )

  lambda <- otm_dzipois$par[1L]
  prob_0 <- otm_dzipois$par[2L]

  # pour estimer le CI on fait un bootstrap
  bootstr <- function(x, dt) {
    mean(sample(dt, length(dt), TRUE))
  }

  simusbs <- sapply(1:n, bootstr, smp_pop)
  simusbs <- n * simusbs

  # estimation de la population
  pop <- mean(simusbs)

  qtile <- c(
    (1L - error_confidence / 100L) / 2L,
    1L - (1L - error_confidence / 100L) / 2L
  )

  # Limite infereure et superieure de l interval de confiance
  pop_i <- stats::quantile(simusbs, qtile, na.rm = TRUE)

  list(
    pop = as.integer(pop),
    pop_i = pop_i,
    lambda = round(lambda, 6L),
    prob = round(prob_0, 6L)
  )
}

calculateTSquare <- function(
  d1,
  d2,
  p1 = NULL,
  p2 = NULL,
  p = NULL,
  area = 10000L,
  error_confidence = 95L
) {
  d1 <- dplyr::coalesce(d1, 0L)
  d2 <- dplyr::coalesce(d2, 0L)

  if (is.null(p)) {
    p1 <- dplyr::coalesce(p1, 0L)
    p2 <- dplyr::coalesce(p2, 0L)
    p <- p1 + p2
  } else {
    p <- dplyr::coalesce(p, 0L)
  }

  p <- p / 2L

  error <- 1L - error_confidence / 100L

  m <- length(p)

  t <- NA
  z <- NA
  z_p <- NA
  mc <- NA
  mc_p <- NA

  spd <- NA

  pop <- NA
  pop_i <- NA

  area_hh_sde <- NA

  if (
    (m > 0L) &&
      any(d1 != 0L) &&
      any(d2 != 0L) &&
      any(p != 0L)
  ) {
    p_hh <- sum(p) / m

    t <- sum(d1^2L / (d1^2L + d2^2L / 2L)) / m

    if (is.na(t)) {
      t <- 0L
    }

    z <- (t - 1L / 2L) / sqrt(1L / (12L * m))
    z_p <- 1L - stats::pnorm(abs(z))

    if (z_p < (error / 2L)) {
      # not random

      if (z > 0L) {
        spd <- 'not random (aggregated)'
      } else {
        spd <- 'not random (regular)'
      }

      area_hh <- (pi / m) * sqrt(sum(d1^2L) * sum(d2^2L) / 2L)
    } else {
      # random >  test 2

      mc <- ((48L * m) / (13L * m + 1L)) *
        (m *
          log(sum(d1^2L + (d2^2L / 2L)) / m) -
          sum(log(d1^2L + (d2^2L / 2L)))) # 42.8331
      mc_p <- 1L - stats::pchisq(mc, m - 1L)

      if (mc_p > 0.05) {
        spd <- 'random (homogeneous)'

        area_hh <- pi * (sum(d1^2L) + sum(d2^2L) / 2L) / (2L * m)
        area_hh_sde <- 1.96 * area_hh / sqrt(2L * m)
      } else {
        spd <- 'random (non-homogeneous)'

        area_hh <- (pi / m) * sqrt(sum(d1^2L) * sum(d2^2L) / 2L)
      }
    }

    pop <- (p_hh * 2L) * area / area_hh

    if (is.na(area_hh_sde)) {
      pop_i <- NA
    } else {
      pop_i <- p_hh * area / (area_hh + area_hh_sde)
    }
  }

  list(
    t = round(t, 6L),
    z = round(z, 6L),
    z_p = round(z_p, 6L),
    mc = round(mc, 6L),
    mc_p = round(mc_p, 6L),
    spd = spd,
    pop = round(pop),
    pop_i = round(pop_i)
  )
}

cleanOSM <- function(osm) {
  if (is.defined(osm$osm_polygons)) {
    names(osm$osm_polygons$geometry) <- NULL
  }
  if (is.defined(osm$osm_multipolygons)) {
    names(osm$osm_multipolygons$geometry) <- NULL
  }
  if (is.defined(osm$osm_points)) {
    names(osm$osm_points$geometry) <- NULL
  }
  osm
}

#' @title Sample Polygons
#' @description Creates an equal sample of n for each polygon in an
#'              sp Polygon class object
#'
#' @param x     sp class SpatialPolygons or SpatialPolygonsDataFrame object
#' @param n     Number of random samples
#' @param type  Type of sample with options for: 'random', 'regular',
#'              'nonaligned', 'hexagonal', 'clustered',
#'               'Fibonacci'. See 'spsample' for details.
#' @param ...   Additional arguments passed to spsample
#'
#' @return sp SpatialPointsDataFrame object
#'
#' @author Jeffrey S. Evans  <jeffrey_evans@@tnc.org>
#'
#' @noRd
#'
#' @source
#' The code has been found from VGAM package code written by Paul Bays
#' (https://www.stat.auckland.ac.nz/~yee/VGAM/) published under GPL-3 License.
#'
#' @noRd
#'
dzipois <- function(x, lambda, pstr0 = 0L, log = FALSE) {
  log.arg <- log

  if (!is.logical(log.arg) || length(log) != 1L) {
    stop('bad input for argument \'log\'')
  }

  rm(log)

  LLL <- max(length(x), length(lambda), length(pstr0))
  if (length(x) != LLL) x <- rep_len(x, LLL)
  if (length(lambda) != LLL) lambda <- rep_len(lambda, LLL)
  if (length(pstr0) != LLL) pstr0 <- rep_len(pstr0, LLL)

  ans <- x + lambda + pstr0

  index0 <- (x == 0L)
  if (log.arg) {
    ans[index0] <- log(
      pstr0[index0] +
        (1L - pstr0[index0]) * stats::dpois(x[index0], lambda[index0])
    )
    ans[!index0] <- log1p(-pstr0[!index0]) +
      stats::dpois(x[!index0], lambda[!index0], log = TRUE)
  } else {
    ans[index0] <- pstr0[index0] +
      (1L - pstr0[index0]) * stats::dpois(x[index0], lambda[index0])
    ans[!index0] <- (1L - pstr0[!index0]) *
      stats::dpois(x[!index0], lambda[!index0])
  }

  deflat.limit <- -1L / expm1(lambda)
  ans[pstr0 < deflat.limit] <- NaN
  ans[pstr0 > 1L] <- NaN

  ans
}

getOSMSpatialFeatures <- function(
  bbox,
  key,
  bbox_crop = FALSE,
  osm_multipolygons = FALSE
) {
  osm <-
    osmdata::opq(bbox = bbox) |>
    osmdata::add_osm_feature(key = key) |>
    osmdata::osmdata_sf()

  if (osm_multipolygons) {
    osm_sf <- osm$osm_multipolygons
  }

  if (bbox_crop) {
    osm_sf <- sf::st_crop(osm_sf, bbox)
  }

  for (i in seq_len(nrow(osm_sf))) {
    names(osm_sf$geometry[[i]][[1L]]) <- NULL
  }

  osm_sf
}

getSampleFilename <- function(polygon, ext) {
  file <- sprintf(
    '%s %s - %s.%s',
    tolower(df_sampling_method[polygon$type, 'label']),
    tolower(polygon$id),
    Sys.Date(),
    ext
  )
  file
}

key <- function(polygon_idx, point, method) {
  dplyr::case_when(
    method == 'SP_QDR' ~
      ((point * 100000L) + (polygon_idx * 10000L) + (1L * 1000L)) %% 97L,
    method == 'SP_TSQ' ~
      ((point * 100000L) + (polygon_idx * 10000L) + (2L * 1000L)) %% 97L,
    method == 'SP_SPV' ~
      ((point * 100000L) + (polygon_idx * 10000L) + (3L * 1000L)) %% 97L,
    method == 'SP_SMP' ~
      ((point * 100000L) + (polygon_idx * 10000L) + (4L * 1000L)) %% 97L,
    method == 'RS_SMP' ~
      ((point * 100000L) + (polygon_idx * 10000L) + (5L * 1000L)) %% 97L
  )
}

#' See the terra::new_rast function.
#'
#' Problem with extent invalid in latitude in hemisphere Sud
#'
#' @noRd
#'
new_rast <- function(
  nrows = 10L,
  ncols = 10L,
  nlyrs = 1L,
  xmin = 0L,
  xmax = 1L,
  ymin = 0L,
  ymax = 1L,
  crs,
  extent,
  resolution,
  vals,
  names,
  time,
  units
) {
  ncols <- round(ncols)
  if (ncols < 1L) error('rast', 'ncols < 1')
  nrows <- round(nrows)
  if (nrows < 1L) error('rast', 'nrows < 1')

  if (missing(extent)) {
    e <- c(xmin, xmax, ymin, ymax)
  } else {
    extent <- terra::ext(extent)
    e <- as.vector(extent)
  }
  if ((e[1L] >= e[2L]) || e[3L] >= e[4L]) {
    # error('rast,missing', 'invalid extent')
  }
  if (missing(crs)) {
    if (e[1L] > -360.01 && e[2L] < 360.01 && e[3L] > -90.01 && e[4L] < 90.01) {
      crs <- 'OGC:CRS84'
    } else {
      crs <- ''
    }
  } else {
    crs <- terra:::character_crs(crs, 'rast')
  }
  # check_proj4_datum(crs)

  r <- methods::new('SpatRaster')
  r@ptr <- terra:::SpatRaster$new(c(nrows, ncols, nlyrs), e, crs)
  r <- terra:::messages(r, 'rast')

  if (!missing(resolution)) {
    res(r) <- resolution
  }
  if (!missing(names)) {
    names(r) <- names
  }
  if (!missing(vals)) {
    if (length(vals) == 1L) {
      if (is.na(vals[1L])) {
        vals <- as.numeric(NA)
      }
    }
    terra::values(r) <- vals
  }
  if (!missing(time)) {
    time(r) <- time
  }
  if (!missing(units)) {
    time(r) <- units
  }
  r
}

parseSampleId <- function(id) {
  m <- stringr::str_match_all(id, '([0-9]+)/([0-9]+)')
  m <- m[[1L]]

  list(
    polygon = as.integer(m[, 2L]),
    id = as.integer(m[, 3L])
  )
}

plotMap <- function(nc, road = FALSE, force = FALSE) {
  bbox <- sf::st_bbox(nc)

  bbox[1L] <- bbox[1L] - (bbox[3L] - bbox[1L]) / 10L
  bbox[2L] <- bbox[2L] - (bbox[4L] - bbox[2L]) / 10L
  bbox[3L] <- bbox[3L] + (bbox[3L] - bbox[1L]) / 10L
  bbox[4L] <- bbox[4L] + (bbox[4L] - bbox[2L]) / 10L

  tile_grid <- slippymath::bbox_to_tile_grid(bbox, max_tiles = 100L)

  if (road) {
    provider <- list(
      src = 'GOOGLE_ROAD',
      q = 'https://mt{s}.google.com/vt/lyrs=m&x={x}&y={y}&z={z}&ext=jpg'
    )
  } else {
    provider <- list(
      src = 'GOOGLE_SAT',
      q = 'https://mt{s}.google.com/vt/lyrs=s&x={x}&y={y}&z={z}&ext=jpg'
    )
  }

  provider <- utils::modifyList(
    provider,
    list(sub = c('0', '1', '2', '3'), cit = '\u00a9 Google.')
  )

  r <- maptiles::get_tiles(
    x = bbox,
    zoom = tile_grid$zoom,
    provider = provider,
    crop = TRUE,
    cachedir = fs::path_temp(),
    verbose = TRUE,
    forceDownload = force
  )

  # r_df <- data.frame(terra::xyFromCell(r, 1:terra::ncell(r)), terra::getValues(r / 255))
  # r_df <- setNames(r_df, c('x', 'y', 'red', 'green', 'blue'))
  # r_df[, 1:2] <- merc_to_lonlat(r_df[, 1:2])

  p <- maptiles::plot_tiles(r, mar = c(0.5, 0.5, 0.5, 0.5))

  p
}

pointsToTiles <- function(polygon, points) {
  r <- polygonToRasterCells(polygon)
  r <- terra::mask(r, terra::vect(sf::st_geometry(points)))

  # terra::values(r) <- ifelse(
  #   is.na(terra::values(r)),
  #   rep(
  #       c(
  #         rep(1:3, ncol(r)/3)+0,
  #         rep(1:3, ncol(r)/3)+3,
  #         rep(1:3, ncol(r)/3)+6
  #       ), nrow(r)/3),
  #   20
  #   )

  # terra::plot(r)
  # terra::plot(terra::vect(sf::st_geometry(polygon)), add = TRUE)
  # terra::plot(terra::vect(sf::st_geometry(points)), add = TRUE)

  df <- as.data.frame(r, xy = TRUE, na.rm = FALSE)
  df <- df |>
    tibble::as_tibble() |>
    dplyr::transmute(
      x = (match(df$x, unique(df$x)) - 1L) %/% 3L,
      y = (match(df$y, unique(df$y)) - 1L) %/% 3L,
      n = seq_len(dplyr::n()),
      v = ifelse(is.na(builded), 0L, 1L),
      c = rep(
        c(
          rep(1:3, ncol(r) / 3L) + 6L,
          rep(1:3, ncol(r) / 3L) + 3L,
          rep(1:3, ncol(r) / 3L) + 0L
        ),
        nrow(r) / 3L
      )
    ) |>
    dplyr::arrange(y, x, c) |>
    dplyr::group_by(y, x) |>
    dplyr::summarize(v = paste(v, collapse = '')) |>
    dplyr::ungroup()

  tiles <-
    slippymath::bbox_to_tile_grid(
      bbox = sf::st_bbox(polygon),
      zoom = 18L
    )$tiles |>
    tibble::as_tibble() |>
    dplyr::mutate(
      n = seq_len(dplyr::n()),
      locked = NA_character_
    )

  tiles <-
    dplyr::mutate(
      tiles,
      # status = ifelse(!is.na(values(r)), 0L, -1L),
      status = 0L,
      cells = df$v
    )

  tiles
}

polygonToRasterCells <- function(polygon) {
  bbox <- sf::st_bbox(polygon)

  tile_grid <- slippymath::bbox_to_tile_grid(bbox = bbox, zoom = 18L)
  tile_grid_bbox <- tile_grid_bbox(tile_grid)

  tile_grid_xy_min <- slippymath::tilenum_to_lonlat(
    x = tile_grid$tiles$x[1L],
    y = tile_grid$tiles$y[1L],
    zoom = 18L
  )[1:2]
  tile_grid_xy_max <- slippymath::tilenum_to_lonlat(
    x = tile_grid$tiles$x[nrow(tile_grid$tiles)] + 1L,
    y = tile_grid$tiles$y[nrow(tile_grid$tiles)] + 1L,
    zoom = 18L
  )[1:2]

  tiles <- tile_grid$tiles

  rg_x <- range(tiles$x)
  rg_y <- range(tiles$y)

  polygon_cells_rst <- terra::rast(
    names = 'builded',
    xmin = as.double(tile_grid_xy_min$lon),
    xmax = as.double(tile_grid_xy_max$lon),
    ymin = as.double(tile_grid_xy_max$lat),
    ymax = as.double(tile_grid_xy_min$lat),
    nrows = (rg_y[2L] - rg_y[1L] + 1L) * 3L,
    ncols = (rg_x[2L] - rg_x[1L] + 1L) * 3L,
    nlyrs = 1L,
    vals = 1L
  )

  polygon_cells_rst <- terra::mask(
    polygon_cells_rst,
    terra::vect(sf::st_geometry(polygon)),
    overwrite = TRUE
  )

  polygon_cells_rst
}

polygonToTiles <- function(polygon) {
  bbox <- sf::st_bbox(polygon)

  tile_grid <- slippymath::bbox_to_tile_grid(bbox = bbox, zoom = 18L)
  tile_grid_bbox <- tile_grid_bbox(tile_grid)

  tiles <- tibble::as_tibble(tile_grid$tiles)

  rg_x <- range(tiles$x)
  rg_y <- range(tiles$y)

  r <- terra::rast(
    names = 'status',
    xmin = as.double(tile_grid_bbox$xmin),
    xmax = as.double(tile_grid_bbox$xmax),
    ymin = as.double(tile_grid_bbox$ymin),
    ymax = as.double(tile_grid_bbox$ymax),
    nrows = rg_y[2L] - rg_y[1L] + 1L,
    ncols = rg_x[2L] - rg_x[1L] + 1L,
    nlyrs = 1L,
    vals = rep(0L, nrow(tiles))
  )

  r <- terra::mask(r, terra::vect(polygon), updatevalue = -1L)

  tiles <- tiles |>
    tibble::as_tibble() |>
    dplyr::mutate(
      n = seq_len(dplyr::n()),
      locked = NA_character_
    )

  tiles <- tiles |>
    dplyr::mutate(
      status = terra::values(r),
      cells = '000000000'
    )

  tiles
}

progressBegin <- function(vars, text = 'Starting computation') {
  vars$progress_ptm <- proc.time()
  vars$progress_value <- 0L

  text <- sprintf('%s (0 %%)', text)

  shinybusy::show_modal_progress_line(
    duration = 900L,
    easing = 'easeOut',
    text = text,
    value = 0L
  )

  console.out(text)
}

progressBeginStep <- function(vars, steps, duration = 2L) {
  vars$progress <- 1L
  vars$progress_steps <- steps
  vars$progress_steps_duration <- duration / 100L
  vars$progress_steps_value <- vars$progress_value
}

progressEnd <- function() {
  shinybusy::remove_modal_progress()
}

progressUpdate <- function(vars, text) {
  vars$progress_value <- vars$progress_steps_value +
    (vars$progress / vars$progress_steps) * vars$progress_steps_duration
  vars$progress <- vars$progress + 1L

  text <- sprintf(
    '%s (%s %% / %s min)',
    text,
    round(100L * vars$progress_value, 2L),
    round(
      ((proc.time() - vars$progress_ptm)['elapsed'] /
        60L /
        vars$progress_value) *
        100L,
      2L
    )
  )

  shinybusy::update_modal_progress(
    text = text,
    value = vars$progress_value
  )
  console.out(text)
}

rast_empty <- function() {
  terra::rast(crs = 'epsg:4326')
}

rasterCellsToPolygon <- function(raster) {
  polygon_vect <- terra::as.polygons(raster, extent = FALSE)
  polygon_vect
}

#' Read Google Maps basemap tiles for given polygons
#'
#' This function reads Google Maps basemap tiles for the given polygons and returns a list of tile grids.
#'
#' @param polygons A spatial feature collection of polygons or multipolygons in EPSG:4326 projection
#'
#' @param async_queue An optional async queue object to track progress
#'
#' @return A list of tile grids
#'
#' @examples
#' # Create a polygon from North Carolina
#' nc <- sf::st_read(system.file('shape/nc.shp', package = 'sf'))
#' nc <- sf::st_transform(nc, 'EPSG:4326')
#'
#' # Read Google Maps basemap tiles for the polygon
#' readBasemapGoogle(nc[1L, ])
#'
readBasemapGoogle <- function(polygons, async_queue = NULL) {
  # checkmate::assert(
  #   checkmate::assert_class(polygons, classes = 'sfc_POLYGON'),
  #   checkmate::assert_class(polygons, classes = 'sfc_MULTIPOLYGON')
  # )
  checkmate::assert_true(sf::st_crs(polygons)$input == 'EPSG:4326')
  checkmate::assert_true(length(polygons) >= 1L)

  tiles_dir <- getDirApp('imageries')
  tile_grids <- list(tiles_count = 0L)

  for (i in seq_len(length(polygons))) {
    polygon <- polygons[i]
    tile_grids$polygons[[i]] <- list()
    for (zoom in 20:1) {
      tile_grid <- slippymath::bbox_to_tile_grid(
        bbox = sf::st_bbox(polygon),
        zoom = zoom
      )

      tile_grid_sf <- tile_grid_to_sf(tile_grid)
      tile_grid_intersect <- sf::st_intersects(
        tile_grid_sf,
        polygon,
        sparse = FALSE
      )
      tile_grid$tiles <- tile_grid$tiles[as.vector(tile_grid_intersect), ]
      tile_grids$tiles_count <- tile_grids$tiles_count + nrow(tile_grid$tiles)

      tile_grids$polygons[[i]]$tiles[[zoom]] <- tibble::as_tibble(
        tile_grid$tiles
      )
    }
  }

  tile_current <- 0L

  for (i in seq_len(length(polygons))) {
    if (!is.null(async_queue)) {
      async_queue$producer$fireEval(
        expr = {
          progress_polygon(polygon = polygon)
        },
        env = list(
          polygon = i
        )
      )
    }

    for (zoom in 20:1) {
      if (!is.null(async_queue)) {
        async_queue$producer$fireEval(
          expr = {
            progress_zoom()
          }
        )
      }

      tiles <- tile_grids$polygons[[i]]$tiles[[zoom]]
      tiles$zoom <- zoom

      for (t in seq_len(nrow(tiles))) {
        tile_current <- tile_current + 1L

        progress_value <- round(
          100L * (tile_current / tile_grids$tiles_count),
          2L
        )

        logDebug("Tile progress: %s%%", progress_value)

        tile <- tiles[t, ]

        query <- sprintf(
          paste0(
            'http://mt%s.google.com/vt/lyrs=s&hl=en',
            '&x=%s',
            '&y=%s',
            '&z=%s',
            '&s=Gal'
          ),
          round(stats::runif(1L, 0L, 3L)),
          tile$x,
          tile$y,
          zoom
        )
        query_png <- sprintf('%s/%s_%s_%s.png', tiles_dir, zoom, tile$x, tile$y)

        if (!fs::file_exists(query_png)) {
          result <- safe_download(
            url = query,
            destfile = query_png,
            quiet = TRUE,
            msg = "readBasemapGoogle"
          )
          if (is.null(result)) next
        }

        if (!is.null(async_queue) && nrow(tiles) > 1L) {
          async_queue$producer$fireEval(
            expr = {
              progress_tile(progress = progress, tile = tile)
            },
            env = list(
              progress = progress_value,
              tile = tile
            )
          )
        }
      }
    }
  }

  tile_grids
}

sf_empty <- function() {
  sf::st_sf(sf::st_sfc(crs = 'epsg:4326'))
}
