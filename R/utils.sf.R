# styler: block

#' @keywords internal
#'
combine_list_of_sf <- function(sf_list, crs = sf::st_crs(sf_list[[1L]])) {
  if (length(sf_list) == 0L) {
    NULL
  }
  props <- dplyr::bind_rows(
    lapply(
      sf_list,
      function(x) {
        dplyr::select_(
          as.data.frame(x, stringsAsFactors = FALSE),
          paste0('-', attr(x, 'sf_column', exact = TRUE))
        )
      }
    )
  )

  sf::st_sf(
    props,
    geometry = sf::st_sfc(
      unlist(lapply(sf_list, function(x) sf::st_geometry(x)), recursive = FALSE)
    ),
    crs = sf::st_crs(crs)
  )
}

getBBoxCentroid <- function(bbox) {
  pt <- list()
  pt$lon <- as.double(bbox$xmin + (bbox$xmax - bbox$xmin) %/% 2L)
  pt$lat <- as.double(bbox$ymin + (bbox$ymax - bbox$ymin) %/% 2L)
  pt
}

getKeyName <- function(x) {
  checkmate::assertClass(x, 'sf')
  x <- x |>
    sf::st_set_geometry(NULL) |>
    dplyr::select(where(is.character)) |>
    dplyr::mutate_all(~ gsub('[0-9]', '', .)) |>
    dplyr::summarize_all(~ length(unique(.)))
  if (ncol(x) > 0L) {
    names(x[match(max(x), x)])
  } else {
    NULL
  }
}

readSpatialLayer <- function(
  file,
  layer = NULL,
  simplify = TRUE,
  keep = 0.05,
  verbose = TRUE
) {
  if (tools::file_ext(file) == 'zip') {
    path <- fs::path_temp('layers')
    if (fs::dir_exists(path)) {
      fs::dir_delete(path)
    }
    zip::unzip(zipfile = file, exdir = path, junkpaths = TRUE)
    layers <- sf::st_layers(path)
    layers_dsn <- path
  } else {
    layers <- sf::st_layers(file)
    layers_dsn <- file
  }
  if (verbose) {
    logDebug("Layers: %s", paste(layers$name, collapse = ", "))
  }
  layers <-
    tibble::tibble(
      name = layers$name,
      geomtype = layers$geomtype[[1L]],
      driver = layers$driver
    )
  layers <- layers |>
    dplyr::filter(driver %in% c('ESRI Shapefile', 'GeoJSON'))
  if (!is.null(layer)) {
    layers <- layers |>
      dplyr::filter(name == layer)
  }
  layer_sf <- NULL
  if (nrow(layers) == 0L) {
    if (!is.null(layer)) {
      warning(sprintf("This file doesn't seem to contain a %s file.", layer))
    } else {
      warning("This file doesn't seem to contain shapefiles.")
    }
  } else {
    if (nrow(layers) > 1L) {
      warning(
        'This file contains several shapefiles. Please keep one shapefile by zip.'
      )
    }

    layer_sf <- sf::st_read(
      dsn = layers_dsn,
      layer = layers$name[1L],
      quiet = !verbose
    )
    layer_sf_df <- sf::st_drop_geometry(layer_sf)

    if (
      simplify &&
        as.character(unique(sf::st_geometry_type(layer_sf))) %in%
          c('POLYGON', 'MULTIPOLYGON')
    ) {
      # layer_sf <- rmapshaper::ms_simplify(layer_sf, keep = keep, keep_shapes = TRUE)
    }

    layer_sf_df_key <- NA
    layer_sf_df_key_scores <- sapply(layer_sf_df, score_unique)

    if (any(layer_sf_df_key_scores > 0L)) {
      layer_sf_df_key_scores <- layer_sf_df_key_scores[
        layer_sf_df_key_scores > 0L
      ]
      layer_sf_df_key_scores <- layer_sf_df_key_scores[
        layer_sf_df_key_scores == max(layer_sf_df_key_scores)
      ]
      if (length(layer_sf_df_key_scores)) {
        layer_sf_df_key <- names(layer_sf_df_key_scores[
          layer_sf_df_key_scores == max(layer_sf_df_key_scores)
        ])[1L]
      }
    }

    layer_sf_df[, 'lf_key'] <- seq_len(nrow(layer_sf_df))
    if (is.na(layer_sf_df_key)) {
      layer_sf_df[, 'lf_name'] <- seq_len(nrow(layer_sf_df))
    } else {
      layer_sf_df[, 'lf_name'] <- sprintf(
        '%0.3d / %s',
        seq_len(nrow(layer_sf_df)),
        stringr::str_to_title(layer_sf_df[, layer_sf_df_key])
      )
    }

    layer_sf <- layer_sf |>
      sf::st_geometry() |>
      # rmapshaper::ms_simplify(keep = 0.02, keep_shapes = FALSE) |>
      # geos::as_geos_geometry() |>
      # geos::geos_simplify_preserve_topology(tolerance = 0.0005) |>
      sf::st_as_sf() |>
      sf::st_set_geometry('geometry') |>
      sf::st_make_valid() |>
      dplyr::bind_cols(layer_sf_df)
  }
  layer_geometry_type_tbl <- table(sf::st_geometry_type(layer_sf))
  layer_geometry_type_tbl <- layer_geometry_type_tbl[
    layer_geometry_type_tbl > 0L
  ]
  list(
    layers = layers,
    layer = layer_sf,
    layer_geometry_types = layer_geometry_type_tbl
  )
}

writeSpatialLayer <- function(sf, file, layer, fieldname) {
  driver <- tools::file_ext(file)
  if (!is.null(sf)) {
    if (driver == 'zip') {
      d <- fs::path_temp('shapefile')
      if (fs::dir_exists(d)) {
        fs::dir_delete(d)
      }
      fs::dir_create(d, recurse = TRUE)
      sf::st_write(
        sf,
        driver = 'ESRI Shapefile',
        dsn = d,
        layer = layer,
        overwrite = TRUE,
        append = FALSE
      )
      fp <- fs::path(d, sprintf('%s.zip', layer))
      zip::zip(
        zipfile = fp,
        files = c(
          sprintf('%s.dbf', layer),
          sprintf('%s.prj', layer),
          sprintf('%s.shp', layer),
          sprintf('%s.shx', layer)
        ),
        root = d
      )
      fs::file_copy(path = fp, new_path = file, overwrite = TRUE)
    } else if (driver == 'kml') {
      fp <- fs::file_temp()
      sf$name <- sf[[fieldname]]
      sf::st_write(sf, driver = 'kml', dsn = fp, layer = layer, append = FALSE)
      fs::file_copy(path = fp, new_path = file, overwrite = TRUE)
    } else if (driver == 'gpx') {
      fp <- fs::file_temp()
      sf$Name <- sf[[fieldname]]
      sf::st_write(
        sf,
        dsn = fp,
        layer = 'waypoints',
        driver = 'GPX',
        dataset_options = 'GPX_USE_EXTENSIONS=yes'
      )
      fs::file_copy(path = fp, new_path = file, overwrite = TRUE)
    }
  }
}

# styler: block sf

st_add_coordinates <- function(sf) {
  cc <- sf::st_coordinates(st_centroid_ll(sf::st_geometry(sf)))

  sf$centroid_lon <- cc[, 1L]
  sf$centroid_lat <- cc[, 2L]

  sf
}

st_as_df <- function(x, geometry, names = c('lon', 'lat')) {
  if (nrow(x) > 0L) {
    if (missing(geometry)) {
      geometry <- sf::st_geometry(x)
    } else {
      geometry <- rlang::eval_tidy(rlang::enquo(geometry), x)
    }
    ret <- sf::st_coordinates(geometry)
    ret <- tibble::as_tibble(ret)
    stopifnot(length(names) == ncol(ret))
  } else {
    ret <- data.frame(x = as.double(), y = as.double())
  }
  ret <- stats::setNames(ret, names)
  x <- sf::st_set_geometry(x, NULL)
  x <- x[, !names(x) %in% names]
  dplyr::bind_cols(x, ret)
}

#' @keywords internal
#'
st_as_sf.feature <- function(x, crs = 4326L, ...) {
  if (x$type != 'Feature') {
    stop("should be of type 'Feature'", call. = FALSE)
  }

  sf <- st_as_sfc.feature(x)

  suppressWarnings({
    sf::st_crs(sf) <- crs
  })

  sf
}

#' @keywords internal
#'
st_as_sfc.feature <- function(x, crs = 4326L, ...) {
  sf <- sf::read_sf(
    jsonlite::toJSON(x, auto_unbox = TRUE, force = TRUE, digits = NA)
  )

  suppressWarnings({
    sf::st_crs(sf) <- crs
  })

  sf
}

#' @keywords internal
#'
st_bbox_polygon <- function(bbox, crs = 4326L, ...) {
  sf::st_polygon(list(matrix(
    bbox[c(1L, 2L, 3L, 2L, 3L, 4L, 1L, 4L, 1L, 2L)],
    ncol = 2L,
    byrow = TRUE
  )))
}

st_centroid_ll <- function(sf) {
  sf_crs <- sf::st_crs(sf)
  sf <- suppressWarnings(
    sf |>
      sf::st_transform(3857L) |>
      sf::st_centroid() |>
      sf::st_transform(sf_crs)
  )
  sf
}

st_get_id_max <- function(sf, prefix = 'sf') {
  if (is.null(sf)) {
    0L
  } else if (nrow(sf) == 0L) {
    0L
  } else {
    max(as.integer(gsub(sprintf('%s_', prefix), '', sf$id)))
  }
}

st_id <- function(id, prefix = 'sf') {
  id <- sprintf('%s_%s', prefix, id)
  id
}

st_id_new <- function(sf, prefix = 'sf') {
  id <- st_id(nrow(sf) + 1L, prefix)
  id
}

st_intersects_quiet <- function(...) {
  suppressWarnings(sf::st_intersects(...))
}

st_read <- function(..., add_coordinates = TRUE) {
  sf <- sf::st_read(...)

  sf <- sf |>
    normalizeDataframe() |>
    sf::st_transform(crs = 4326L) |>
    sf::st_set_agr('constant')

  if (add_coordinates) {
    sf <- st_add_coordinates(sf)
  }

  sf
}

st_sample <- function(polygons, n = 1L, type = 'random') {
  if (n == 1L && type == 'random') {
    samples <- sf::st_sample(polygons, size = n, by_polygon = FALSE)
  } else {
    samples <- sf::as_Spatial(polygons)
    samples <- sp::spsample(samples, n = n, type = type, iter = 100L)
    samples <- samples |>
      sf::st_as_sf() |>
      sf::st_set_crs(4326L)
  }
  samples
}

st_select <- function(sf, ...) {
  df <- sf::st_set_geometry(sf, NULL)
  df <- dplyr::select(df, ...)
  df
}

st_set_id <- function(sf, prefix = 'sf') {
  sf <- sf |>
    dplyr::mutate(
      id = st_id(seq_len(nrow(sf)), prefix),
      id_key = seq_len(nrow(sf))
    )
  sf
}

st_simplify <- function(sf, keep = 0.02, explode = FALSE) {
  rmapshaper::ms_simplify(
    sf,
    keep = keep,
    keep_shapes = TRUE,
    explode = explode
  )
}

st_validate <- function(sf) {
  df <- sf::st_set_geometry(sf, NULL)
  sf <- sf |>
    geos::as_geos_geometry() |>
    geos::geos_make_valid() |>
    sf::st_as_sf() |>
    cbind(df)
  sf <- sf[sf::st_is_valid(sf), ]
  sf
}

# ptsample <- function(polygons) {
#   internal.polygons <- polygons@polygons[[1]]@Polygons
#   internal.polygons.areas <- sapply(internal.polygons, function(p) p@area)
#   internal.polygons.areas <- cumsum(internal.polygons.areas)
#   internal.polygons.areas <- internal.polygons.areas >= stats::runif(1) * rev(internal.polygons.areas)[1]
#   internal.polygons <- internal.polygons[length(internal.polygons.areas[!internal.polygons.areas]) + 1]
#   ax <- internal.polygons[[1]]@coords[, 1]
#   axr <- range(ax)
#   ay <- internal.polygons[[1]]@coords[, 2]
#   ayr <- range(ay)
#   pt <- list()
#   while (TRUE) {
#     x <- axr[1] + stats::runif(1) * (axr[2] - axr[1])
#     y <- ayr[1] + stats::runif(1) * (ayr[2] - ayr[1])
#     if (sp::point.in.polygons(x, y, ax, ay)) {
#       pt$x <- x
#       pt$y <- y
#       break
#     }
#   }
#   return(pt)
# }
