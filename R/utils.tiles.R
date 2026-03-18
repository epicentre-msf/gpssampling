# styler: block

#' Add Tile Numbers to Spatial Features
#'
#' This function adds tile numbers to a spatial features object based on its centroid coordinates.
#'
#' @param x A spatial features object.
#' @param mercator A logical value indicating whether the coordinates are in Mercator projection. Default is TRUE.
#' @param zoom An integer indicating the zoom level for the tile numbers. Default is 18.
#'
#' @return A spatial features object with added tile numbers.
#'
#' @examples
#' nc <- sf::st_read(system.file('shape/nc.shp', package = 'sf'))
#' add_tilenum(nc)
#'
#' @keywords internal
#'
add_tilenum <- function(x, mercator = TRUE, zoom = 18L) {
  checkmate::assert_class(x, classes = 'sf')
  checkmate::assert_logical(mercator, len = 1L)
  checkmate::assert_int(zoom, lower = 1L, upper = 20L)

  # Create a copy of the input object
  x_copy <- sf::st_as_sf(x)

  # Calculate centroid coordinates
  x_cc <- x_copy |>
    sf::st_set_agr('constant') |>
    sf::st_centroid() |>
    sf::st_coordinates() |>
    as.matrix()

  # Convert coordinates to lon-lat if mercator is TRUE
  if (mercator) {
    x_cc <- slippymath::merc_to_lonlat(x_cc)
  }

  # Calculate tile numbers
  tn <-
    slippymath::lonlat_to_tilenum(
      lon_deg = x_cc[, 1L],
      lat_deg = x_cc[, 2L],
      zoom = zoom
    )

  # Create new "sf" object with added columns
  x_copy$x <- as.integer(tn$x)
  x_copy$y <- as.integer(tn$y)

  x_copy
}

is_tile_grid <- function(obj) {
  inherits(obj, 'tile_grid')
}

tile_bbox_ll <- function(x, y, zoom) {
  bottom_left <- slippymath::tilenum_to_lonlat(x, y + 1L, zoom)
  top_right <- slippymath::tilenum_to_lonlat(x + 1L, y, zoom)
  structure(
    c(
      xmin = bottom_left[[1L]],
      ymin = bottom_left[[2L]],
      xmax = top_right[[1L]],
      ymax = top_right[[2L]]
    ),
    class = 'bbox'
  )
}

tile_grid_bbox <- function(tile_grid, zoom = 18L) {
  tiles <- tile_grid$tiles

  tile_top_left <- tiles[1L, ]
  tile_top_left <- slippymath::tilenum_to_lonlat(x = tile_top_left$x, y = tile_top_left$y, zoom = zoom)

  tile_bottom_right <- tiles[nrow(tiles), ]
  tile_bottom_right <- slippymath::tilenum_to_lonlat(x = tile_bottom_right$x + 1L, y = tile_bottom_right$y + 1L, zoom = zoom)

  structure(
    c(
      xmin = tile_top_left$lon,
      ymin = tile_bottom_right$lat,
      xmax = tile_bottom_right$lon,
      ymax = tile_top_left$lat
    ),
    class = 'bbox'
  )
}

tile_grid_bboxes_ll <- function(tile_grid) {
  if (!is_tile_grid(tile_grid)) {
    stop('tile_grid must be of class tile_grid - output from bbox_to_tile_grid()')
  }

  tiles <- tibble::as_tibble(tile_grid$tiles)
  tiles_zoom <- tile_grid$zoom

  tiles <- dplyr::mutate(tiles,
    xmin = tilenum_to_lon(x, y + 1L, tiles_zoom),
    ymin = tilenum_to_lat(x, y + 1L, tiles_zoom),
    xmax = tilenum_to_lon(x + 1L, y, tiles_zoom),
    ymax = tilenum_to_lat(x + 1L, y, tiles_zoom)
    # pt = sprintf("POLYGON((0 0 , 0 1 , 1 1 , 1 0, 0 0))"),
    # pt = sf::st_as_sfc(pt)
  )

  pts <- sf::st_as_sf(tiles,
    coords = c('xmin', 'ymin'),
    agr = 'constant',
    crs = 4326L, # nad83 / new york long island projection
    stringsAsFactors = FALSE,
    remove = TRUE
  )

  c(
    xmin, ymin,
    xmax, ymin,
    xmax, ymax,
    xmin, ymax,
    xmin, ymin
  )

  tiles <- tiles |>
    dplyr::rowwise() |>
    dplyr::mutate(
      pol = list(sf::st_polygon(list(matrix(c(
        xmin, ymin,
        xmax, ymin,
        xmax, ymax,
        xmin, ymax,
        xmin, ymin
      ), ncol = 2L, byrow = TRUE))))
    )

  m <- tiles[
    1:2,
    c(
      'xmin', 'ymin',
      'xmax', 'ymin',
      'xmax', 'ymax',
      'xmin', 'ymax',
      'xmin', 'ymin'
    )
  ]

  tiles <- tiles |>
    dplyr::rowwise() |>
    dplyr::mutate(
      pol = list(sf::st_polygon(list(matrix(c(
        xmin, ymin,
        xmax, ymin,
        xmax, ymax,
        xmin, ymax,
        xmin, ymin
      ), ncol = 2L, byrow = TRUE))))
    )


  purrr::pmap(.l = tile_grid$tiles, .f = tile_bbox_ll, zoom = tiles_zoom)
}

tile_grid_to_sf <- function(tile_grid) {
  if (!is_tile_grid(tile_grid)) {
    stop('tile_grid must be of class tile_grid - output from bbox_to_tile_grid()')
  }

  tiles <- tibble::as_tibble(tile_grid$tiles)
  tiles_zoom <- tile_grid$zoom

  tiles <- dplyr::mutate(tiles,
    xmin = tilenum_to_lon(x, y + 1L, tiles_zoom),
    ymin = tilenum_to_lat(x, y + 1L, tiles_zoom),
    xmax = tilenum_to_lon(x + 1L, y, tiles_zoom),
    ymax = tilenum_to_lat(x + 1L, y, tiles_zoom),
    pt = sprintf(
      'POLYGON((%s %s,%s %s,%s %s,%s %s,%s %s))',
      xmin, ymin,
      xmax, ymin,
      xmax, ymax,
      xmin, ymax,
      xmin, ymin
    ),
    pt = sf::st_as_sfc(pt)
  )

  sf <- sf::st_as_sf(tiles, agr = 'constant', crs = 4326L, stringsAsFactors = FALSE, remove = TRUE)

  sf
}

tile_polygon <- function(x, y, zoom) {
  bbox <- tile_bbox_ll(x, y, zoom)
}

tile_to_sf <- function(x, y, zoom, divide = 1L) {
  tile_bbox <- tile_bbox_ll(x, y, zoom)

  if (divide > 1L) {
    xmin0 <- as.double(tile_bbox$xmin)
    ymin0 <- as.double(tile_bbox$ymin)
    w <- as.double((tile_bbox$xmax - tile_bbox$xmin) / divide)
    h <- as.double((tile_bbox$ymax - tile_bbox$ymin) / divide)

    tile_sf <- sf::st_sfc()

    for (c in 0:(divide - 1L)) {
      for (r in 0:(divide - 1L)) {
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
        tile_sf <- rbind(tile_sf, cell_sf)
      }
    }
  } else {
    tile_sf <- st_bbox_polygon(bbox = tile_bbox)
  }

  tile_sf
}

tilenum_to_lat <- function(x, y, zoom) {
  n_tiles <- 2L^zoom
  merc_lat <- (1L - ((y / n_tiles) * 2L)) * pi
  lat_rad <- atan(sinh(merc_lat))
  lat <- degrees(lat_rad)
  lat
}

tilenum_to_lon <- function(x, y, zoom) {
  n_tiles <- 2L^zoom
  lon_rad <- (((x / n_tiles) * 2L) - 1L) * pi
  lon <- degrees(lon_rad)
  lon
}
