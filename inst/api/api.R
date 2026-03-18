# api.R
# plumber::pr("inst/api/api.R") |> plumber::pr_run(port=8000)

pools <- list()

#* Server Tiles
#* @param x
#* @param y
#* @param z
#* @param rnd
#* @get /tile
#* @serializer contentType list(type='image/png')
function(user, x, y, z, rnd) {

  cat(sprintf('%s/%s/%s)\n', x, y, z))

  user <- 'default@epicentre.msf.org'

  dbname <- sprintf('../../cache/%s/default/identify.sqlite', user)

  if (is.null(pools[[dbname]])) {
    pool <- pool::dbPool(drv = RSQLite::SQLite(), dbname = dbname, idleTimeout = 3600000L)
    pools[[dbname]] <- pool
  } else {
    pool <- pools[[dbname]]
  }

  m <- 2L^(18L - as.integer(z))

  x_18 <- floor(as.integer(x) / m) * m * m
  y_18 <- floor(as.integer(y) / m) * m * m

  xx <- (x_18):(x_18 + (m - 1L))
  yy <- (y_18):(y_18 + (m - 1L))

  xy <-
    matrix(
      c(
        rep(xx,  each = m),
        rep(yy, times = m)
      ),
      nrow = m * m,
      ncol = 2L
    )

  status <- pool::dbGetQuery(conn = pool, pool::sqlInterpolate(DBI::ANSI(), sprintf('SELECT * FROM tiles WHERE x || "_" || y IN (%s)', paste(sprintf('"%s_%s"', xy[, 1L], xy[, 2L]), collapse = ','))))

  if (nrow(status) == 0L) {

    tile_color <- 'none'

  } else {

    cols <- status$status

    cat(sprintf('%s/%s/%s)\n', x, y, z))

    # img_size <- 256

    # r <- 1:m - 1

    # img <- matrix(rep(rep(cols, each = img_size / m), times = img_size / m), nrow = img_size, ncol = img_size, byrow = TRUE)
    # img <- img[rep(r * m + 1, 4) + rep(r, each = m),]

    col <- max(0L, cols[cols > 0L])

    # if(col==1) browser()

    tile_color <- dplyr::case_when(
      col == 0L ~ 'white',
      col == 1L ~ 'yellow',
      TRUE ~ 'none'
    )

  }

  pool::poolClose(pool)

  # x <- 70406
  # y <- 42987

  # upper_zoom <- 2
  # lower_zoom <- 1

  # lower_zoom_tile_x <- floor(157874 / 2 )
  # lower_zoom_tile_x
  # lower_zoom_tile_y <- floor(132080 / 2 )
  # lower_zoom_tile_y

  # ll <- slippymath::tilenum_to_lonlat(x = 70406, y = 42987, zoom = 17)
  # ll <- slippymath::tilenum_to_lonlat(x = 157897, y = 132096, zoom = 17)

  # bbox <- tile_bbox_ll(x = 70406, y = 42987, zoom = 17)
  # bbox <- c(
  #   xmin = bbox_tl$lon,
  #   ymin = bbox_tl$lat,
  #   xmax = bbox_tl$lon,
  #   ymax = bbox_tl$lat
  # )
  # bbox_to_tile_grid(bbox, zoom = 17)
  # bbox_to_tile_grid(bbox, zoom = 18)

  # slippymath::tile_bbox()

  tile_color_file <- sprintf('../../cache/default@epicentre.msf.org/default/status/%s_%s_%s.png', z, x, y)
  tile_color_file <- sprintf('../../cache/default@epicentre.msf.org/default/status/%s_.png', tile_color)

  if (!fs::file_exists(tile_color_file)) {
    img <- array(0L, dim = c(256L, 256L, 4L))
    img[, , 4L] <- 0.25
    if (tile_color %in% c('white', 'yellow', 'red')) img[, , 1L] <- 1L
    if (tile_color %in% c('white', 'yellow', 'green')) img[, , 2L] <- 1L
    if (tile_color %in% c('white')) img[, , 3L] <- 1L
    if (tile_color %in% c('none')) img[, , 4L] <- 0L
    png::writePNG(img, tile_color_file)
  }

  readBin(tile_color_file, 'raw', n = file.info(tile_color_file)$size)
}
