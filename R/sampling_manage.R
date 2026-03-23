# GPS Point Management Pipeline
#
# Batch splitting, buffer creation, SQLite tile overlays for OsmAnd,
# file export, zipping, and email delivery.

# Task 2: GPS Point Management
# ............................................................................

#' Split sample points into batches
#'
#' Distributes primary or secondary sample points into numbered batches
#' using round-robin assignment. Useful for dividing field work across
#' multiple teams.
#'
#' @param samples_list Named list of communities, output of
#'   [sample_communities()].
#' @param n_batches Integer (applied to all communities) or named integer
#'   vector (per community). E.g., `5L` or `c(kamboma = 5, largo = 3)`.
#' @param set Which point set to split: `"primary"` (default) or
#'   `"secondary"`.
#' @return A named list of `sf` POINT objects, each with an added
#'   `assigned_batch` column (integer, 1 to n_batches).
#' @export
#' @examples
#' \dontrun{
#' batched <- split_batches(samples, n_batches = 5L, set = "primary")
#' }
split_batches <- function(
  samples_list,
  n_batches,
  set = c("primary", "secondary")
) {
  set <- match.arg(set)
  checkmate::assert_list(samples_list, min.len = 1L)

  community_names <- names(samples_list)

  if (length(n_batches) == 1L && is.null(names(n_batches))) {
    n_batches <- rep(as.integer(n_batches), length(community_names))
    names(n_batches) <- community_names
  }

  checkmate::assert_integerish(n_batches, lower = 1L, names = "named")

  result <- list()

  for (nm in community_names) {
    pts <- samples_list[[nm]][[set]]
    checkmate::assert_class(pts, "sf")

    nb <- n_batches[[nm]]

    sort_col <- if (set == "primary" && "selection_order" %in% names(pts)) {
      "selection_order"
    } else {
      "id"
    }

    pts <- pts |>
      dplyr::arrange(.data[[sort_col]])

    pts$assigned_batch <- ((seq_len(nrow(pts)) - 1L) %% nb) + 1L

    result[[nm]] <- pts
  }

  result
}


#' Create circular buffers around points
#'
#' Generates buffer polygons around sampled points using an auto-detected
#' UTM projection for accurate metric distances. Accepts either a single
#' `sf` POINT or a named list of communities from [sample_communities()]
#' or [split_batches()].
#'
#' @param x An `sf` POINT object, or a named list of communities (output
#'   of [sample_communities()] or [split_batches()]).
#' @param radius Buffer radius in meters. Default `50`.
#' @param set Which point set to buffer when `x` is a list: `"primary"`
#'   (default) or `"secondary"`. Ignored when `x` is a plain `sf`.
#' @return If `x` is `sf`: an `sf` POLYGON with `buffer_radius_m` column.
#'   If `x` is a list: a named list of `sf` POLYGON, one per community.
#' @export
#' @examples
#' \dontrun{
#' buffers <- create_buffers(samples, radius = 50, set = "primary")
#' }
create_buffers <- function(x, radius = 50, set = c("primary", "secondary")) {
  set <- match.arg(set)
  checkmate::assert_number(radius, lower = 0)

  if (inherits(x, "sf")) {
    return(buffer_sf(x, radius))
  }

  checkmate::assert_list(x, min.len = 1L)

  result <- list()
  for (nm in names(x)) {
    pts <- if (is.list(x[[nm]]) && set %in% names(x[[nm]])) {
      x[[nm]][[set]]
    } else if (inherits(x[[nm]], "sf")) {
      x[[nm]]
    } else {
      cli::cli_abort(
        "Cannot extract points from {.val {nm}}. Expected sf or list with {.val {set}} element."
      )
    }
    result[[nm]] <- buffer_sf(pts, radius)
  }
  result
}


#' @noRd
buffer_sf <- function(pts, radius) {
  utm_crs <- auto_utm_crs(pts)
  pts_utm <- sf::st_transform(pts, utm_crs)
  buffers_utm <- sf::st_buffer(pts_utm, dist = radius)
  buffers <- sf::st_transform(buffers_utm, 4326L)
  buffers$buffer_radius_m <- radius
  buffers
}


#' Create SQLite tile overlay for OsmAnd
#'
#' Renders buffer polygons into a tiled SQLite database compatible with
#' OsmAnd's overlay/underlay tile format. Each tile is a 256x256 PNG
#' with transparent background.
#'
#' @param buffers_sf An `sf` POLYGON of buffer zones (output of
#'   [create_buffers()]).
#' @param out_file Path for the output `.sqlitedb` file.
#' @param min_zoom Minimum zoom level for tiles. Default `8L`.
#' @param max_zoom Maximum zoom level for tiles. Default `14L`.
#' @param fill_color Fill color in `#RRGGBBAA` format. Default
#'   `"#90EE9066"` (light green, 40% opacity).
#' @param boundary_color Boundary color in `#RRGGBBAA` format. Default
#'   `"#228B22CC"` (forest green, 80% opacity).
#' @return Invisibly, the path to the created `.sqlitedb` file.
#' @export
#' @examples
#' \dontrun{
#' create_buffer_tiles(buffers, "output/buffers.sqlitedb")
#' }
create_buffer_tiles <- function(
  buffers_sf,
  out_file,
  min_zoom = 8L,
  max_zoom = 14L,
  fill_color = "#90EE9066",
  boundary_color = "#228B22CC"
) {
  checkmate::assert_class(buffers_sf, "sf")
  checkmate::assert_path_for_output(out_file, overwrite = TRUE)
  checkmate::assert_int(min_zoom, lower = 1L, upper = 20L)
  checkmate::assert_int(max_zoom, lower = min_zoom, upper = 20L)

  buffers_4326 <- sf::st_transform(buffers_sf, 4326L)
  bbox <- sf::st_bbox(buffers_4326)

  con <- DBI::dbConnect(RSQLite::SQLite(), out_file)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbExecute(
    con,
    paste0(
      "CREATE TABLE IF NOT EXISTS tiles ",
      "(x INT, y INT, z INT, s INT, image BLOB, ",
      "PRIMARY KEY(x, y, z, s))"
    )
  )
  DBI::dbExecute(
    con,
    "CREATE TABLE IF NOT EXISTS info (minzoom INT, maxzoom INT)"
  )
  DBI::dbExecute(
    con,
    "INSERT INTO info VALUES (?, ?)",
    params = list(min_zoom, max_zoom)
  )

  for (z in seq(min_zoom, max_zoom)) {
    tile_grid <- slippymath::bbox_to_tile_grid(bbox, zoom = z)
    tiles <- tile_grid$tiles

    for (j in seq_len(nrow(tiles))) {
      tx <- tiles$x[j]
      ty <- tiles$y[j]
      tile_bb <- tile_bbox_ll(tx, ty, z)
      tile_sfc <- sf::st_as_sfc(sf::st_bbox(
        c(
          xmin = tile_bb[["xmin"]],
          ymin = tile_bb[["ymin"]],
          xmax = tile_bb[["xmax"]],
          ymax = tile_bb[["ymax"]]
        ),
        crs = 4326L
      ))

      hits <- sf::st_intersects(tile_sfc, buffers_4326)[[1L]]
      if (length(hits) == 0L) next

      tile_buffers <- buffers_4326[hits, ]
      img_raw <- render_tile(
        tile_buffers,
        tile_bb,
        fill_color,
        boundary_color
      )

      DBI::dbExecute(
        con,
        "INSERT OR REPLACE INTO tiles (x, y, z, s, image) VALUES (?, ?, ?, 0, ?)",
        params = list(tx, ty, z, list(img_raw))
      )
    }
  }

  invisible(out_file)
}


#' Render a single map tile as PNG bytes
#' @noRd
render_tile <- function(buffers_sf, tile_bb, fill_color, boundary_color) {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  grDevices::png(
    tmp,
    width = 256L,
    height = 256L,
    bg = "transparent",
    type = "cairo"
  )
  graphics::par(mar = c(0, 0, 0, 0))
  graphics::plot.new()
  graphics::plot.window(
    xlim = c(tile_bb[["xmin"]], tile_bb[["xmax"]]),
    ylim = c(tile_bb[["ymin"]], tile_bb[["ymax"]])
  )
  graphics::plot(
    sf::st_geometry(buffers_sf),
    col = fill_color,
    border = boundary_color,
    lwd = 1,
    add = TRUE
  )
  grDevices::dev.off()

  readBin(tmp, "raw", file.info(tmp)$size)
}


#' Export sample points, buffers, and tile overlays
#'
#' Saves points in multiple formats, optionally generates buffer polygons
#' and OsmAnd-compatible SQLite tile overlays. Each community gets a
#' self-contained folder.
#'
#' @param samples_list Output of [split_batches()] (preferred) or
#'   [sample_communities()].
#' @param out_dir Root output directory.
#' @param formats Character vector of export formats: `"gpkg"`, `"gpx"`,
#'   `"shp"`, `"kml"`. Default `c("gpkg", "gpx")`.
#' @param include_buffers Whether to generate and export buffer polygons
#'   and SQLite tile overlays. Default `TRUE`.
#' @param buffer_radius Buffer radius in meters. Default `50`.
#' @param set Which point set to export: `"primary"` (default) or
#'   `"secondary"`.
#' @return Invisibly, a tibble of exported file paths with columns:
#'   `community`, `set`, `batch`, `type`, `format`, `path`.
#' @export
#' @examples
#' \dontrun{
#' export_points(batched, "output", set = "primary")
#' }
export_points <- function(
  samples_list,
  out_dir,
  formats = c("gpkg", "gpx"),
  include_buffers = TRUE,
  buffer_radius = 50,
  set = c("primary", "secondary")
) {
  set <- match.arg(set)
  checkmate::assert_list(samples_list, min.len = 1L)
  checkmate::assert_character(formats, min.len = 1L)
  checkmate::assert_flag(include_buffers)
  checkmate::assert_number(buffer_radius, lower = 0)

  manifest <- tibble::tibble(
    community = character(),
    set = character(),
    batch = character(),
    type = character(),
    format = character(),
    path = character()
  )

  for (nm in names(samples_list)) {
    pts <- if (inherits(samples_list[[nm]], "sf")) {
      samples_list[[nm]]
    } else {
      samples_list[[nm]][[set]]
    }

    community_dir <- fs::path(out_dir, set, nm)
    fs::dir_create(community_dir, recurse = TRUE)

    # Write all points
    for (fmt in formats) {
      fname <- glue::glue("{nm}_{set}_all.{fmt}")
      fpath <- fs::path(community_dir, fname)
      write_spatial(pts, fpath, fmt)
      manifest <- tibble::add_row(
        manifest,
        community = nm,
        set = set,
        batch = "all",
        type = "points",
        format = fmt,
        path = as.character(fpath)
      )
    }

    # Write per-batch points
    if ("assigned_batch" %in% names(pts)) {
      batches <- sort(unique(pts$assigned_batch))
      for (b in batches) {
        batch_pts <- pts[pts$assigned_batch == b, ]
        for (fmt in formats) {
          fname <- glue::glue("{nm}_{set}_batch_{b}.{fmt}")
          fpath <- fs::path(community_dir, fname)
          write_spatial(batch_pts, fpath, fmt)
          manifest <- tibble::add_row(
            manifest,
            community = nm,
            set = set,
            batch = as.character(b),
            type = "points",
            format = fmt,
            path = as.character(fpath)
          )
        }
      }
    }

    # Buffers + SQLite tiles
    if (include_buffers) {
      buffers <- buffer_sf(pts, buffer_radius)

      for (fmt in c("gpkg", "gpx")) {
        fname <- glue::glue("{nm}_buffers_all.{fmt}")
        fpath <- fs::path(community_dir, fname)
        write_spatial(buffers, fpath, fmt)
        manifest <- tibble::add_row(
          manifest,
          community = nm,
          set = set,
          batch = "all",
          type = "buffers",
          format = fmt,
          path = as.character(fpath)
        )
      }

      # SQLite tiles for all buffers
      tiles_path <- fs::path(
        community_dir,
        glue::glue("{nm}_buffers_all.sqlitedb")
      )
      create_buffer_tiles(buffers, as.character(tiles_path))
      manifest <- tibble::add_row(
        manifest,
        community = nm,
        set = set,
        batch = "all",
        type = "tiles",
        format = "sqlitedb",
        path = as.character(tiles_path)
      )

      # Per-batch buffers + tiles
      if ("assigned_batch" %in% names(pts)) {
        for (b in batches) {
          batch_buffers <- buffers[buffers$assigned_batch == b, ]

          for (fmt in c("gpkg", "gpx")) {
            fname <- glue::glue("{nm}_buffers_batch_{b}.{fmt}")
            fpath <- fs::path(community_dir, fname)
            write_spatial(batch_buffers, fpath, fmt)
            manifest <- tibble::add_row(
              manifest,
              community = nm,
              set = set,
              batch = as.character(b),
              type = "buffers",
              format = fmt,
              path = as.character(fpath)
            )
          }

          btiles_path <- fs::path(
            community_dir,
            glue::glue("{nm}_buffers_batch_{b}.sqlitedb")
          )
          create_buffer_tiles(batch_buffers, as.character(btiles_path))
          manifest <- tibble::add_row(
            manifest,
            community = nm,
            set = set,
            batch = as.character(b),
            type = "tiles",
            format = "sqlitedb",
            path = as.character(btiles_path)
          )
        }
      }
    }
  }

  cli::cli_inform(
    "Exported {nrow(manifest)} files to {.path {out_dir}}"
  )
  invisible(manifest)
}


#' Write spatial data to various formats
#' @noRd
write_spatial <- function(sf_obj, path, fmt) {
  sf::st_agr(sf_obj) <- "constant"
  switch(
    fmt,
    gpkg = sf::st_write(
      sf_obj,
      path,
      driver = "GPKG",
      quiet = TRUE,
      delete_dsn = TRUE
    ),
    gpx = write_gpx(sf_obj, path),
    shp = sf::st_write(
      sf_obj,
      path,
      driver = "ESRI Shapefile",
      quiet = TRUE,
      delete_dsn = TRUE
    ),
    kml = sf::st_write(
      sf_obj,
      path,
      driver = "KML",
      quiet = TRUE,
      delete_dsn = TRUE
    ),
    cli::cli_abort("Unsupported format: {.val {fmt}}")
  )
  invisible(path)
}


#' Write sf to GPX format
#'
#' GPX supports waypoints (points) and tracks (lines). Polygons are
#' converted to track boundaries (linestrings).
#' @noRd
write_gpx <- function(sf_obj, path) {
  geom_type <- unique(as.character(sf::st_geometry_type(sf_obj)))

  if (all(geom_type %in% c("POINT", "MULTIPOINT"))) {
    gpx_obj <- sf_obj |>
      dplyr::select(dplyr::any_of(c("id", "community", "assigned_batch")))
    if ("id" %in% names(gpx_obj)) {
      gpx_obj <- gpx_obj |> dplyr::rename(name = "id")
    }
    sf::st_write(
      gpx_obj,
      path,
      driver = "GPX",
      dataset_options = "GPX_USE_EXTENSIONS=YES",
      layer = "waypoints",
      quiet = TRUE,
      delete_dsn = TRUE
    )
  } else {
    lines <- sf::st_cast(
      sf::st_cast(sf_obj, "MULTILINESTRING"),
      "LINESTRING"
    )
    gpx_lines <- lines |>
      dplyr::select(dplyr::any_of(c("id", "community", "assigned_batch")))
    if ("id" %in% names(gpx_lines)) {
      gpx_lines <- gpx_lines |> dplyr::rename(name = "id")
    }
    sf::st_write(
      gpx_lines,
      path,
      driver = "GPX",
      dataset_options = "GPX_USE_EXTENSIONS=YES",
      layer = "tracks",
      quiet = TRUE,
      delete_dsn = TRUE
    )
  }

  invisible(path)
}


#' Create zip archives for field distribution
#'
#' Bundles GPX files and SQLite tile overlays into zip archives ready
#' for copying to field devices.
#'
#' @param export_dir Root export directory (output of [export_points()]).
#' @param out_dir Where to write zip files. Defaults to `export_dir`.
#' @param sets Character vector of sets to zip: `"primary"`,
#'   `"secondary"`, or both.
#' @param prefix Optional prefix for zip filenames (e.g., project name).
#' @return Invisibly, character vector of created zip file paths.
#' @export
#' @examples
#' \dontrun{
#' zips <- zip_points("output", prefix = "kgh-")
#' }
zip_points <- function(
  export_dir,
  out_dir = export_dir,
  sets = c("primary", "secondary"),
  prefix = ""
) {
  checkmate::assert_directory_exists(export_dir)
  checkmate::assert_character(sets, min.len = 1L)
  checkmate::assert_string(prefix)

  fs::dir_create(out_dir, recurse = TRUE)
  zip_paths <- character()

  for (s in sets) {
    set_dir <- fs::path(export_dir, s)
    if (!fs::dir_exists(set_dir)) next

    gpx_files <- fs::dir_ls(
      set_dir,
      recurse = TRUE,
      glob = "*.gpx"
    )
    sqlite_files <- fs::dir_ls(
      set_dir,
      recurse = TRUE,
      glob = "*.sqlitedb"
    )
    all_files <- c(gpx_files, sqlite_files)

    if (length(all_files) == 0L) next

    zipname <- glue::glue("{prefix}{s}-points.zip")
    zipfile <- fs::path(out_dir, zipname)

    withr::with_dir(as.character(set_dir), {
      rel_files <- fs::path_rel(all_files, set_dir)
      utils::zip(
        as.character(zipfile),
        files = as.character(rel_files),
        flags = "-j"
      )
    })

    zip_paths <- c(zip_paths, as.character(zipfile))
    cli::cli_inform("Created {.path {zipfile}}")
  }

  invisible(zip_paths)
}


#' Send exported zip files via email
#'
#' Sends zip archives as email attachments using the `emayili` package.
#' Requires `emayili` to be installed (in Suggests).
#'
#' @param zip_files Character vector of zip file paths to attach.
#' @param to Email recipient(s) (character vector).
#' @param from Sender email. Default from env var `EMAIL_FROM`.
#' @param subject Email subject line.
#' @param body Optional custom body text.
#' @param host SMTP server hostname. Default: Office 365.
#' @param port SMTP port. Default: 587 (STARTTLS).
#' @param username SMTP username. Default from env var `EMAIL_USER`.
#' @param password SMTP password. Default from env var `EMAIL_PASSWORD`.
#' @return Invisibly, the result of the SMTP send.
#' @export
#' @examples
#' \dontrun{
#' email_points(
#'   c("primary-points.zip", "secondary-points.zip"),
#'   to = "fieldteam@example.org"
#' )
#' }
email_points <- function(
  zip_files,
  to,
  from = Sys.getenv("EMAIL_FROM"),
  subject = "GPS Sampling Points",
  body = NULL,
  host = "smtp.office365.com",
  port = 587L,
  username = Sys.getenv("EMAIL_USER"),
  password = Sys.getenv("EMAIL_PASSWORD")
) {
  rlang::check_installed("emayili", reason = "to send emails")
  checkmate::assert_file_exists(zip_files)
  checkmate::assert_character(to, min.len = 1L)
  checkmate::assert_string(from, min.chars = 1L)

  if (is.null(body)) {
    body <- glue::glue(
      "GPS sampling points attached.\n\n",
      "Files: {paste(basename(zip_files), collapse = ', ')}\n",
      "Generated: {Sys.Date()}"
    )
  }

  email <- emayili::envelope(from = from, to = to, subject = subject) |>
    emayili::text(body)

  for (zf in zip_files) {
    email <- email |> emayili::attachment(zf)
  }

  smtp <- emayili::server(
    host = host,
    port = port,
    username = username,
    password = password
  )

  result <- smtp(email)
  cli::cli_inform("Email sent to {.val {to}}")
  invisible(result)
}
