# GPS Point Management Pipeline
#
# Batch splitting, buffer creation, SQLite tile overlays for OsmAnd,
# file export, zipping, and email delivery.

#' Split sample points into batches
#'
#' Distributes primary or secondary sample points into numbered batches
#' using round-robin assignment. Useful for dividing field work across
#' multiple teams.
#'
#' @param samples_list Named list of communities, output of
#'   [sample_communities()].
#' @param n_batches Integer (applied to all communities) or named integer
#'   vector (per community). E.g., `5L` or `c(community_one = 5, community_two = 3)`.
#' @param set Which point set to split: `"primary"` (default) or
#'   `"secondary"`.
#' @return A named list of communities. Each element is a list with:
#'   \describe{
#'     \item{`batches`}{`sf` POINT with `assigned_batch` column.}
#'     \item{`min_distance`}{Buffer radius in meters.}
#'     \item{`n_buildings`}{Total building count in the community.}
#'     \item{`seed`}{Per-community RNG seed.}
#'     \item{`n_batches`}{Number of batches.}
#'     \item{`buildings`}{`sf` of all candidate buildings.}
#'   }
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

  cli::cli_inform(
    "Splitting {set} points into batches for {length(community_names)} communit{?y/ies}..."
  )

  result <- list()

  for (nm in community_names) {
    pts <- samples_list[[nm]][[set]]
    checkmate::assert_class(pts, "sf")

    nb <- n_batches[[nm]]

    sort_col <- if ("selection_order" %in% names(pts)) {
      "selection_order"
    } else if ("point_id" %in% names(pts)) {
      "point_id"
    } else {
      "id"
    }

    pts <- pts |>
      dplyr::arrange(.data[[sort_col]])

    pts$assigned_batch <- ((seq_len(nrow(pts)) - 1L) %% nb) + 1L

    result[[nm]] <- list(
      batches = pts,
      min_distance = samples_list[[nm]]$min_distance,
      n_buildings = nrow(samples_list[[nm]]$buildings),
      seed = samples_list[[nm]]$seed,
      n_batches = nb,
      buildings = samples_list[[nm]]$buildings
    )
  }

  result
}


#' Extract metadata from batched sampling results
#'
#' Combines primary and (optionally) secondary batched results into a
#' single data frame with per-point metadata. Useful for creating
#' field assignment sheets.
#'
#' @param primary Output of [split_batches()] for the primary set.
#' @param secondary Output of [split_batches()] for the secondary
#'   set. Default `NULL` (primary only).
#' @return A `data.frame` with columns: `community`, `point_id`,
#'   `named_point_id` (when available), `assigned_batch`, `set`.
#'   Carries two attributes: `buffer_size` (named numeric vector of
#'   per-community buffer radii) and `n_teams` (named integer vector
#'   of per-community batch counts).
#' @export
#' @examples
#' \dontrun{
#' meta <- extract_metadata(primary_batches, secondary_batches)
#' attr(meta, "buffer_size")
#' attr(meta, "n_teams")
#' }
extract_metadata <- function(primary, secondary = NULL) {
  checkmate::assert_list(primary, min.len = 1L)

  extract_set <- function(batched, set_name) {
    rows <- list()
    for (nm in names(batched)) {
      pts <- extract_points(batched[[nm]])
      if (is.null(pts)) {
        cli::cli_abort(
          "Cannot extract points from {.val {nm}}."
        )
      }
      row <- data.frame(
        community = nm,
        point_id = pts$point_id,
        assigned_batch = pts$assigned_batch,
        set = set_name,
        stringsAsFactors = FALSE
      )
      if ("named_point_id" %in% names(pts)) {
        row$named_point_id <- pts$named_point_id
      }
      rows <- c(rows, list(row))
    }
    do.call(rbind, rows)
  }

  result <- extract_set(primary, "primary")
  if (!is.null(secondary)) {
    checkmate::assert_list(secondary, min.len = 1L)
    sec_df <- extract_set(secondary, "secondary")
    result <- rbind(result, sec_df)
  }

  # Build metadata attributes
  buffer_sizes <- vapply(
    primary,
    function(entry) {
      if (is.list(entry) && "min_distance" %in% names(entry)) {
        entry[["min_distance"]]
      } else {
        NA_real_
      }
    },
    numeric(1L)
  )

  n_teams <- vapply(
    primary,
    function(entry) {
      if (is.list(entry) && "n_batches" %in% names(entry)) {
        as.integer(entry[["n_batches"]])
      } else {
        NA_integer_
      }
    },
    integer(1L)
  )

  attr(result, "buffer_size") <- buffer_sizes
  attr(result, "n_teams") <- n_teams

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
    cli::cli_inform(
      "Creating {radius}m buffers for {nrow(x)} point{?s}..."
    )
    return(buffer_sf(x, radius))
  }

  checkmate::assert_list(x, min.len = 1L)
  cli::cli_inform(
    "Creating {radius}m buffers for {length(x)} communit{?y/ies}..."
  )

  result <- list()
  for (nm in names(x)) {
    pts <- extract_points(x[[nm]], set)
    if (is.null(pts)) {
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


#' Extract sf points from various input shapes
#'
#' Handles enriched `split_batches()` output (list with `$batches`),
#' bare `sf`, or `sample_communities()` output (list with `$primary`/
#' `$secondary`).
#'
#' @param entry A single community element from any of the above.
#' @param set Which set to extract when `entry` is a
#'   `sample_communities()` element: `"primary"` or `"secondary"`.
#' @return An `sf` POINT object, or `NULL` if extraction fails.
#' @noRd
extract_points <- function(entry, set = NULL) {
  if (is.list(entry) && "batches" %in% names(entry)) {
    return(entry[["batches"]])
  }
  if (inherits(entry, "sf")) {
    return(entry)
  }
  if (is.list(entry) && !is.null(set) && set %in% names(entry)) {
    return(entry[[set]])
  }
  NULL
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
  buffers_3857 <- sf::st_transform(buffers_sf, 3857L)
  bbox <- sf::st_bbox(buffers_4326)

  cli::cli_inform(
    "Rendering buffer tiles ({nrow(buffers_sf)} buffer{?s}, zoom {min_zoom}-{max_zoom})..."
  )

  if (file.exists(out_file)) {
    file.remove(out_file)
  }

  con <- DBI::dbConnect(RSQLite::SQLite(), out_file)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbExecute(
    con,
    paste0(
      "CREATE TABLE tiles ",
      "(x INT, y INT, z INT, s INT, image BLOB, ",
      "PRIMARY KEY(x, y, z, s))"
    )
  )
  DBI::dbExecute(
    con,
    paste0(
      "CREATE TABLE info (",
      "tilenumbering TEXT, ",
      "minzoom INTEGER, ",
      "maxzoom INTEGER, ",
      "url TEXT DEFAULT '', ",
      "ellipsoid INTEGER DEFAULT 0, ",
      "inverted_y INTEGER DEFAULT 0, ",
      "timecolumn TEXT DEFAULT 'no', ",
      "expireminutes INTEGER DEFAULT -1",
      ")"
    )
  )
  DBI::dbExecute(
    con,
    paste0(
      "INSERT INTO info ",
      "(tilenumbering, minzoom, maxzoom, url, ellipsoid, inverted_y, ",
      "timecolumn, expireminutes) ",
      "VALUES ('simple', ?, ?, '', 0, 0, 'no', -1)"
    ),
    params = list(min_zoom, max_zoom)
  )

  merc_origin <- 20037508.342789244

  for (z in seq(min_zoom, max_zoom)) {
    tile_grid <- suppressWarnings(slippymath::bbox_to_tile_grid(bbox, zoom = z))
    tiles <- tile_grid$tiles
    cli::cli_inform(
      "  Zoom {z}: {nrow(tiles)} tile{?s}..."
    )

    n_tiles <- 2L^z
    tile_size <- 2 * merc_origin / n_tiles

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
      if (length(hits) == 0L) {
        next
      }

      merc_bb <- c(
        xmin = -merc_origin + tx * tile_size,
        xmax = -merc_origin + (tx + 1L) * tile_size,
        ymax = merc_origin - ty * tile_size,
        ymin = merc_origin - (ty + 1L) * tile_size
      )

      tile_buffers <- buffers_3857[hits, ]
      img_raw <- render_tile(
        tile_buffers,
        merc_bb,
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
#'
#' @param buffers_sf An `sf` object in EPSG:3857 (Web Mercator).
#' @param merc_bb Named numeric vector with xmin, xmax, ymin, ymax in Mercator
#'   metres.
#' @noRd
render_tile <- function(buffers_sf, merc_bb, fill_color, boundary_color) {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  grDevices::png(
    tmp,
    width = 256L,
    height = 256L,
    bg = "transparent",
    type = "cairo"
  )
  graphics::par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
  graphics::plot.new()
  graphics::plot.window(
    xlim = c(merc_bb[["xmin"]], merc_bb[["xmax"]]),
    ylim = c(merc_bb[["ymin"]], merc_bb[["ymax"]])
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
#' self-contained folder. Buffer radius is derived from the per-community
#' `$min_distance` in the enriched [split_batches()] output.
#'
#' @param samples_list Output of [split_batches()] (preferred) or
#'   [sample_communities()].
#' @param out_dir Root output directory.
#' @param formats Character vector of export formats: `"gpkg"`, `"gpx"`,
#'   `"shp"`, `"kml"`. Default `c("gpkg", "gpx")`.
#' @param include_buffers Whether to generate and export buffer polygons
#'   and SQLite tile overlays. Default `TRUE`.
#' @param set Which point set to export: `"primary"` (default) or
#'   `"secondary"`.
#' @param print_table Logical. If `TRUE`, computes buffer-level
#'   statistics (buildings per buffer) and attaches a
#'   [flextable::flextable()] as `attr(, "summary_table")` and the
#'   underlying data frame as `attr(, "summary_df")`. Default `FALSE`.
#' @return Invisibly, a tibble of exported file paths with columns:
#'   `community`, `set`, `batch`, `type`, `format`, `path`. When
#'   `print_table = TRUE`, carries `summary_table`, `summary_df`, and
#'   `buffer_details` attributes. `buffer_details` is a data frame with
#'   per-buffer building counts (`community`, `buffer_idx`,
#'   `n_buildings`, `buffer_radius_m`) suitable for
#'   [plot_buffer_distribution()].
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
  set = c("primary", "secondary"),
  print_table = FALSE
) {
  set <- match.arg(set)
  checkmate::assert_list(samples_list, min.len = 1L)
  checkmate::assert_character(formats, min.len = 1L)
  checkmate::assert_flag(include_buffers)
  checkmate::assert_flag(print_table)

  n_communities <- length(samples_list)
  cli::cli_inform(
    "Exporting {set} points for {n_communities} communit{?y/ies} to {.path {out_dir}}..."
  )

  manifest <- tibble::tibble(
    community = character(),
    set = character(),
    batch = character(),
    type = character(),
    format = character(),
    path = character()
  )

  # Collect per-community info for summary
  summary_rows <- list()
  detail_rows <- list()

  for (nm in names(samples_list)) {
    entry <- samples_list[[nm]]
    pts <- extract_points(entry, set)
    if (is.null(pts)) {
      cli::cli_abort(
        "Cannot extract points from {.val {nm}}."
      )
    }

    # Resolve buffer radius from metadata
    buf_radius <- if (is.list(entry) && "min_distance" %in% names(entry)) {
      entry[["min_distance"]]
    } else if (is.list(entry) && !inherits(entry, "sf")) {
      entry[["min_distance"]]
    } else {
      NULL
    }

    community_dir <- fs::path(out_dir, set, nm)
    fs::dir_create(community_dir, recurse = TRUE)
    cli::cli_inform("  {.val {nm}}: {nrow(pts)} point{?s}...")

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
      if (is.null(buf_radius)) {
        cli::cli_abort(
          "Cannot determine buffer radius for {.val {nm}}. Use enriched {.fn split_batches} output."
        )
      }
      buf_int <- as.integer(buf_radius)
      buffers <- buffer_sf(pts, buf_radius)

      for (fmt in c("gpkg", "gpx")) {
        fname <- glue::glue("{nm}_buffers_{buf_int}m_all.{fmt}")
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
        glue::glue("{nm}_buffers_{buf_int}m_all.sqlitedb")
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
            fname <- glue::glue(
              "{nm}_buffers_{buf_int}m_batch_{b}.{fmt}"
            )
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
            glue::glue("{nm}_buffers_{buf_int}m_batch_{b}.sqlitedb")
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

      # Collect stats for summary table
      if (print_table) {
        buildings_sf <- if (is.list(entry) && "buildings" %in% names(entry)) {
          entry[["buildings"]]
        } else {
          NULL
        }
        if (!is.null(buildings_sf) && nrow(buildings_sf) > 0L) {
          hits <- sf::st_intersects(buffers, buildings_sf)
          bldgs_per_buf <- lengths(hits)
          summary_rows <- c(
            summary_rows,
            list(data.frame(
              community = nm,
              n_points = nrow(pts),
              buffer_radius_m = buf_radius,
              n_buildings = nrow(buildings_sf),
              avg_bldgs_per_buffer = round(mean(bldgs_per_buf), 1),
              min_bldgs_per_buffer = min(bldgs_per_buf),
              max_bldgs_per_buffer = max(bldgs_per_buf),
              median_bldgs_per_buffer = round(
                stats::median(bldgs_per_buf),
                1
              ),
              stringsAsFactors = FALSE
            ))
          )
          # Raw per-buffer counts for distribution plotting
          detail_rows <- c(
            detail_rows,
            list(data.frame(
              community = nm,
              buffer_idx = seq_along(bldgs_per_buf),
              n_buildings = bldgs_per_buf,
              buffer_radius_m = buf_radius,
              stringsAsFactors = FALSE
            ))
          )
        }
      }
    }
  }

  cli::cli_inform(
    "Exported {nrow(manifest)} files to {.path {out_dir}}"
  )

  # --- Summary table ---
  if (print_table && length(summary_rows) > 0L) {
    summary_df <- do.call(rbind, summary_rows)

    ft <- flextable::flextable(summary_df) |>
      flextable::set_header_labels(
        community = "Community",
        n_points = "Points",
        buffer_radius_m = "Buffer\nRadius (m)",
        n_buildings = "Buildings",
        avg_bldgs_per_buffer = "Avg Bldgs\n/ Buffer",
        min_bldgs_per_buffer = "Min Bldgs\n/ Buffer",
        max_bldgs_per_buffer = "Max Bldgs\n/ Buffer",
        median_bldgs_per_buffer = "Median Bldgs\n/ Buffer"
      ) |>
      flextable::autofit() |>
      flextable::set_caption("Export Summary: Buffer Statistics")

    attr(manifest, "summary_table") <- ft
    attr(manifest, "summary_df") <- summary_df
    if (length(detail_rows) > 0L) {
      attr(manifest, "buffer_details") <- do.call(rbind, detail_rows)
    }
  }

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
      dplyr::select(
        dplyr::any_of(
          c("named_point_id", "point_id", "id", "community", "assigned_batch")
        )
      )
    # Prefer named_point_id > point_id > id for GPX name
    if ("named_point_id" %in% names(gpx_obj)) {
      gpx_obj$name <- gpx_obj$named_point_id
      gpx_obj <- gpx_obj |>
        dplyr::select(
          -dplyr::any_of(c("named_point_id", "point_id", "id"))
        )
    } else if ("point_id" %in% names(gpx_obj)) {
      gpx_obj$name <- as.character(gpx_obj$point_id)
      gpx_obj <- gpx_obj |>
        dplyr::select(-dplyr::any_of(c("point_id", "id")))
    } else if ("id" %in% names(gpx_obj)) {
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
    lines <- suppressWarnings(sf::st_cast(
      sf::st_cast(sf_obj, "MULTILINESTRING"),
      "LINESTRING"
    ))
    gpx_lines <- lines |>
      dplyr::select(
        dplyr::any_of(
          c("named_point_id", "point_id", "id", "community", "assigned_batch")
        )
      )
    if ("named_point_id" %in% names(gpx_lines)) {
      gpx_lines$name <- gpx_lines$named_point_id
      gpx_lines <- gpx_lines |>
        dplyr::select(
          -dplyr::any_of(c("named_point_id", "point_id", "id"))
        )
    } else if ("point_id" %in% names(gpx_lines)) {
      gpx_lines$name <- as.character(gpx_lines$point_id)
      gpx_lines <- gpx_lines |>
        dplyr::select(-dplyr::any_of(c("point_id", "id")))
    } else if ("id" %in% names(gpx_lines)) {
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
    if (!fs::dir_exists(set_dir)) {
      next
    }

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

    if (length(all_files) == 0L) {
      next
    }

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


#' Create a Google Earth (KML) project
#'
#' Generates a KML file with organized folders for primary points,
#' secondary points, and buffer zones across all communities. Each set
#' uses distinct colors, and every placemark is labeled with its
#' `point_id`. The resulting file can be opened directly in Google
#' Earth.
#'
#' @param samples_list Output of [sample_communities()].
#' @param out_file Path for the output `.kml` file.
#' @param buffer_radius Buffer radius in meters. Default `NULL`
#'   (derives from per-community `$min_distance`). Pass an explicit
#'   value to override.
#' @param primary_color Point color for primary set (`#RRGGBB` or
#'   `#RRGGBBAA`). Default `"#FF4500"` (orange-red).
#' @param secondary_color Point color for secondary set. Default
#'   `"#1E90FF"` (dodger blue).
#' @param primary_buffer_color Buffer fill for primary set. Default
#'   `"#FF450044"` (orange-red, 27% opacity).
#' @param secondary_buffer_color Buffer fill for secondary set. Default
#'   `"#1E90FF44"` (dodger blue, 27% opacity).
#' @param title Document title shown in Google Earth. Default
#'   `"Sampling Project"`.
#' @return Invisibly, the path to the created `.kml` file.
#' @export
#' @examples
#' \dontrun{
#' create_earth_project(samples, "output/sampling.kml", buffer_radius = 50)
#' }
create_earth_project <- function(
  samples_list,
  out_file,
  buffer_radius = NULL,
  primary_color = "#FF4500",
  secondary_color = "#1E90FF",
  primary_buffer_color = "#FF450044",
  secondary_buffer_color = "#1E90FF44",
  title = "Sampling Project"
) {
  checkmate::assert_list(samples_list, min.len = 1L)
  checkmate::assert_path_for_output(out_file, overwrite = TRUE)
  if (!is.null(buffer_radius)) {
    checkmate::assert_number(buffer_radius, lower = 0)
  }
  checkmate::assert_string(title)

  community_names <- sort(names(samples_list))
  cli::cli_inform(
    "Building KML for {length(community_names)} communit{?y/ies}..."
  )

  kml <- character()

  # --- Header and styles ---
  kml <- c(
    kml,
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<kml xmlns="http://www.opengis.net/kml/2.2">',
    "<Document>",
    paste0("  <name>", kml_escape(title), "</name>"),
    "",
    kml_point_style("primary_style", primary_color),
    kml_point_style("secondary_style", secondary_color),
    kml_poly_style("primary_buf_style", primary_buffer_color),
    kml_poly_style("secondary_buf_style", secondary_buffer_color)
  )

  # --- Primary points ---
  kml <- c(kml, "  <Folder>", "    <name>Primary Points</name>")
  for (nm in community_names) {
    pts <- samples_list[[nm]]$primary
    if (is.null(pts) || nrow(pts) == 0L) next
    kml <- c(kml, kml_points_folder(pts, nm, "#primary_style"))
  }
  kml <- c(kml, "  </Folder>")

  # --- Secondary points ---
  kml <- c(kml, "  <Folder>", "    <name>Secondary Points</name>")
  for (nm in community_names) {
    pts <- samples_list[[nm]]$secondary
    if (is.null(pts) || nrow(pts) == 0L) next
    kml <- c(kml, kml_points_folder(pts, nm, "#secondary_style"))
  }
  kml <- c(kml, "  </Folder>")

  # --- Primary buffers ---
  kml <- c(kml, "  <Folder>", "    <name>Primary Buffers</name>")
  for (nm in community_names) {
    pts <- samples_list[[nm]]$primary
    if (is.null(pts) || nrow(pts) == 0L) next
    radius <- buffer_radius %||% samples_list[[nm]]$min_distance %||% 50
    bufs <- buffer_sf(pts, radius)
    kml <- c(kml, kml_buffers_folder(bufs, nm, "#primary_buf_style"))
  }
  kml <- c(kml, "  </Folder>")

  # --- Secondary buffers ---
  kml <- c(kml, "  <Folder>", "    <name>Secondary Buffers</name>")
  for (nm in community_names) {
    pts <- samples_list[[nm]]$secondary
    if (is.null(pts) || nrow(pts) == 0L) next
    radius <- buffer_radius %||% samples_list[[nm]]$min_distance %||% 50
    bufs <- buffer_sf(pts, radius)
    kml <- c(kml, kml_buffers_folder(bufs, nm, "#secondary_buf_style"))
  }
  kml <- c(kml, "  </Folder>")

  # --- Footer ---
  kml <- c(kml, "</Document>", "</kml>")

  fs::dir_create(fs::path_dir(out_file), recurse = TRUE)
  writeLines(kml, out_file)
  cli::cli_inform("Created Google Earth project: {.path {out_file}}")
  invisible(out_file)
}


# --- KML helper functions (internal) ---

#' Escape XML special characters
#' @noRd
kml_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x
}

#' Convert #RRGGBB or #RRGGBBAA to KML aabbggrr
#' @noRd
hex_to_kml <- function(hex) {
  hex <- sub("^#", "", hex)
  alpha <- "ff"
  if (nchar(hex) == 8L) {
    alpha <- substr(hex, 7L, 8L)
    hex <- substr(hex, 1L, 6L)
  }
  rr <- substr(hex, 1L, 2L)
  gg <- substr(hex, 3L, 4L)
  bb <- substr(hex, 5L, 6L)
  paste0(alpha, bb, gg, rr)
}

#' Generate KML <Style> block for a point icon
#' @noRd
kml_point_style <- function(id, color) {
  kml_col <- hex_to_kml(color)
  c(
    paste0('  <Style id="', id, '">'),
    "    <IconStyle>",
    paste0("      <color>", kml_col, "</color>"),
    "      <scale>0.8</scale>",
    "      <Icon>",
    "        <href>http://maps.google.com/mapfiles/kml/paddle/wht-blank.png</href>",
    "      </Icon>",
    "    </IconStyle>",
    "    <LabelStyle>",
    "      <scale>0.7</scale>",
    "    </LabelStyle>",
    "  </Style>"
  )
}

#' Generate KML <Style> block for a polygon
#' @noRd
kml_poly_style <- function(id, color) {
  kml_fill <- hex_to_kml(color)
  # Border: same hue, full opacity
  border_hex <- paste0("#", substr(sub("^#", "", color), 1L, 6L))
  kml_border <- hex_to_kml(border_hex)
  c(
    paste0('  <Style id="', id, '">'),
    "    <PolyStyle>",
    paste0("      <color>", kml_fill, "</color>"),
    "    </PolyStyle>",
    "    <LineStyle>",
    paste0("      <color>", kml_border, "</color>"),
    "      <width>1</width>",
    "    </LineStyle>",
    "  </Style>"
  )
}

#' Build a KML folder of point placemarks for one community
#' @noRd
kml_points_folder <- function(pts_sf, community_name, style_url) {
  pts_4326 <- sf::st_transform(pts_sf, 4326L)
  coords <- sf::st_coordinates(pts_4326)
  has_named_pid <- "named_point_id" %in% names(pts_4326)
  has_pid <- "point_id" %in% names(pts_4326)
  has_batch <- "assigned_batch" %in% names(pts_4326)

  lines <- c(
    "    <Folder>",
    paste0("      <name>", kml_escape(community_name), "</name>")
  )

  for (i in seq_len(nrow(pts_4326))) {
    pid <- if (has_named_pid) {
      pts_4326$named_point_id[i]
    } else if (has_pid) {
      pts_4326$point_id[i]
    } else {
      i
    }
    desc_parts <- paste0("Community: ", community_name)
    if (has_batch) {
      desc_parts <- paste0(
        desc_parts,
        "\nBatch: ",
        pts_4326$assigned_batch[i]
      )
    }
    lines <- c(
      lines,
      "      <Placemark>",
      paste0("        <name>", kml_escape(as.character(pid)), "</name>"),
      paste0(
        "        <description>",
        kml_escape(desc_parts),
        "</description>"
      ),
      paste0("        <styleUrl>", style_url, "</styleUrl>"),
      "        <Point>",
      paste0(
        "          <coordinates>",
        coords[i, 1L],
        ",",
        coords[i, 2L],
        ",0",
        "</coordinates>"
      ),
      "        </Point>",
      "      </Placemark>"
    )
  }

  c(lines, "    </Folder>")
}

#' Build a KML folder of buffer polygon placemarks for one community
#' @noRd
kml_buffers_folder <- function(bufs_sf, community_name, style_url) {
  bufs_4326 <- sf::st_transform(bufs_sf, 4326L)
  has_named_pid <- "named_point_id" %in% names(bufs_4326)
  has_pid <- "point_id" %in% names(bufs_4326)

  lines <- c(
    "    <Folder>",
    paste0("      <name>", kml_escape(community_name), "</name>")
  )

  for (i in seq_len(nrow(bufs_4326))) {
    pid <- if (has_named_pid) {
      bufs_4326$named_point_id[i]
    } else if (has_pid) {
      bufs_4326$point_id[i]
    } else {
      i
    }
    geom <- sf::st_geometry(bufs_4326)[[i]]

    # Extract outer ring coordinates (works for POLYGON and MULTIPOLYGON)
    ring <- if (inherits(geom, "MULTIPOLYGON")) geom[[1L]][[1L]] else geom[[1L]]
    coord_str <- paste(
      paste0(ring[, 1L], ",", ring[, 2L], ",0"),
      collapse = " "
    )

    lines <- c(
      lines,
      "      <Placemark>",
      paste0(
        "        <name>Buffer ",
        kml_escape(as.character(pid)),
        "</name>"
      ),
      paste0("        <styleUrl>", style_url, "</styleUrl>"),
      "        <Polygon>",
      "          <outerBoundaryIs>",
      "            <LinearRing>",
      paste0("              <coordinates>", coord_str, "</coordinates>"),
      "            </LinearRing>",
      "          </outerBoundaryIs>",
      "        </Polygon>",
      "      </Placemark>"
    )
  }

  c(lines, "    </Folder>")
}
