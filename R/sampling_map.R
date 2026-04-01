# Static Mapping Pipeline
#
# Publication-quality static maps showing communities, sampled points,
# and buffers. Uses ggplot2 + maptiles + ggspatial (all in Suggests).

#' Compute minimum pairwise distance (meters) for a set of points
#' @noRd
min_pairwise_dist <- function(pts_sf) {
  if (nrow(pts_sf) < 2L) return(NA_real_)
  utm_crs <- auto_utm_crs(pts_sf)
  pts_utm <- sf::st_transform(pts_sf, utm_crs)
  dmat <- sf::st_distance(pts_utm)
  diag(dmat) <- units::set_units(Inf, "m")
  round(as.numeric(min(dmat)), 1)
}


#' Create a static map for one community
#'
#' Renders a publication-quality map showing a community polygon,
#' sampled points (colored by batch), and buffer zones for a single
#' point set (primary or secondary). Returns a `ggplot` object.
#'
#' Requires `ggplot2`, `ggspatial`, and `tidyterra` (all in Suggests).
#'
#' @param community_name Character, community name (used in title).
#' @param community_sf An `sf` POLYGON for the community boundary.
#' @param points_sf An `sf` POINT of sampled points. If it has an
#'   `assigned_batch` column, batch coloring is applied.
#' @param buffers_sf Optional `sf` POLYGON of buffers.
#' @param color_batches Logical. If `TRUE` and `points_sf` has an
#'   `assigned_batch` column, color points by batch. Default `TRUE`.
#' @param show_labels Logical. If `TRUE` and `points_sf` has a
#'   `point_id` column, display point IDs as text labels. Default
#'   `TRUE`.
#' @param label_size Numeric, text size for point ID labels. Default
#'   `1.8`.
#' @param point_shape Marker shape. Default `16` (filled circle).
#' @param basemap Tile provider name for [maptiles::get_tiles()].
#'   Default `"OpenStreetMap.HOT"`.
#' @param point_color Uniform color when no `assigned_batch`. Default
#'   `"#e97a52"`.
#' @param buffer_color Buffer fill color (with alpha). Default
#'   `"#90EE9066"`.
#' @param community_color Boundary stroke color. Default `"#808380"`.
#' @param community_fill Boundary fill color. Default `"#f6efdd"`.
#' @param title Map title. Defaults to `community_name`.
#' @param subtitle Optional subtitle. If `NULL`, auto-generated from
#'   point count, ID range, and min pairwise distance.
#' @return A `ggplot` object.
#' @export
#' @examples
#' \dontrun{
#' p <- map_community("community_one", comm_poly, pri_pts, bufs)
#' ggplot2::ggsave("community_one.png", p, width = 10, height = 12)
#' }
map_community <- function(
  community_name,
  community_sf,
  points_sf,
  buffers_sf = NULL,
  color_batches = TRUE,
  show_labels = TRUE,
  label_size = 1.8,
  point_shape = 16,
  basemap = "OpenStreetMap.HOT",
  point_color = "#e97a52",
  buffer_color = "#90EE9066",
  community_color = "#808380",
  community_fill = "#f6efdd",
  title = community_name,
  subtitle = NULL
) {
  rlang::check_installed("ggplot2", reason = "for static maps")
  rlang::check_installed("ggspatial", reason = "for scale bar and north arrow")
  rlang::check_installed("tidyterra", reason = "for basemap tile rendering")

  checkmate::assert_string(community_name)
  checkmate::assert_class(community_sf, "sf")
  checkmate::assert_class(points_sf, "sf")

  n_pts <- nrow(points_sf)

  cli::cli_inform(
    "Rendering map for {.val {community_name}} ({n_pts} point{?s})..."
  )
  cli::cli_inform("  Downloading basemap tiles...")

  tiles <- tryCatch(
    suppressWarnings(
      maptiles::get_tiles(community_sf, provider = basemap, crop = TRUE)
    ),
    error = function(e) {
      cli::cli_warn("Basemap download failed: {e$message}")
      NULL
    }
  )

  # --- Auto-generate subtitle if not provided ---
  if (is.null(subtitle)) {
    sub_label <- paste0(n_pts, " points")
    if ("point_id" %in% names(points_sf)) {
      sub_label <- paste0(
        sub_label,
        " (",
        min(points_sf$point_id),
        "-",
        max(points_sf$point_id),
        ")"
      )
    }
    mdist <- min_pairwise_dist(points_sf)
    if (!is.na(mdist)) {
      sub_label <- paste0(sub_label, " | min dist: ", mdist, "m")
    }
    subtitle <- sub_label
  }

  # --- Build ggplot ---
  p <- ggplot2::ggplot()

  if (!is.null(tiles)) {
    p <- p + tidyterra::geom_spatraster_rgb(data = tiles)
  }

  p <- p +
    ggplot2::geom_sf(
      data = community_sf,
      fill = community_fill,
      color = community_color,
      linewidth = 0.8,
      alpha = 0.3
    )

  if (!is.null(buffers_sf) && nrow(buffers_sf) > 0L) {
    p <- p +
      ggplot2::geom_sf(
        data = buffers_sf,
        fill = buffer_color,
        color = NA
      )
  }

  has_batch <- color_batches && "assigned_batch" %in% names(points_sf)

  if (has_batch) {
    points_sf$assigned_batch <- factor(points_sf$assigned_batch)
    p <- p +
      ggplot2::geom_sf(
        data = points_sf,
        ggplot2::aes(color = .data$assigned_batch),
        size = 2,
        shape = point_shape
      ) +
      ggplot2::scale_color_brewer(palette = "Set1", name = "Batch")
  } else {
    p <- p +
      ggplot2::geom_sf(
        data = points_sf,
        color = point_color,
        size = 2,
        shape = point_shape
      )
  }

  if (show_labels && "point_id" %in% names(points_sf)) {
    p <- p +
      ggplot2::geom_sf_text(
        data = points_sf,
        ggplot2::aes(label = .data$point_id),
        size = label_size,
        fontface = "bold",
        vjust = -0.8
      )
  }

  p <- p +
    ggspatial::annotation_scale(location = "bl") +
    ggspatial::annotation_north_arrow(
      location = "tr",
      style = ggspatial::north_arrow_minimal(line_width = 0)
    ) +
    ggplot2::labs(title = title, subtitle = subtitle) +
    ggplot2::theme_light() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(size = 10, color = "grey40")
    )

  p
}


#' Create an overview map of all communities
#'
#' Renders a zoomed-out map showing all community polygons, sampled
#' points (uniform color), and buffers. No batch coloring on the
#' overview for clarity.
#'
#' @param points_list Named list of `sf` POINT objects (output of
#'   [split_batches()], or raw `sf` per community).
#' @param communities_sf All community polygons (`sf`).
#' @param community_id_col Column name for community ID.
#' @param buffer_radius Buffer radius in meters. Default `50`.
#' @param basemap Tile provider name. Default `"OpenStreetMap.HOT"`.
#' @param point_color Uniform point color. Default `"#e97a52"`.
#' @param buffer_color Buffer fill color. Default `"#90EE9066"`.
#' @param community_color Boundary stroke color. Default `"#808380"`.
#' @param community_fill Boundary fill color. Default `"#f6efdd"`.
#' @param title Map title. Default `"Sampling Overview"`.
#' @param subtitle Optional subtitle.
#' @return A `ggplot` object.
#' @export
#' @examples
#' \dontrun{
#' p <- map_overview(primary_batches, communities, community_id_col = "name")
#' }
map_overview <- function(
  points_list,
  communities_sf,
  community_id_col = "name",
  buffer_radius = 50,
  basemap = "OpenStreetMap.HOT",
  point_color = "#e97a52",
  buffer_color = "#90EE9066",
  community_color = "#808380",
  community_fill = "#f6efdd",
  title = "Sampling Overview",
  subtitle = NULL
) {
  rlang::check_installed("ggplot2", reason = "for static maps")
  rlang::check_installed("ggspatial", reason = "for scale bar and north arrow")
  rlang::check_installed("tidyterra", reason = "for basemap tile rendering")

  checkmate::assert_class(communities_sf, "sf")
  checkmate::assert_string(community_id_col)

  cli::cli_inform("Rendering overview map...")

  all_points <- list()
  for (nm in names(points_list)) {
    pts <- if (inherits(points_list[[nm]], "sf")) {
      points_list[[nm]]
    } else if (
      is.list(points_list[[nm]]) && "primary" %in% names(points_list[[nm]])
    ) {
      points_list[[nm]][["primary"]]
    } else {
      next
    }
    all_points <- c(all_points, list(pts))
  }

  if (length(all_points) == 0L) {
    cli::cli_abort("No points found in {.arg points_list}.")
  }

  combined_pts <- dplyr::bind_rows(all_points)
  cli::cli_inform(
    "  {nrow(combined_pts)} total point{?s} across {length(all_points)} communit{?y/ies}."
  )
  all_buffers <- buffer_sf(combined_pts, buffer_radius)

  cli::cli_inform("  Downloading basemap tiles...")
  tiles <- tryCatch(
    suppressWarnings(
      maptiles::get_tiles(communities_sf, provider = basemap, crop = TRUE)
    ),
    error = function(e) {
      cli::cli_warn("Basemap download failed: {e$message}")
      NULL
    }
  )

  p <- ggplot2::ggplot()

  if (!is.null(tiles)) {
    p <- p + tidyterra::geom_spatraster_rgb(data = tiles)
  }

  p <- p +
    ggplot2::geom_sf(
      data = communities_sf,
      fill = community_fill,
      color = community_color,
      linewidth = 0.8,
      alpha = 0.3
    ) +
    ggplot2::geom_sf(
      data = all_buffers,
      fill = buffer_color,
      color = NA
    ) +
    ggplot2::geom_sf(
      data = combined_pts,
      color = point_color,
      size = 1.5,
      shape = 16
    )

  if (community_id_col %in% names(communities_sf)) {
    centroids <- suppressWarnings(sf::st_centroid(communities_sf))
    p <- p +
      ggplot2::geom_sf_label(
        data = centroids,
        ggplot2::aes(label = .data[[community_id_col]]),
        size = 3,
        fill = scales::alpha("white", 0.7),
        linewidth = 0
      )
  }

  p <- p +
    ggspatial::annotation_scale(location = "bl") +
    ggspatial::annotation_north_arrow(
      location = "tr",
      style = ggspatial::north_arrow_minimal(line_width = 0)
    ) +
    ggplot2::labs(title = title, subtitle = subtitle) +
    ggplot2::theme_light() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(size = 10, color = "grey40")
    )

  p
}


#' Generate all maps and optionally save to files
#'
#' Creates separate primary and secondary maps per community
#' (batch-colored), plus an overview map. Each map shows one point set
#' with its buffers, point IDs, and min pairwise distance. Always
#' returns the ggplot objects. Optionally saves as PNG files.
#'
#' @param primary_batches Named list of `sf` POINT objects (output of
#'   [split_batches()] with `set = "primary"`).
#' @param communities_sf Community polygons (`sf`).
#' @param community_id_col Column name for community ID.
#' @param secondary_batches Optional named list of `sf` POINT objects
#'   (output of [split_batches()] with `set = "secondary"`). If
#'   `NULL`, only primary maps are created.
#' @param color_batches Logical. If `TRUE` (default) and points have
#'   `assigned_batch`, color by batch.
#' @param out_dir Optional output directory. If provided, maps are
#'   saved as PNG. If `NULL`, maps are only returned.
#' @param buffer_radius Buffer radius in meters. Default `50`.
#' @param primary_shape Marker shape for primary maps. Default `16`
#'   (filled circle).
#' @param secondary_shape Marker shape for secondary maps. Default
#'   `17` (filled triangle).
#' @param primary_buffer_color Buffer fill for primary maps. Default
#'   `"#90EE9066"`.
#' @param secondary_buffer_color Buffer fill for secondary maps.
#'   Default `"#ADD8E666"`.
#' @param width Plot width in inches. Default `10`.
#' @param height Plot height in inches. Default `12`.
#' @param dpi Plot resolution. Default `300`.
#' @param ... Additional arguments passed to [map_community()]
#'   (basemap, label_size, etc.).
#' @return A named list of `ggplot` objects: `overview`,
#'   `{name}_primary`, and `{name}_secondary` for each community.
#' @export
#' @examples
#' \dontrun{
#' pri <- split_batches(samples, n_batches = 5L, set = "primary")
#' sec <- split_batches(samples, n_batches = 5L, set = "secondary")
#' maps <- map_all_communities(
#'   pri, communities,
#'   secondary_batches = sec,
#'   out_dir = "output/maps"
#' )
#' }
map_all_communities <- function(
  primary_batches,
  communities_sf,
  community_id_col = "name",
  secondary_batches = NULL,
  color_batches = TRUE,
  out_dir = NULL,
  buffer_radius = 50,
  primary_shape = 16,
  secondary_shape = 17,
  primary_buffer_color = "#90EE9066",
  secondary_buffer_color = "#ADD8E666",
  width = 10,
  height = 12,
  dpi = 300,
  ...
) {
  rlang::check_installed("ggplot2", reason = "for static maps")

  checkmate::assert_list(primary_batches, min.len = 1L)
  checkmate::assert_class(communities_sf, "sf")
  checkmate::assert_string(community_id_col)
  checkmate::assert_choice(community_id_col, names(communities_sf))

  result <- list()

  # --- Overview map (primary points only) ---
  cli::cli_inform("Generating overview map...")
  result$overview <- map_overview(
    primary_batches,
    communities_sf,
    community_id_col = community_id_col,
    buffer_radius = buffer_radius,
    ...
  )

  # --- Per-community maps (separate primary and secondary) ---
  for (nm in names(primary_batches)) {
    community_row <- communities_sf[communities_sf[[community_id_col]] == nm, ]
    if (nrow(community_row) == 0L) {
      cli::cli_warn(
        "Community {.val {nm}} not found in {.arg communities_sf}."
      )
      next
    }

    # Primary map
    pri_pts <- primary_batches[[nm]]
    if (inherits(pri_pts, "sf") && nrow(pri_pts) > 0L) {
      pri_bufs <- buffer_sf(pri_pts, buffer_radius)
      map_name <- paste0(nm, "_primary")

      cli::cli_inform("Generating map for {.val {nm}} (primary)...")
      result[[map_name]] <- map_community(
        community_name = nm,
        community_sf = community_row,
        points_sf = pri_pts,
        buffers_sf = pri_bufs,
        color_batches = color_batches,
        point_shape = primary_shape,
        buffer_color = primary_buffer_color,
        ...
      )
    }

    # Secondary map
    if (
      !is.null(secondary_batches) &&
        nm %in% names(secondary_batches) &&
        inherits(secondary_batches[[nm]], "sf") &&
        nrow(secondary_batches[[nm]]) > 0L
    ) {
      sec_pts <- secondary_batches[[nm]]
      sec_bufs <- buffer_sf(sec_pts, buffer_radius)
      map_name <- paste0(nm, "_secondary")

      # Compute min distances: secondary-only and all (primary + secondary)
      sec_min_dist <- min_pairwise_dist(sec_pts)
      all_community_pts <- dplyr::bind_rows(pri_pts, sec_pts)
      all_min_dist <- min_pairwise_dist(all_community_pts)
      sec_subtitle <- NULL
      n_sec <- nrow(sec_pts)
      if (!is.na(sec_min_dist) && !is.na(all_min_dist)) {
        sec_subtitle <- paste0(
          n_sec,
          " points"
        )
        if ("point_id" %in% names(sec_pts)) {
          sec_subtitle <- paste0(
            sec_subtitle,
            " (",
            min(sec_pts$point_id),
            "-",
            max(sec_pts$point_id),
            ")"
          )
        }
        sec_subtitle <- paste0(
          sec_subtitle,
          " | min dist (sec): ",
          sec_min_dist,
          "m",
          " | min dist (all): ",
          all_min_dist,
          "m"
        )
      }

      cli::cli_inform("Generating map for {.val {nm}} (secondary)...")
      result[[map_name]] <- map_community(
        community_name = nm,
        community_sf = community_row,
        points_sf = sec_pts,
        buffers_sf = sec_bufs,
        color_batches = color_batches,
        point_shape = secondary_shape,
        buffer_color = secondary_buffer_color,
        subtitle = sec_subtitle,
        ...
      )
    }
  }

  if (!is.null(out_dir)) {
    fs::dir_create(out_dir, recurse = TRUE)
    for (map_name in names(result)) {
      fpath <- fs::path(out_dir, paste0(map_name, ".png"))
      suppressWarnings(ggplot2::ggsave(
        as.character(fpath),
        plot = result[[map_name]],
        width = width,
        height = height,
        dpi = dpi
      ))
      cli::cli_inform("Saved {.path {fpath}}")
    }
  }

  invisible(result)
}


#' Map cropped buildings per community
#'
#' Produces one large map per community showing the community boundary
#' and building footprint shapes over an OSM basemap. Returns a named
#' list of `ggplot` objects (one per community).
#'
#' By default, buildings are filtered to those overlapping each community
#' (fast spatial predicate) but not geometrically clipped. Set
#' `clip = TRUE` for exact boundary clipping (slower, needed only when
#' boundary-edge precision matters for publication).
#'
#' Requires `ggplot2`, `ggspatial`, and `tidyterra` (all in Suggests).
#'
#' @param buildings_sf An `sf` POLYGON of building footprints (the same
#'   input you passed to [crop_buildings()]).
#' @param communities_sf Community polygons (`sf`).
#' @param community_id_col Column name for community ID in
#'   `communities_sf`. Default `"name"`.
#' @param clip Logical. If `TRUE`, buildings are geometrically clipped
#'   to the community boundary (slower but precise). If `FALSE`
#'   (default), buildings that intersect the community are drawn with
#'   their full footprint (fast).
#' @param building_fill Fill color for building footprints.
#'   Default `"#8B6914"` (brown).
#' @param building_stroke Stroke color for building outlines.
#'   Default `"#5B3A29"` (dark brown).
#' @param community_color Boundary stroke color. Default `"#c98585"`.
#' @param community_fill Boundary fill color (with transparency).
#'   Default `"#faf3e820"`.
#' @param basemap Tile provider name for [maptiles::get_tiles()].
#'   Default `"OpenStreetMap"`.
#' @param out_dir Optional output directory. If provided, maps are saved
#'   as PNG. If `NULL`, maps are only returned.
#' @param width Plot width in inches. Default `12`.
#' @param height Plot height in inches. Default `12`.
#' @param dpi Plot resolution. Default `300`.
#' @return A named list of `ggplot` objects, one per community.
#' @export
#' @examples
#' \dontrun{
#' maps <- map_cropped_buildings(buildings, communities)
#' ggplot2::ggsave("community_one.png", maps[["community_one"]])
#' }
map_cropped_buildings <- function(
  buildings_sf,
  communities_sf,
  community_id_col = "name",
  clip = FALSE,
  building_fill = "#8B6914",
  building_stroke = "#5B3A29",
  community_color = "#c98585",
  community_fill = "#faf3e820",
  basemap = "OpenStreetMap",
  out_dir = NULL,
  width = 12,
  height = 12,
  dpi = 300
) {
  rlang::check_installed("ggplot2", reason = "for static maps")
  rlang::check_installed("ggspatial", reason = "for scale bar and north arrow")
  rlang::check_installed("tidyterra", reason = "for basemap tile rendering")

  checkmate::assert_class(buildings_sf, "sf")
  checkmate::assert_class(communities_sf, "sf")
  checkmate::assert_string(community_id_col)
  checkmate::assert_choice(community_id_col, names(communities_sf))
  checkmate::assert_flag(clip)

  n_buildings <- nrow(buildings_sf)

  if (sf::st_crs(buildings_sf) != sf::st_crs(communities_sf)) {
    cli::cli_inform("Aligning CRS...")
    buildings_sf <- sf::st_transform(
      buildings_sf,
      sf::st_crs(communities_sf)
    )
  }
  if (clip) {
    cli::cli_inform("Validating geometries...")
    buildings_sf <- sf::st_make_valid(buildings_sf)
    sf::st_agr(buildings_sf) <- "constant"
  }

  # Dissolve communities by ID: a community may span multiple rows
  # (e.g. multipolygon stored as separate features with the same name).
  n_rows_raw <- nrow(communities_sf)
  n_unique <- length(unique(communities_sf[[community_id_col]]))
  if (n_unique < n_rows_raw) {
    cli::cli_inform(
      "Dissolving {n_rows_raw} feature{?s} into {n_unique} unique communit{?y/ies}..."
    )
  }
  communities_sf <- communities_sf |>
    dplyr::select(dplyr::all_of(community_id_col)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(community_id_col))) |>
    dplyr::summarise(.groups = "drop")
  sf::st_agr(communities_sf) <- "constant"

  n_communities <- nrow(communities_sf)
  clip_label <- if (clip) "clipping" else "filtering"
  cli::cli_inform(
    "Mapping {n_buildings} building footprint{?s} across {n_communities} communit{?y/ies} ({clip_label} mode)..."
  )

  community_names <- communities_sf[[community_id_col]]
  result <- list()

  # Pre-compute all intersections at once (spatial index)
  hits_list <- sf::st_intersects(communities_sf, buildings_sf)

  for (i in seq_along(community_names)) {
    nm <- community_names[i]
    community_row <- communities_sf[i, ]

    # Fast spatial predicate: find buildings that overlap this community
    cli::cli_inform("  {.val {nm}}: finding overlapping buildings...")
    hits <- hits_list[[i]]

    if (length(hits) == 0L) {
      cli::cli_warn("No buildings in {.val {nm}}, skipping.")
      next
    }

    if (clip) {
      # Exact geometric clip (slower)
      cli::cli_inform(
        "  {.val {nm}}: clipping {length(hits)} building{?s} to boundary..."
      )
      plot_buildings <- tryCatch(
        suppressWarnings(
          sf::st_intersection(
            buildings_sf[hits, ],
            sf::st_geometry(community_row)
          )
        ),
        error = function(e) {
          cli::cli_warn("Clipping failed for {.val {nm}}: {e$message}")
          NULL
        }
      )

      if (is.null(plot_buildings) || nrow(plot_buildings) == 0L) {
        cli::cli_warn("No buildings in {.val {nm}}, skipping.")
        next
      }

      # Keep only polygon geometries after clipping
      geom_types <- sf::st_geometry_type(plot_buildings)
      is_poly <- geom_types %in% c("POLYGON", "MULTIPOLYGON")
      if (!all(is_poly)) {
        plot_buildings <- plot_buildings[is_poly, ]
      }
      if (nrow(plot_buildings) == 0L) next
    } else {
      # Fast: use pre-filtered buildings as-is (no geometric clipping)
      plot_buildings <- buildings_sf[hits, ]
    }

    n_plot <- nrow(plot_buildings)
    cli::cli_inform(
      "  {.val {nm}}: rendering map ({n_plot} building{?s})..."
    )

    tiles <- tryCatch(
      suppressWarnings(
        maptiles::get_tiles(community_row, provider = basemap, crop = TRUE)
      ),
      error = function(e) {
        cli::cli_warn("Basemap download failed: {e$message}")
        NULL
      }
    )

    p <- ggplot2::ggplot()

    if (!is.null(tiles)) {
      p <- p + tidyterra::geom_spatraster_rgb(data = tiles)
    }

    # Strip attribute columns — only geometry is needed for fixed-aesthetic
    # rendering. This avoids jsonlite serialization errors when character
    # columns contain logical NA values from upstream spatial operations.
    plot_geom <- sf::st_sf(geometry = sf::st_geometry(plot_buildings))

    p <- p +
      ggplot2::geom_sf(
        data = community_row,
        fill = community_fill,
        color = community_color,
        linewidth = 0.6
      ) +
      ggplot2::geom_sf(
        data = plot_geom,
        fill = building_fill,
        color = building_stroke,
        linewidth = 0.15,
        alpha = 0.7
      ) +
      ggspatial::annotation_scale(location = "bl") +
      ggspatial::annotation_north_arrow(
        location = "tr",
        style = ggspatial::north_arrow_minimal(line_width = 0)
      ) +
      ggplot2::labs(
        title = nm,
        subtitle = paste0(n_plot, " buildings")
      ) +
      ggplot2::theme_light() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(
          face = "bold",
          size = 16,
          hjust = 0.5
        ),
        plot.subtitle = ggplot2::element_text(
          size = 11,
          hjust = 0.5,
          color = "grey40"
        ),
        plot.margin = ggplot2::margin(10, 10, 10, 10)
      )

    result[[nm]] <- p
  }

  if (length(result) == 0L) {
    cli::cli_abort("No valid communities to plot.")
  }

  cli::cli_inform(
    "Generated {length(result)} building map{?s}."
  )

  if (!is.null(out_dir)) {
    fs::dir_create(out_dir, recurse = TRUE)
    cli::cli_inform("Saving maps to {.path {out_dir}}...")
    for (nm in names(result)) {
      fpath <- fs::path(out_dir, paste0(nm, "_buildings.png"))
      suppressWarnings(ggplot2::ggsave(
        as.character(fpath),
        plot = result[[nm]],
        width = width,
        height = height,
        dpi = dpi
      ))
      cli::cli_inform("Saved {.path {fpath}}")
    }
  }

  invisible(result)
}


#' Interactive leaflet map of all communities
#'
#' Creates an interactive [leaflet::leaflet()] map with toggleable
#' layers for communities, buildings, roads, primary points, and
#' secondary points. Points are colored by batch when
#' `color_batches = TRUE`. Primary points are shown as circles,
#' secondary as triangles. Layers are z-ordered via custom panes:
#' communities (bottom), roads, buildings, buffers, points (top).
#'
#' Includes fullscreen control (if `leaflet.extras` is installed),
#' a community navigation panel for quick zoom, and multiple base
#' map options.
#'
#' @param primary_batches Named list of `sf` POINT objects (output of
#'   [split_batches()] with `set = "primary"`).
#' @param communities_sf Community polygons (`sf`).
#' @param community_id_col Column name for community ID.
#' @param secondary_batches Optional named list of `sf` POINT objects
#'   (output of [split_batches()] with `set = "secondary"`).
#' @param buildings_list Optional named list of `sf` objects, one per
#'   community (output of [crop_buildings()]). Accepts both POINT
#'   (centroids) and POLYGON (footprints). Hidden by default; toggle
#'   via layer control.
#' @param roads_list Optional named list of `sf` LINESTRING objects
#'   (output of [fetch_community_roads()]). One entry per community.
#' @param color_batches Logical. If `TRUE` (default) and points have
#'   `assigned_batch`, color by batch.
#' @param buffer_radius Buffer radius in meters. Default `50`.
#' @param primary_color Default color for primary points. Default
#'   `"#e97a52"`.
#' @param secondary_color Default color for secondary points. Default
#'   `"#1E90FF"`.
#' @param primary_buffer_color Fill for primary buffers. Default
#'   `"#90EE90"`.
#' @param secondary_buffer_color Fill for secondary buffers. Default
#'   `"#ADD8E6"`.
#' @param community_color Boundary stroke color. Default `"#808380"`.
#' @param building_color Fill/stroke for building footprints. Default
#'   `"#8B6914"`.
#' @param road_color Stroke color for roads. Default `"#555555"`.
#' @param out_file Optional file path for saving as a self-contained
#'   HTML file. Requires `htmlwidgets`. If `NULL` (default), no file
#'   is saved.
#' @return A `leaflet` htmlwidget object.
#' @export
#' @examples
#' \dontrun{
#' pri <- split_batches(samples, n_batches = 5L, set = "primary")
#' sec <- split_batches(samples, n_batches = 5L, set = "secondary")
#' m <- leaflet_communities(
#'   pri, communities,
#'   secondary_batches = sec,
#'   buildings_list = buildings_cropped,
#'   roads_list = roads
#' )
#' m
#' }
leaflet_communities <- function(
  primary_batches,
  communities_sf,
  community_id_col = "name",
  secondary_batches = NULL,
  buildings_list = NULL,
  roads_list = NULL,
  color_batches = TRUE,
  buffer_radius = 50,
  primary_color = "#e97a52",
  secondary_color = "#1E90FF",
  primary_buffer_color = "#90EE90",
  secondary_buffer_color = "#ADD8E6",
  community_color = "#808380",
  building_color = "#8B6914",
  road_color = "#555555",
  out_file = NULL
) {
  checkmate::assert_list(primary_batches, min.len = 1L)
  checkmate::assert_class(communities_sf, "sf")
  checkmate::assert_string(community_id_col)

  cli::cli_inform("Building interactive map...")

  community_names <- names(primary_batches)

  # --- Dissolve multipolygon communities for navigation ---
  dissolved_sf <- communities_sf |>
    dplyr::select(dplyr::all_of(community_id_col)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(community_id_col))) |>
    dplyr::summarise(.groups = "drop")

  # --- Compute community bounding boxes for navigation ---
  bbox_list <- lapply(seq_len(nrow(dissolved_sf)), function(i) {
    bb <- sf::st_bbox(dissolved_sf[i, ])
    c(
      unname(bb[["ymin"]]),
      unname(bb[["xmin"]]),
      unname(bb[["ymax"]]),
      unname(bb[["xmax"]])
    )
  })
  names(bbox_list) <- dissolved_sf[[community_id_col]]
  bbox_json <- jsonlite::toJSON(bbox_list, auto_unbox = TRUE)

  # --- Create map with custom panes via onRender ---
  pane_js <- sprintf(
    "
    function(el, x) {
      var map = this;

      // Custom panes for z-ordering
      map.createPane('communities'); map.getPane('communities').style.zIndex = 410;
      map.createPane('roads');       map.getPane('roads').style.zIndex = 415;
      map.createPane('buildings');   map.getPane('buildings').style.zIndex = 420;
      map.createPane('buffers');     map.getPane('buffers').style.zIndex = 425;
      map.createPane('points');      map.getPane('points').style.zIndex = 430;

      // Community navigation panel
      var communityBounds = %s;
      var nav = L.control({position: 'topleft'});
      nav.onAdd = function() {
        var div = L.DomUtil.create('div', 'community-nav');
        div.style.cssText = 'background:rgba(246,246,248,0.92); padding:6px; border-radius:10px; box-shadow:0 1px 4px rgba(0,0,0,0.12); max-height:300px; overflow-y:auto; backdrop-filter:blur(20px); -webkit-backdrop-filter:blur(20px);';
        var html = '<div style=\"font-weight:600; font-size:10px; margin-bottom:4px; text-align:center; color:#86868B; text-transform:uppercase; letter-spacing:0.5px;\">Communities</div>';
        for (var cname in communityBounds) {
          html += '<button class=\"comm-nav-btn\" data-community=\"' + cname + '\" style=\"display:block; width:100%%; margin:2px 0; padding:6px 12px; background:#FFFFFF; color:#1D1D1F; border:0.5px solid #D2D2D7; border-radius:6px; cursor:pointer; font-weight:500; font-size:12px; font-family:-apple-system,BlinkMacSystemFont,sans-serif; text-align:left; transition:background 0.15s ease;\">' + cname + '</button>';
        }
        div.innerHTML = html;
        L.DomEvent.disableClickPropagation(div);
        L.DomEvent.disableScrollPropagation(div);
        return div;
      };
      nav.addTo(map);

      setTimeout(function() {
        var buttons = document.querySelectorAll('.comm-nav-btn');
        buttons.forEach(function(btn) {
          btn.addEventListener('click', function(e) {
            e.stopPropagation();
            var bounds = communityBounds[btn.getAttribute('data-community')];
            if (bounds) {
              map.fitBounds([[bounds[0], bounds[1]], [bounds[2], bounds[3]]], {padding: [50, 50], maxZoom: 15});
            }
          });
          btn.addEventListener('mouseenter', function() { btn.style.background = '#E8E8ED'; });
          btn.addEventListener('mouseleave', function() { btn.style.background = '#FFFFFF'; });
        });
      }, 500);
    }
    ",
    bbox_json
  )

  m <- leaflet::leaflet(width = "100%", height = "800px") |>
    leaflet::addTiles(group = "OpenStreetMap") |>
    leaflet::addProviderTiles("CartoDB.Positron", group = "Light") |>
    leaflet::addProviderTiles("Esri.WorldImagery", group = "Satellite") |>
    leaflet::addProviderTiles("OpenStreetMap.HOT", group = "OSM Humanitarian")

  # --- Communities layer (pane: communities) ---
  m <- m |>
    leaflet::addPolygons(
      data = communities_sf,
      color = community_color,
      weight = 2,
      fillOpacity = 0.1,
      label = ~ get(community_id_col),
      labelOptions = leaflet::labelOptions(
        noHide = TRUE,
        direction = "center",
        textOnly = FALSE,
        style = list(
          "background-color" = "#FFEACC",
          "color" = "#1D1D1F",
          "font-weight" = "bold",
          "font-size" = "12px",
          "padding" = "4px 10px",
          "border-radius" = "4px",
          "border" = "1px solid #E8C99B",
          "box-shadow" = "0 1px 3px rgba(0,0,0,0.3)"
        )
      ),
      highlightOptions = leaflet::highlightOptions(
        weight = 4,
        fillOpacity = 0.2,
        bringToFront = FALSE
      ),
      group = "Communities",
      options = leaflet::pathOptions(pane = "communities")
    )

  # --- Roads layer (pane: roads) ---
  if (!is.null(roads_list)) {
    all_roads <- tryCatch(
      dplyr::bind_rows(roads_list),
      error = function(e) NULL
    )
    if (!is.null(all_roads) && nrow(all_roads) > 0L) {
      # Keep only geometry to avoid column conflicts
      roads_geom <- sf::st_sf(geometry = sf::st_geometry(all_roads))
      m <- m |>
        leaflet::addPolylines(
          data = roads_geom,
          color = road_color,
          weight = 2,
          opacity = 0.7,
          group = "Roads",
          options = leaflet::pathOptions(pane = "roads")
        )
    }
  }

  # --- Buildings layer (pane: buildings, hidden by default) ---
  if (!is.null(buildings_list) && length(buildings_list) > 0L) {
    all_bldg <- tryCatch(
      dplyr::bind_rows(buildings_list),
      error = function(e) NULL
    )
    if (!is.null(all_bldg) && nrow(all_bldg) > 0L) {
      bldg_geom <- sf::st_sf(geometry = sf::st_geometry(all_bldg))
      geom_type <- unique(as.character(sf::st_geometry_type(bldg_geom)))
      is_polygon <- any(geom_type %in% c("POLYGON", "MULTIPOLYGON"))

      if (is_polygon) {
        m <- m |>
          leaflet::addPolygons(
            data = bldg_geom,
            color = building_color,
            weight = 0.5,
            fillColor = building_color,
            fillOpacity = 0.4,
            group = "Buildings",
            options = leaflet::pathOptions(pane = "buildings")
          )
      } else {
        m <- m |>
          leaflet::addCircleMarkers(
            data = bldg_geom,
            radius = 2,
            color = building_color,
            fillColor = building_color,
            fillOpacity = 0.6,
            stroke = FALSE,
            group = "Buildings",
            options = leaflet::pathOptions(pane = "buildings")
          )
      }
    }
  }

  # --- Batch color palette ---
  all_batches <- sort(unique(unlist(lapply(
    primary_batches,
    function(x) {
      if ("assigned_batch" %in% names(x)) unique(x$assigned_batch)
    }
  ))))
  batch_pal <- if (length(all_batches) > 0L && color_batches) {
    leaflet::colorFactor("Set1", domain = all_batches)
  }

  # --- Pass 1: Add ALL buffers first (renders below points) ---
  for (nm in community_names) {
    pri_pts <- primary_batches[[nm]]
    if (!inherits(pri_pts, "sf") || nrow(pri_pts) == 0L) next

    pri_bufs <- buffer_sf(pri_pts, buffer_radius)
    m <- m |>
      leaflet::addPolygons(
        data = pri_bufs,
        color = primary_buffer_color,
        fillColor = primary_buffer_color,
        fillOpacity = 0.2,
        weight = 1,
        group = "Primary Buffers",
        options = leaflet::pathOptions(pane = "buffers")
      )

    if (
      !is.null(secondary_batches) &&
        nm %in% names(secondary_batches) &&
        inherits(secondary_batches[[nm]], "sf") &&
        nrow(secondary_batches[[nm]]) > 0L
    ) {
      sec_bufs <- buffer_sf(secondary_batches[[nm]], buffer_radius)
      m <- m |>
        leaflet::addPolygons(
          data = sec_bufs,
          color = secondary_buffer_color,
          fillColor = secondary_buffer_color,
          fillOpacity = 0.2,
          weight = 1,
          group = "Secondary Buffers",
          options = leaflet::pathOptions(pane = "buffers")
        )
    }
  }

  # --- Pass 2: Add ALL points on top of buffers ---
  for (nm in community_names) {
    pri_pts <- primary_batches[[nm]]
    if (!inherits(pri_pts, "sf") || nrow(pri_pts) == 0L) next

    has_batch <- color_batches && "assigned_batch" %in% names(pri_pts)
    has_pid <- "point_id" %in% names(pri_pts)

    pri_colors <- if (has_batch && !is.null(batch_pal)) {
      batch_pal(pri_pts$assigned_batch)
    } else {
      primary_color
    }
    pri_labels <- if (has_pid) {
      as.character(pri_pts$point_id)
    } else {
      as.character(seq_len(nrow(pri_pts)))
    }
    pri_popups <- if (has_batch) {
      paste0(
        "<b>ID:</b> ",
        pri_labels,
        "<br><b>Batch:</b> ",
        pri_pts$assigned_batch,
        "<br><b>Community:</b> ",
        nm
      )
    } else {
      paste0("<b>ID:</b> ", pri_labels, "<br><b>Community:</b> ", nm)
    }

    # Primary points (circles)
    m <- m |>
      leaflet::addCircleMarkers(
        data = pri_pts,
        radius = 6,
        color = pri_colors,
        fillColor = pri_colors,
        fillOpacity = 0.9,
        stroke = TRUE,
        weight = 2,
        label = pri_labels,
        popup = pri_popups,
        group = "Primary Points",
        options = leaflet::pathOptions(pane = "points")
      )

    # Secondary points
    if (
      !is.null(secondary_batches) &&
        nm %in% names(secondary_batches) &&
        inherits(secondary_batches[[nm]], "sf") &&
        nrow(secondary_batches[[nm]]) > 0L
    ) {
      sec_pts <- secondary_batches[[nm]]
      has_sec_batch <- color_batches &&
        "assigned_batch" %in% names(sec_pts)
      has_sec_pid <- "point_id" %in% names(sec_pts)

      sec_colors <- if (has_sec_batch && !is.null(batch_pal)) {
        batch_pal(sec_pts$assigned_batch)
      } else {
        secondary_color
      }
      sec_labels <- if (has_sec_pid) {
        as.character(sec_pts$point_id)
      } else {
        as.character(seq_len(nrow(sec_pts)))
      }
      sec_popups <- if (has_sec_batch) {
        paste0(
          "<b>ID:</b> ",
          sec_labels,
          "<br><b>Batch:</b> ",
          sec_pts$assigned_batch,
          "<br><b>Community:</b> ",
          nm
        )
      } else {
        paste0(
          "<b>ID:</b> ",
          sec_labels,
          "<br><b>Community:</b> ",
          nm
        )
      }

      # Secondary points (triangle markers via SVG data URI icons)
      svg_template <- '<svg xmlns="http://www.w3.org/2000/svg" width="14" height="14" viewBox="0 0 14 14"><polygon points="7,0 14,14 0,14" fill="%s" stroke="white" stroke-width="1.5"/></svg>'
      icon_urls <- vapply(
        sec_colors,
        function(clr) {
          svg_str <- sprintf(svg_template, clr)
          paste0(
            "data:image/svg+xml;base64,",
            base64enc::base64encode(charToRaw(svg_str))
          )
        },
        character(1L)
      )

      sec_icons <- leaflet::icons(
        iconUrl = icon_urls,
        iconWidth = 14,
        iconHeight = 14,
        iconAnchorX = 7,
        iconAnchorY = 7
      )

      m <- m |>
        leaflet::addMarkers(
          data = sec_pts,
          icon = sec_icons,
          label = sec_labels,
          popup = sec_popups,
          group = "Secondary Points",
          options = leaflet::markerOptions(pane = "points")
        )
    }
  }

  # --- Layer control ---
  overlay_groups <- "Communities"
  if (!is.null(roads_list)) overlay_groups <- c(overlay_groups, "Roads")
  if (!is.null(buildings_list)) overlay_groups <- c(overlay_groups, "Buildings")
  overlay_groups <- c(
    overlay_groups,
    "Primary Points",
    "Primary Buffers"
  )
  if (!is.null(secondary_batches)) {
    overlay_groups <- c(
      overlay_groups,
      "Secondary Points",
      "Secondary Buffers"
    )
  }

  m <- m |>
    leaflet::addLayersControl(
      baseGroups = c("OpenStreetMap", "Light", "Satellite", "OSM Humanitarian"),
      overlayGroups = overlay_groups,
      options = leaflet::layersControlOptions(collapsed = FALSE)
    )

  # Hide buildings by default (heavy layer)
  if (!is.null(buildings_list)) {
    m <- m |> leaflet::hideGroup("Buildings")
  }

  # Batch legend
  if (!is.null(batch_pal) && length(all_batches) > 0L) {
    m <- m |>
      leaflet::addLegend(
        position = "bottomright",
        pal = batch_pal,
        values = all_batches,
        title = "Batch"
      )
  }

  # --- Fullscreen control ---
  if (requireNamespace("leaflet.extras", quietly = TRUE)) {
    m <- m |> leaflet.extras::addFullscreenControl(position = "topleft")
  }

  # --- Apply panes + navigation JS ---
  m <- m |> htmlwidgets::onRender(pane_js)

  # --- Save to file ---
  if (!is.null(out_file)) {
    rlang::check_installed("htmlwidgets", reason = "to save leaflet as HTML")
    fs::dir_create(fs::path_dir(out_file), recurse = TRUE)
    htmlwidgets::saveWidget(m, file = out_file, selfcontained = TRUE)
    cli::cli_inform("Saved interactive map to {.path {out_file}}")
  }

  cli::cli_inform("Interactive map ready.")
  m
}
