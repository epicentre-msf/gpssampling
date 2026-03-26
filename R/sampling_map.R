# Static Mapping Pipeline
#
# Publication-quality static maps showing communities, sampled points,
# and buffers. Uses ggplot2 + maptiles + ggspatial (all in Suggests).

#' Create a static map for one community
#'
#' Renders a publication-quality map showing a community polygon, sampled
#' points (optionally colored by batch), and buffer zones. Returns a
#' `ggplot` object that can be further customized before saving.
#'
#' Requires `ggplot2`, `ggspatial`, and `tidyterra` (all in Suggests).
#'
#' @param community_name Character, community name (used in title).
#' @param community_sf An `sf` POLYGON for the community boundary.
#' @param points_sf An `sf` POINT of sampled points. If it has an
#'   `assigned_batch` column, batch coloring is available.
#' @param buffers_sf Optional `sf` POLYGON of buffers. If `NULL`, no
#'   buffers are drawn.
#' @param batch_colors Logical. If `TRUE` and `points_sf` has
#'   `assigned_batch`, color by batch. Default `TRUE`.
#' @param show_labels Logical. If `TRUE` and `points_sf` has a
#'   `point_id` column, display point IDs as text labels. Default
#'   `TRUE`.
#' @param label_size Numeric, text size for point ID labels. Default
#'   `1.8`.
#' @param basemap Tile provider name for [maptiles::get_tiles()].
#'   Default `"OpenStreetMap.HOT"`.
#' @param point_color Uniform color when `batch_colors = FALSE`.
#'   Default `"#e97a52"`.
#' @param buffer_color Buffer fill color (with alpha).
#'   Default `"#90EE9066"`.
#' @param community_color Boundary stroke color. Default `"#808380"`.
#' @param community_fill Boundary fill color. Default `"#f6efdd"`.
#' @param title Map title. Defaults to `community_name`.
#' @param subtitle Optional subtitle.
#' @return A `ggplot` object.
#' @export
#' @examples
#' \dontrun{
#' p <- map_community("community_one", community_poly, sampled_pts, bufs)
#' ggplot2::ggsave("community_one.png", p, width = 10, height = 12)
#' }
map_community <- function(
  community_name,
  community_sf,
  points_sf,
  buffers_sf = NULL,
  batch_colors = TRUE,
  show_labels = TRUE,
  label_size = 1.8,
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
  checkmate::assert_flag(batch_colors)

  cli::cli_inform(
    "Rendering map for {.val {community_name}} ({nrow(points_sf)} point{?s})..."
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

  if (!is.null(buffers_sf)) {
    p <- p +
      ggplot2::geom_sf(
        data = buffers_sf,
        fill = buffer_color,
        color = NA
      )
  }

  has_batch <- batch_colors &&
    "assigned_batch" %in% names(points_sf)

  if (has_batch) {
    points_sf$assigned_batch <- factor(points_sf$assigned_batch)
    p <- p +
      ggplot2::geom_sf(
        data = points_sf,
        ggplot2::aes(color = .data$assigned_batch),
        size = 2,
        shape = 16
      ) +
      ggplot2::scale_color_brewer(palette = "Set1", name = "Batch")
  } else {
    p <- p +
      ggplot2::geom_sf(
        data = points_sf,
        color = point_color,
        size = 2,
        shape = 16
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
      style = ggspatial::north_arrow_minimal()
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
#' @param samples_list Output of [sample_communities()] or
#'   [split_batches()].
#' @param communities_sf All community polygons (`sf`).
#' @param community_id_col Column name for community ID.
#' @param set Which point set: `"primary"` or `"secondary"`.
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
#' p <- map_overview(samples, communities, community_id_col = "name")
#' }
map_overview <- function(
  samples_list,
  communities_sf,
  community_id_col = "name",
  set = "primary",
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
  for (nm in names(samples_list)) {
    pts <- if (inherits(samples_list[[nm]], "sf")) {
      samples_list[[nm]]
    } else if (
      is.list(samples_list[[nm]]) && set %in% names(samples_list[[nm]])
    ) {
      samples_list[[nm]][[set]]
    } else {
      next
    }
    all_points <- c(all_points, list(pts))
  }

  if (length(all_points) == 0L) {
    cli::cli_abort("No points found in {.arg samples_list}.")
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
      style = ggspatial::north_arrow_minimal()
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
#' Creates per-community maps (primary + secondary, batch-colored) and
#' an overview map. Always returns the ggplot objects. Optionally saves
#' as PNG files.
#'
#' @param samples_list Output of [sample_communities()] or
#'   [split_batches()].
#' @param communities_sf Community polygons (`sf`).
#' @param community_id_col Column name for community ID.
#' @param out_dir Optional output directory. If provided, maps are saved
#'   as PNG. If `NULL`, maps are only returned.
#' @param buffer_radius Buffer radius in meters. Default `50`.
#' @param width Plot width in inches. Default `10`.
#' @param height Plot height in inches. Default `12`.
#' @param dpi Plot resolution. Default `300`.
#' @param ... Additional arguments passed to [map_community()] (colors,
#'   basemap, etc.).
#' @return A named list of `ggplot` objects: `overview`,
#'   `{name}_primary`, `{name}_secondary` for each community.
#' @export
#' @examples
#' \dontrun{
#' maps <- map_all_communities(
#'   samples, communities,
#'   community_id_col = "name",
#'   out_dir = "output/maps"
#' )
#' }
map_all_communities <- function(
  samples_list,
  communities_sf,
  community_id_col = "name",
  out_dir = NULL,
  buffer_radius = 50,
  width = 10,
  height = 12,
  dpi = 300,
  ...
) {
  rlang::check_installed("ggplot2", reason = "for static maps")

  checkmate::assert_class(communities_sf, "sf")
  checkmate::assert_string(community_id_col)
  checkmate::assert_choice(community_id_col, names(communities_sf))

  result <- list()

  cli::cli_inform("Generating overview map...")
  result$overview <- map_overview(
    samples_list,
    communities_sf,
    community_id_col = community_id_col,
    set = "primary",
    buffer_radius = buffer_radius,
    ...
  )

  for (nm in names(samples_list)) {
    community_row <- communities_sf[communities_sf[[community_id_col]] == nm, ]
    if (nrow(community_row) == 0L) {
      cli::cli_warn("Community {.val {nm}} not found in {.arg communities_sf}.")
      next
    }

    for (s in c("primary", "secondary")) {
      pts <- if (inherits(samples_list[[nm]], "sf")) {
        if (s == "primary") samples_list[[nm]] else next
      } else if (
        is.list(samples_list[[nm]]) && s %in% names(samples_list[[nm]])
      ) {
        samples_list[[nm]][[s]]
      } else {
        next
      }

      if (nrow(pts) == 0L) {
        next
      }

      bufs <- buffer_sf(pts, buffer_radius)
      map_name <- paste0(nm, "_", s)

      cli::cli_inform("Generating map for {.val {nm}} ({s})...")
      result[[map_name]] <- map_community(
        community_name = nm,
        community_sf = community_row,
        points_sf = pts,
        buffers_sf = bufs,
        subtitle = s,
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
