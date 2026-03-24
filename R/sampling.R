# Programmatic Sampling Pipeline
#
# Standalone exported functions for reproducible spatial sampling of
# buildings within community polygons. Pipe-friendly, composable via |>.

# Internal Helpers
# ............................................................................

#' Auto-detect UTM CRS from geometry
#'
#' Computes the appropriate UTM zone EPSG code based on the centroid of
#' the input geometry's bounding box. Northern hemisphere returns 326xx,
#' southern hemisphere returns 327xx.
#'
#' @param x An `sf` or `sfc` object.
#' @return An integer EPSG code (e.g., 32637 for UTM zone 37N).
#' @noRd
auto_utm_crs <- function(x) {
  bbox <- sf::st_bbox(sf::st_transform(x, 4326L))
  center_lon <- mean(c(bbox[["xmin"]], bbox[["xmax"]]))
  center_lat <- mean(c(bbox[["ymin"]], bbox[["ymax"]]))
  utm_zone <- floor((center_lon + 180) / 6) + 1L
  if (center_lat >= 0) 32600L + utm_zone else 32700L + utm_zone
}

#' Fetch OSM building footprints
#'
#' Downloads OpenStreetMap building footprints for a given area using
#' tile-based Overpass API queries.
#'
#' @param area_sf An `sf` polygon or multipolygon defining the area of
#'   interest. Any CRS is accepted (transformed internally to EPSG:4326).
#' @param zoom Integer zoom level for tile-based queries (default `13L`,
#'   matching the package's `ZOOM_OSM` constant).
#' @return An `sf` POLYGON with columns `osm_id` (character), `building`
#'   (character), and `geometry`. Returns an empty `sf` with the correct
#'   schema if no buildings are found.
#' @export
#' @examples
#' \dontrun{
#' state <- sf::st_read("boundary.gpkg")
#' buildings <- fetch_osm_buildings(state)
#' }
fetch_osm_buildings <- function(area_sf, zoom = ZOOM_OSM) {
  checkmate::assert_class(area_sf, "sf")
  checkmate::assert_int(zoom, lower = 1L, upper = 19L)

  area_4326 <- sf::st_transform(area_sf, 4326L)
  bbox <- sf::st_bbox(area_4326)
  tile_grid <- slippymath::bbox_to_tile_grid(bbox, zoom = zoom)
  n_tiles <- nrow(tile_grid$tiles)

  if (n_tiles > OSM_MAX_TILES) {
    cli::cli_warn(c(
      "Area spans {n_tiles} tiles (max recommended: {OSM_MAX_TILES}).",
      "i" = "Consider reducing the area or increasing {.arg zoom}."
    ))
  }

  empty_result <- sf::st_sf(
    osm_id = character(),
    building = character(),
    geometry = sf::st_sfc(crs = 4326L)
  )

  all_buildings <- list()

  for (i in seq_len(n_tiles)) {
    tile_x <- tile_grid$tiles$x[i]
    tile_y <- tile_grid$tiles$y[i]
    tile_bb <- tile_bbox_ll(tile_x, tile_y, zoom)
    tile_sfc <- sf::st_as_sfc(sf::st_bbox(
      c(
        xmin = tile_bb[["xmin"]],
        ymin = tile_bb[["ymin"]],
        xmax = tile_bb[["xmax"]],
        ymax = tile_bb[["ymax"]]
      ),
      crs = 4326L
    ))

    osm <- tryCatch(
      osmdata::opq(bbox = sf::st_bbox(tile_sfc)) |>
        osmdata::add_osm_feature(key = "building") |>
        osmdata::osmdata_sf() |>
        cleanOSM(),
      error = function(e) {
        cli::cli_warn(
          "OSM query failed for tile ({tile_x}, {tile_y}): {e$message}"
        )
        NULL
      }
    )

    if (
      !is.null(osm) &&
        is.defined(osm$osm_polygons) &&
        nrow(osm$osm_polygons) > 0L
    ) {
      tile_buildings <- osm$osm_polygons |>
        dplyr::select(dplyr::any_of(c("osm_id", "building")))
      all_buildings <- c(all_buildings, list(tile_buildings))
    }

    if (i %% 10L == 0L && i < n_tiles) {
      Sys.sleep(0.5)
    }
  }

  if (length(all_buildings) == 0L) {
    cli::cli_inform("No buildings found in the specified area.")
    return(empty_result)
  }

  buildings_sf <- dplyr::bind_rows(all_buildings) |>
    sf::st_make_valid()

  if (!"osm_id" %in% names(buildings_sf)) {
    buildings_sf$osm_id <- as.character(seq_len(nrow(buildings_sf)))
  }
  if (!"building" %in% names(buildings_sf)) {
    buildings_sf$building <- "yes"
  }

  buildings_sf <- buildings_sf |>
    dplyr::select("osm_id", "building")
  sf::st_agr(buildings_sf) <- "constant"
  buildings_sf <- buildings_sf |>
    sf::st_intersection(sf::st_geometry(area_4326)) |>
    sf::st_make_valid()

  geom_types <- sf::st_geometry_type(buildings_sf)
  is_poly <- geom_types %in% c("POLYGON", "MULTIPOLYGON")
  if (!all(is_poly)) {
    buildings_sf <- buildings_sf[is_poly, ]
  }

  if (any(sf::st_geometry_type(buildings_sf) == "MULTIPOLYGON")) {
    buildings_sf <- suppressWarnings(sf::st_cast(buildings_sf, "POLYGON"))
  }

  buildings_sf <- buildings_sf[!duplicated(buildings_sf$osm_id), ]
  buildings_sf
}


#' Filter building footprints by type
#'
#' Removes non-residential buildings (hospitals, schools, etc.) using OSM
#' `building` tags. Supports three workflows: OSM-only, user footprints
#' with OSM labeling, and user footprints without OSM.
#'
#' @param buildings_sf An `sf` POLYGON of building footprints. Can be
#'   output from [fetch_osm_buildings()] (has `building` column) or a
#'   user-provided dataset.
#' @param osm_buildings_sf Optional `sf` POLYGON of OSM buildings (output
#'   of [fetch_osm_buildings()]). Required when `buildings_sf` is
#'   user-provided and lacks a `building` column.
#' @param remove_tags Character vector of OSM `building=*` tag values to
#'   exclude. Defaults to common non-residential types.
#' @param keep_untagged Logical. When intersecting user footprints with
#'   OSM, keep buildings that have no OSM match? Default `TRUE` (assumes
#'   unlabeled buildings are residential).
#' @return An `sf` POLYGON of filtered buildings.
#' @export
#' @examples
#' \dontrun{
#' # OSM-only
#' buildings <- fetch_osm_buildings(area) |> filter_buildings()
#'
#' # User footprints + OSM labeling
#' osm <- fetch_osm_buildings(area)
#' filtered <- filter_buildings(my_buildings, osm_buildings_sf = osm)
#' }
filter_buildings <- function(
  buildings_sf,
  osm_buildings_sf = NULL,
  remove_tags = c(
    "hospital",
    "school",
    "church",
    "mosque",
    "industrial",
    "commercial",
    "warehouse",
    "government",
    "public"
  ),
  keep_untagged = TRUE
) {
  checkmate::assert_class(buildings_sf, "sf")
  checkmate::assert_character(remove_tags, min.len = 1L)
  checkmate::assert_flag(keep_untagged)

  has_building_col <- "building" %in% names(buildings_sf)

  # Path A: OSM-only
  if (has_building_col && is.null(osm_buildings_sf)) {
    result <- buildings_sf |>
      dplyr::filter(!(.data$building %in% remove_tags))
    if (nrow(result) == 0L) {
      cli::cli_warn("All buildings were filtered out.")
    }
    return(result)
  }

  # Path B: User footprints + OSM

  if (!is.null(osm_buildings_sf)) {
    checkmate::assert_class(osm_buildings_sf, "sf")

    buildings_crs <- sf::st_crs(buildings_sf)
    osm_crs <- sf::st_crs(osm_buildings_sf)
    if (buildings_crs != osm_crs) {
      buildings_sf <- sf::st_transform(
        buildings_sf,
        sf::st_crs(osm_buildings_sf)
      )
    }

    sf::st_agr(buildings_sf) <- "constant"
    osm_sub <- osm_buildings_sf |> dplyr::select("osm_id", "building")
    sf::st_agr(osm_sub) <- "constant"
    joined <- suppressWarnings(sf::st_join(
      buildings_sf,
      osm_sub,
      join = sf::st_intersects,
      largest = TRUE
    ))

    joined$osm_building_tag <- joined$building
    if ("building" %in% names(buildings_sf)) {
      joined <- joined |>
        dplyr::select(-"building")
    }

    has_match <- !is.na(joined$osm_building_tag)
    is_excluded <- joined$osm_building_tag %in% remove_tags

    keep <- rep(TRUE, nrow(joined))
    keep[has_match & is_excluded] <- FALSE
    if (!keep_untagged) {
      keep[!has_match] <- FALSE
    }

    result <- joined[keep, ]
    if (nrow(result) == 0L) {
      cli::cli_warn("All buildings were filtered out.")
    }
    return(result)
  }

  # Path C: User footprints without OSM

  cli::cli_warn(c(
    "No OSM data provided and no {.field building} column found.",
    "i" = "Returning all buildings as-is."
  ))
  buildings_sf
}


#' Crop buildings to community polygons
#'
#' Intersects building footprints with community polygons, converts to
#' centroids, and returns a named list (one `sf` POINT per community).
#' Points are sorted deterministically by longitude then latitude.
#'
#' @param buildings_sf An `sf` POLYGON of building footprints.
#' @param communities_sf An `sf` POLYGON/MULTIPOLYGON with one row per
#'   community.
#' @param community_id_col Character. Name of the column in
#'   `communities_sf` that uniquely identifies each community.
#' @return A named list of `sf` POINT objects (keyed by community name).
#'   Each has columns: `id` (integer), `community` (character), and
#'   `geometry` (sfc_POINT). If `buildings_sf` has an `osm_id` column,
#'   it is preserved.
#' @export
#' @examples
#' \dontrun{
#' buildings_list <- buildings |>
#'   crop_buildings(communities, community_id_col = "name")
#' }
crop_buildings <- function(
  buildings_sf,
  communities_sf,
  community_id_col = "name"
) {
  checkmate::assert_class(buildings_sf, "sf")
  checkmate::assert_class(communities_sf, "sf")
  checkmate::assert_string(community_id_col)
  checkmate::assert_choice(community_id_col, names(communities_sf))

  if (sf::st_crs(buildings_sf) != sf::st_crs(communities_sf)) {
    buildings_sf <- sf::st_transform(
      buildings_sf,
      sf::st_crs(communities_sf)
    )
  }

  keep_cols <- intersect(c("osm_id"), names(buildings_sf))

  sf::st_agr(buildings_sf) <- "constant"
  sf::st_agr(communities_sf) <- "constant"
  cropped <- sf::st_intersection(buildings_sf, communities_sf)

  if (nrow(cropped) == 0L) {
    cli::cli_warn("No buildings intersect any community polygon.")
    return(list())
  }

  cropped$community <- cropped[[community_id_col]]

  centroids <- suppressWarnings(sf::st_centroid(cropped))

  select_cols <- c(keep_cols, "community")
  centroids <- centroids |>
    dplyr::select(dplyr::all_of(select_cols))

  coords <- sf::st_coordinates(centroids)
  centroids <- centroids |>
    dplyr::mutate(
      .lon = coords[, 1L],
      .lat = coords[, 2L]
    ) |>
    dplyr::arrange(.data$community, .data$.lon, .data$.lat) |>
    dplyr::select(-".lon", -".lat")

  community_names <- sort(unique(centroids$community))
  result <- list()

  for (nm in community_names) {
    pts <- centroids[centroids$community == nm, ]
    pts$id <- seq_len(nrow(pts))
    result[[nm]] <- pts
  }

  result
}


#' Find starting points closest to roads
#'
#' For each community, identifies the building closest to an OSM road.
#' This provides a deterministic, reproducible starting point for the
#' sampling algorithm.
#'
#' @param buildings_list Named list of `sf` POINT objects (output of
#'   [crop_buildings()]).
#' @param road_types Character vector of OSM `highway=*` values to
#'   consider as roads.
#' @return A named integer vector of starting point `id` values, one per
#'   community.
#' @export
#' @examples
#' \dontrun{
#' starts <- find_start_points(buildings_list)
#' # c(community_one = 17, community_two = 78)
#' }
find_start_points <- function(
  buildings_list,
  road_types = c(
    "primary",
    "secondary",
    "tertiary",
    "residential",
    "trunk",
    "unclassified"
  )
) {
  checkmate::assert_list(buildings_list, types = "sf", min.len = 1L)
  checkmate::assert_character(road_types, min.len = 1L)

  start_ids <- integer(length(buildings_list))
  names(start_ids) <- names(buildings_list)

  for (nm in names(buildings_list)) {
    pts <- buildings_list[[nm]]

    if (nrow(pts) == 1L) {
      start_ids[[nm]] <- pts$id[1L]
      next
    }

    utm_crs <- auto_utm_crs(pts)
    pts_utm <- sf::st_transform(pts, utm_crs)
    bbox <- sf::st_bbox(sf::st_transform(pts, 4326L))

    roads <- tryCatch(
      {
        osm <- osmdata::opq(bbox = bbox) |>
          osmdata::add_osm_feature(
            key = "highway",
            value = road_types
          ) |>
          osmdata::osmdata_sf()
        osm$osm_lines
      },
      error = function(e) {
        cli::cli_warn(
          "OSM road query failed for {.val {nm}}: {e$message}"
        )
        NULL
      }
    )

    if (is.null(roads) || nrow(roads) == 0L) {
      bbox_expanded <- bbox + c(-0.005, -0.005, 0.005, 0.005)
      roads <- tryCatch(
        {
          osm <- osmdata::opq(bbox = bbox_expanded) |>
            osmdata::add_osm_feature(
              key = "highway",
              value = road_types
            ) |>
            osmdata::osmdata_sf()
          osm$osm_lines
        },
        error = function(e) NULL
      )
    }

    if (is.null(roads) || nrow(roads) == 0L) {
      cli::cli_warn(c(
        "No roads found near {.val {nm}}.",
        "i" = "Falling back to centroid-nearest building."
      ))
      community_centroid <- sf::st_centroid(
        sf::st_union(sf::st_transform(pts, utm_crs))
      )
      dists <- sf::st_distance(pts_utm, community_centroid)
      idx <- which.min(dists[, 1L])
      start_ids[[nm]] <- pts$id[idx]
      next
    }

    roads_utm <- sf::st_transform(roads, utm_crs)
    nearest_road_idx <- sf::st_nearest_feature(pts_utm, roads_utm)
    dists <- sf::st_distance(
      pts_utm,
      roads_utm[nearest_road_idx, ],
      by_element = TRUE
    )
    min_dist <- min(dists)
    ties <- which(dists == min_dist)
    idx <- ties[which.min(pts$id[ties])]
    start_ids[[nm]] <- pts$id[idx]
  }

  start_ids
}


#' Select sample points with minimum distance constraint
#'
#' Core sampling algorithm for a single community. Selects `n_required`
#' points from candidate buildings such that no two selected points are
#' closer than `min_distance` meters. Does NOT manage its own RNG seed;
#' the caller ([sample_communities()]) wraps the loop in
#' [withr::with_seed()].
#'
#' @param points_sf An `sf` POINT for one community (element of
#'   [crop_buildings()] output).
#' @param n_required Integer, number of points to select.
#' @param start_id Integer, the `id` of the starting point.
#' @param min_distance Numeric, minimum distance in meters between any
#'   two selected points. Default `50`.
#' @return A list with `$primary` (`sf` POINT of selected points with
#'   added `selection_order` column) and `$secondary` (`sf` POINT of
#'   all remaining points).
#' @noRd
select_sample_points <- function(
  points_sf,
  n_required,
  start_id,
  min_distance = 50
) {
  checkmate::assert_class(points_sf, "sf")
  checkmate::assert_int(n_required, lower = 1L)
  checkmate::assert_number(min_distance, lower = 0)

  if (n_required > nrow(points_sf)) {
    cli::cli_abort(
      "Requested {n_required} points but only {nrow(points_sf)} available."
    )
  }

  if (!start_id %in% points_sf$id) {
    cli::cli_abort("Start ID {start_id} not found in points.")
  }

  utm_crs <- auto_utm_crs(points_sf)
  pts_utm <- sf::st_transform(points_sf, utm_crs)

  start_row <- which(pts_utm$id == start_id)
  selected_idx <- start_row
  remaining_idx <- setdiff(seq_len(nrow(pts_utm)), start_row)

  while (length(selected_idx) < n_required && length(remaining_idx) > 0L) {
    sel_geom <- sf::st_geometry(pts_utm[selected_idx, ])
    rem_geom <- sf::st_geometry(pts_utm[remaining_idx, ])

    within_dist <- sf::st_is_within_distance(
      rem_geom,
      sel_geom,
      dist = min_distance
    )
    too_close <- vapply(within_dist, function(x) length(x) > 0L, logical(1L))
    candidate_mask <- !too_close

    if (!any(candidate_mask)) {
      n_still_needed <- n_required - length(selected_idx)
      cli::cli_inform(c(
        "!" = paste0(
          "No candidates beyond {min_distance}m in this community. ",
          "Drawing {n_still_needed} remaining point{?s} randomly."
        )
      ))
      drawn <- sample(remaining_idx, min(n_still_needed, length(remaining_idx)))
      selected_idx <- c(selected_idx, drawn)
      remaining_idx <- setdiff(remaining_idx, drawn)
      next
    }

    candidate_positions <- which(candidate_mask)
    pick <- sample(candidate_positions, 1L)
    actual_idx <- remaining_idx[pick]
    selected_idx <- c(selected_idx, actual_idx)
    remaining_idx <- remaining_idx[-pick]
  }

  primary <- sf::st_transform(pts_utm[selected_idx, ], 4326L)
  primary$selection_order <- seq_len(nrow(primary))

  secondary <- if (length(remaining_idx) > 0L) {
    sf::st_transform(pts_utm[remaining_idx, ], 4326L)
  } else {
    pts_utm[integer(0L), ] |> sf::st_transform(4326L)
  }

  list(primary = primary, secondary = secondary)
}


#' Sample buildings across communities
#'
#' Top-level function that orchestrates reproducible spatial sampling
#' across all communities. Uses minimum-distance constraints and
#' deterministic starting points (closest building to a road).
#'
#' @param buildings_list Named list of `sf` POINT objects (output of
#'   [crop_buildings()]).
#' @param n_required Named integer vector of required sample sizes per
#'   community. Names must match `buildings_list`. A single unnamed
#'   integer applies the same size to all communities.
#' @param start_points Optional named integer vector of starting point
#'   IDs. If `NULL`, computed automatically via [find_start_points()].
#' @param min_distance Numeric, minimum distance in meters between any
#'   two selected points. Default `50`.
#' @param seed Integer RNG seed for reproducibility. Default `250292L`.
#'   Results are reproducible across machines given the same seed, input
#'   data, and R version (>= 3.6.0). R 3.6.0 changed the default
#'   sampling algorithm (`sample.kind = "Rejection"`), so results from
#'   R < 3.6 and R >= 3.6 will differ even with the same seed.
#' @param road_types Character vector passed to [find_start_points()]
#'   when `start_points` is `NULL`.
#' @return A named list of lists. Each community element contains:
#'   `$buildings` (all candidates), `$primary` (selected points),
#'   `$secondary` (remaining points), `$start_id`, `$min_distance`,
#'   and `$seed`.
#' @export
#' @examples
#' \dontrun{
#' samples <- sample_communities(
#'   buildings_list,
#'   n_required = c(community_one = 30, community_two = 80),
#'   min_distance = 50
#' )
#' }
sample_communities <- function(
  buildings_list,
  n_required,
  start_points = NULL,
  min_distance = 50,
  seed = 250292L,
  road_types = c(
    "primary",
    "secondary",
    "tertiary",
    "residential",
    "trunk",
    "unclassified"
  )
) {
  checkmate::assert_list(buildings_list, types = "sf", min.len = 1L)
  checkmate::assert_number(min_distance, lower = 0)
  checkmate::assert_int(seed)

  community_names <- names(buildings_list)

  if (length(n_required) == 1L && is.null(names(n_required))) {
    n_required <- rep(as.integer(n_required), length(community_names))
    names(n_required) <- community_names
  }

  checkmate::assert_integerish(
    n_required,
    lower = 1L,
    names = "named"
  )

  missing_names <- setdiff(names(n_required), community_names)
  if (length(missing_names) > 0L) {
    cli::cli_abort(
      "Communities not found in {.arg buildings_list}: {missing_names}"
    )
  }

  extra_names <- setdiff(community_names, names(n_required))
  if (length(extra_names) > 0L) {
    cli::cli_abort(
      "Communities missing from {.arg n_required}: {extra_names}"
    )
  }

  for (nm in names(n_required)) {
    n_available <- nrow(buildings_list[[nm]])
    if (n_required[[nm]] > n_available) {
      cli::cli_abort(
        "{.val {nm}}: requested {n_required[[nm]]} but only {n_available} buildings available."
      )
    }
  }

  if (is.null(start_points)) {
    cli::cli_inform("Computing starting points (closest to road)...")
    start_points <- find_start_points(buildings_list, road_types)
  }

  checkmate::assert_integerish(
    start_points,
    names = "named",
    len = length(community_names)
  )

  sorted_names <- sort(community_names)

  cli::cli_inform(
    "Sampling {length(sorted_names)} communit{?y/ies} with seed {seed}..."
  )

  results <- withr::with_seed(seed, {
    res <- list()
    for (nm in sorted_names) {
      cli::cli_inform("  Sampling {.val {nm}} ({n_required[[nm]]} points)...")
      sampled <- select_sample_points(
        buildings_list[[nm]],
        n_required[[nm]],
        start_points[[nm]],
        min_distance
      )
      res[[nm]] <- list(
        buildings = buildings_list[[nm]],
        primary = sampled$primary,
        secondary = sampled$secondary,
        start_id = start_points[[nm]],
        min_distance = min_distance,
        seed = seed
      )
    }
    res
  })

  results
}
