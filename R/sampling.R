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


#' Derive a per-community seed from a master seed and community name
#'
#' Produces a deterministic integer seed unique to each community so that
#' adding or removing a community does not change the selection in others.
#'
#' @param seed Integer master seed.
#' @param name Character community name.
#' @return An integer seed.
#' @noRd
derive_community_seed <- function(seed, name) {
  chars <- utf8ToInt(name)
  name_hash <- sum(chars * seq_along(chars))
  abs(bitwXor(as.integer(seed), as.integer(name_hash %% .Machine$integer.max)))
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
  tile_grid <- suppressWarnings(slippymath::bbox_to_tile_grid(
    bbox,
    zoom = zoom
  ))
  n_tiles <- nrow(tile_grid$tiles)

  cli::cli_inform(
    "Fetching OSM buildings across {n_tiles} tile{?s} at zoom {zoom}..."
  )

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

    cli::cli_inform("  Tile {i}/{n_tiles}...")

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

  n_raw <- sum(vapply(all_buildings, nrow, integer(1L)))
  cli::cli_inform(
    "Merging {n_raw} raw building{?s} from {length(all_buildings)} tile{?s}..."
  )

  buildings_sf <- dplyr::bind_rows(all_buildings) |>
    sf::st_make_valid()

  if (!"osm_id" %in% names(buildings_sf)) {
    buildings_sf$osm_id <- as.character(seq_len(nrow(buildings_sf)))
  }
  if (!"building" %in% names(buildings_sf)) {
    buildings_sf$building <- "yes"
  }

  cli::cli_inform("Clipping buildings to area boundary...")
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
  cli::cli_inform(
    "Done: {nrow(buildings_sf)} unique building{?s} fetched."
  )
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
#' @param remove_tags Character vector of building type values to
#'   exclude. Defaults to common non-residential types.
#' @param keep_untagged Logical. When intersecting user footprints with
#'   OSM, keep buildings that have no OSM match? Default `TRUE` (assumes
#'   unlabeled buildings are residential).
#' @param building_col Character. Name of the column containing
#'   building type tags. Default `"type"`. For OSM data from
#'   [fetch_osm_buildings()], use `"building"`.
#' @return An `sf` POLYGON of filtered buildings.
#' @export
#' @examples
#' \dontrun{
#' # OSM-only
#' buildings <- fetch_osm_buildings(area) |>
#'   filter_buildings(building_col = "building")
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
  keep_untagged = TRUE,
  building_col = "type"
) {
  checkmate::assert_class(buildings_sf, "sf")
  checkmate::assert_character(remove_tags, min.len = 1L)
  checkmate::assert_flag(keep_untagged)
  checkmate::assert_string(building_col)

  n_input <- nrow(buildings_sf)
  has_building_col <- building_col %in% names(buildings_sf)

  # Path A: Direct filtering (buildings_sf has the type column)
  if (has_building_col && is.null(osm_buildings_sf)) {
    cli::cli_inform(
      "Filtering {n_input} building{?s} by {.field {building_col}} column..."
    )
    result <- buildings_sf |>
      dplyr::filter(!(.data[[building_col]] %in% remove_tags))
    n_removed <- n_input - nrow(result)
    cli::cli_inform(
      "Removed {n_removed} non-residential building{?s}, {nrow(result)} remaining."
    )
    if (nrow(result) == 0L) {
      cli::cli_warn("All buildings were filtered out.")
    }
    return(result)
  }

  # Path B: User footprints + OSM

  if (!is.null(osm_buildings_sf)) {
    checkmate::assert_class(osm_buildings_sf, "sf")
    n_osm <- nrow(osm_buildings_sf)
    cli::cli_inform(
      "Matching {n_input} building{?s} against {n_osm} OSM footprint{?s}..."
    )

    # Determine the type column in the OSM data
    osm_type_col <- if ("building" %in% names(osm_buildings_sf)) {
      "building"
    } else if (building_col %in% names(osm_buildings_sf)) {
      building_col
    } else {
      cli::cli_abort(
        "Column {.val {building_col}} not found in {.arg osm_buildings_sf}."
      )
    }

    buildings_crs <- sf::st_crs(buildings_sf)
    osm_crs <- sf::st_crs(osm_buildings_sf)
    if (buildings_crs != osm_crs) {
      cli::cli_inform("Aligning CRS...")
      buildings_sf <- sf::st_transform(
        buildings_sf,
        sf::st_crs(osm_buildings_sf)
      )
    }

    cli::cli_inform("Validating geometries...")
    buildings_sf <- sf::st_make_valid(buildings_sf)
    osm_buildings_sf <- sf::st_make_valid(osm_buildings_sf)
    sf::st_agr(buildings_sf) <- "constant"
    osm_sub <- osm_buildings_sf |>
      dplyr::select(dplyr::any_of(c("osm_id", osm_type_col)))
    sf::st_agr(osm_sub) <- "constant"

    # Fast spatial matching: sparse intersection predicate + nearest-feature
    # disambiguation. Avoids the expensive st_join(largest = TRUE) which
    # computes full intersection geometries for every pair.
    cli::cli_inform("Computing spatial intersections...")
    pairs <- sf::st_intersects(buildings_sf, osm_sub)
    n_matches <- lengths(pairs)

    osm_tag <- rep(NA_character_, n_input)

    # Single match: direct tag assignment
    single <- n_matches == 1L
    if (any(single)) {
      osm_tag[single] <- osm_sub[[osm_type_col]][
        vapply(pairs[single], `[[`, integer(1L), 1L)
      ]
    }

    # Multiple matches: disambiguate with st_nearest_feature (fast)
    multi <- n_matches > 1L
    if (any(multi)) {
      cli::cli_inform(
        "Disambiguating {sum(multi)} building{?s} with multiple OSM matches..."
      )
      nearest_idx <- sf::st_nearest_feature(
        buildings_sf[multi, ],
        osm_sub
      )
      osm_tag[multi] <- osm_sub[[osm_type_col]][nearest_idx]
    }

    n_matched <- sum(!is.na(osm_tag))
    n_unmatched <- sum(is.na(osm_tag))
    cli::cli_inform(
      "Matched {n_matched} building{?s}, {n_unmatched} unmatched."
    )

    joined <- buildings_sf
    joined$osm_building_tag <- osm_tag

    has_match <- !is.na(joined$osm_building_tag)
    is_excluded <- joined$osm_building_tag %in% remove_tags

    keep <- rep(TRUE, nrow(joined))
    keep[has_match & is_excluded] <- FALSE
    if (!keep_untagged) {
      keep[!has_match] <- FALSE
    }

    result <- joined[keep, ]
    n_removed <- n_input - nrow(result)
    cli::cli_inform(
      "Removed {n_removed} non-residential building{?s}, {nrow(result)} remaining."
    )
    if (nrow(result) == 0L) {
      cli::cli_warn("All buildings were filtered out.")
    }
    return(result)
  }

  # Path C: User footprints without OSM

  cli::cli_warn(c(
    "No OSM data provided and no {.field {building_col}} column found.",
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

  n_buildings <- nrow(buildings_sf)
  n_rows_raw <- nrow(communities_sf)

  if (sf::st_crs(buildings_sf) != sf::st_crs(communities_sf)) {
    cli::cli_inform("Aligning CRS...")
    buildings_sf <- sf::st_transform(
      buildings_sf,
      sf::st_crs(communities_sf)
    )
  }

  # Dissolve communities by ID: a community may span multiple rows
  # (e.g. multipolygon stored as separate features with the same name).
  # sf's summarise method automatically unions geometries per group.
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

  n_communities <- nrow(communities_sf)
  cli::cli_inform(
    "Cropping {n_buildings} building{?s} to {n_communities} communit{?y/ies}..."
  )

  keep_cols <- intersect(c("osm_id"), names(buildings_sf))
  community_names <- sort(communities_sf[[community_id_col]])

  # Fast per-community approach: use st_intersects (spatial index) to find
  # overlapping buildings per community, then compute centroids only for
  # the matched subset. Avoids centroids on ALL buildings + st_join.
  cli::cli_inform("Finding buildings per community (spatial index)...")
  hits_list <- sf::st_intersects(communities_sf, buildings_sf)

  result <- list()

  for (i in seq_along(community_names)) {
    nm <- community_names[i]
    hits <- hits_list[[i]]

    if (length(hits) == 0L) {
      cli::cli_inform("  {.val {nm}}: 0 buildings, skipping.")
      next
    }

    cli::cli_inform(
      "  {.val {nm}}: computing centroids for {length(hits)} building{?s}..."
    )
    subset_sf <- buildings_sf[hits, ]
    centroids <- suppressWarnings(sf::st_centroid(subset_sf))

    # Keep only centroids that actually fall within the community polygon
    community_geom <- sf::st_geometry(communities_sf[i, ])
    within_mask <- as.logical(
      sf::st_within(centroids, community_geom, sparse = FALSE)
    )
    centroids <- centroids[within_mask, ]

    if (nrow(centroids) == 0L) {
      cli::cli_inform("  {.val {nm}}: 0 centroids within boundary, skipping.")
      next
    }

    centroids <- centroids |>
      dplyr::select(dplyr::all_of(keep_cols))
    centroids$community <- nm

    coords <- sf::st_coordinates(centroids)
    centroids <- centroids |>
      dplyr::mutate(
        .lon = coords[, 1L],
        .lat = coords[, 2L]
      ) |>
      dplyr::arrange(.data$.lon, .data$.lat) |>
      dplyr::select(-".lon", -".lat")

    centroids$id <- seq_len(nrow(centroids))
    result[[nm]] <- centroids
  }

  if (length(result) == 0L) {
    cli::cli_warn("No buildings intersect any community polygon.")
    return(list())
  }

  counts <- vapply(result, nrow, integer(1L))
  counts_str <- paste(
    names(counts),
    counts,
    sep = ": ",
    collapse = ", "
  )
  cli::cli_inform("Done: {counts_str}.")

  result
}


#' Find buildings closest to roads
#'
#' For each community, identifies the building closest to an OSM road.
#' This is a standalone utility; the main pipeline uses
#' [order_selected_points()] internally for proximity-based ordering
#' after random selection.
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

  n_communities <- length(buildings_list)
  cli::cli_inform(
    "Finding road-nearest start points for {n_communities} communit{?y/ies}..."
  )

  start_ids <- integer(length(buildings_list))
  names(start_ids) <- names(buildings_list)

  for (nm in names(buildings_list)) {
    pts <- buildings_list[[nm]]
    cli::cli_inform(
      "  {.val {nm}}: querying roads for {nrow(pts)} point{?s}..."
    )

    if (nrow(pts) == 1L) {
      start_ids[[nm]] <- pts$id[1L]
      cli::cli_inform("  {.val {nm}}: single point, using id {pts$id[1L]}.")
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
      cli::cli_inform("  {.val {nm}}: no roads in bbox, expanding search...")
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

    cli::cli_inform(
      "  {.val {nm}}: computing distances to {nrow(roads)} road segment{?s}..."
    )
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
    cli::cli_inform("  {.val {nm}}: start point id {start_ids[[nm]]}.")
  }

  start_ids
}


#' Draw n points from a pool with minimum distance constraint
#'
#' Internal workhorse for [select_sample_points()]. Picks `n_draw`
#' points from `pool_idx` positions in `pts_utm`, enforcing that no
#' two selected points are closer than `min_distance` meters.
#'
#' @param pts_utm An `sf` POINT already projected to a UTM CRS.
#' @param pool_idx Integer vector of row indices to draw from.
#' @param n_draw Integer, number of points to draw.
#' @param min_distance Numeric, minimum distance in meters.
#' @return A list with `$selected` (integer indices) and `$remaining`
#'   (integer indices not selected).
#' @noRd
draw_with_distance <- function(pts_utm, pool_idx, n_draw, min_distance) {
  first <- sample(pool_idx, 1L)
  selected_idx <- first
  remaining_idx <- setdiff(pool_idx, first)

  while (length(selected_idx) < n_draw && length(remaining_idx) > 0L) {
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
      n_still_needed <- n_draw - length(selected_idx)
      cli::cli_warn(c(
        "No candidates beyond {min_distance}m.",
        "i" = "Drawing {n_still_needed} remaining point{?s} randomly (distance constraint relaxed)."
      ))
      drawn <- sample(
        remaining_idx,
        min(n_still_needed, length(remaining_idx))
      )
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

  list(selected = selected_idx, remaining = remaining_idx)
}


#' Select sample points with minimum distance constraint
#'
#' Core sampling algorithm for a single community. Selects `n_required`
#' primary points randomly from candidate buildings such that no two
#' selected points are closer than `min_distance` meters. Then selects
#' up to `n_required` secondary (replacement) points from the remaining
#' pool using the same distance algorithm. If fewer than `n_required`
#' points remain after primary selection, all remaining become secondary.
#'
#' When `joint = TRUE`, primary and secondary points are drawn together
#' in a single pass (up to `2 * n_required`), enforcing the minimum
#' distance across all points. The first `n_required` drawn become
#' primary; the rest become secondary. This produces less clustered
#' secondary points than drawing them separately.
#'
#' All points (including the first) are chosen at random. Does NOT
#' manage its own RNG seed; the caller ([sample_communities()]) wraps
#' the loop in [withr::with_seed()].
#'
#' @param points_sf An `sf` POINT for one community (element of
#'   [crop_buildings()] output).
#' @param n_required Integer, number of points to select.
#' @param min_distance Numeric, minimum distance in meters between any
#'   two selected points. Default `50`.
#' @param joint Logical. If `TRUE`, draw primary and secondary in a
#'   single pass to reduce clustering. Default `FALSE`.
#' @return A list with `$primary` (`sf` POINT of selected points) and
#'   `$secondary` (`sf` POINT of replacement points, at most
#'   `n_required` rows). The primary set has no `selection_order`
#'   column; ordering is done separately by [order_selected_points()].
#' @noRd
select_sample_points <- function(
  points_sf,
  n_required,
  min_distance = 50,
  joint = FALSE
) {
  checkmate::assert_class(points_sf, "sf")
  checkmate::assert_int(n_required, lower = 1L)
  checkmate::assert_number(min_distance, lower = 0)
  checkmate::assert_flag(joint)

  if (n_required > nrow(points_sf)) {
    cli::cli_abort(
      "Requested {n_required} points but only {nrow(points_sf)} available."
    )
  }

  n_total <- nrow(points_sf)
  utm_crs <- auto_utm_crs(points_sf)
  pts_utm <- sf::st_transform(points_sf, utm_crs)
  all_idx <- seq_len(nrow(pts_utm))

  if (joint) {
    # --- Joint draw: primary + secondary in a single pass ---
    n_draw <- min(2L * n_required, n_total)
    cli::cli_inform(
      "    Joint draw: {n_draw}/{n_total} points (min distance: {min_distance}m)..."
    )

    joint_draw <- draw_with_distance(
      pts_utm,
      all_idx,
      n_draw,
      min_distance
    )

    selected <- joint_draw$selected
    n_selected <- length(selected)

    # First n_required become primary, rest become secondary
    n_pri <- min(n_required, n_selected)
    primary_idx <- selected[seq_len(n_pri)]
    secondary_idx <- if (n_selected > n_pri) {
      selected[seq(n_pri + 1L, n_selected)]
    } else {
      integer(0L)
    }

    # Any remaining unselected points go to secondary (pool exhaustion)
    leftover_idx <- joint_draw$remaining
    if (length(secondary_idx) < n_required && length(leftover_idx) > 0L) {
      n_still_needed <- n_required - length(secondary_idx)
      extra <- leftover_idx[seq_len(min(n_still_needed, length(leftover_idx)))]
      secondary_idx <- c(secondary_idx, extra)
    }

    primary <- sf::st_transform(pts_utm[primary_idx, ], 4326L)
    secondary <- sf::st_transform(pts_utm[secondary_idx, ], 4326L)

    cli::cli_inform(
      "    {nrow(primary)} primary + {nrow(secondary)} secondary point{?s} (joint)."
    )

    return(list(primary = primary, secondary = secondary))
  }

  # --- Independent draw (default): primary then secondary separately ---
  cli::cli_inform(
    "    Selecting {n_required}/{n_total} points (min distance: {min_distance}m)..."
  )

  primary_draw <- draw_with_distance(
    pts_utm,
    all_idx,
    n_required,
    min_distance
  )

  cli::cli_inform(
    "    Selected {length(primary_draw$selected)} primary point{?s}."
  )

  primary <- sf::st_transform(pts_utm[primary_draw$selected, ], 4326L)

  # --- Secondary selection (capped at n_required) ---
  leftover_idx <- primary_draw$remaining

  if (length(leftover_idx) == 0L) {
    secondary <- pts_utm[integer(0L), ] |> sf::st_transform(4326L)
  } else if (length(leftover_idx) <= n_required) {
    cli::cli_inform(
      "    All {length(leftover_idx)} remaining point{?s} become secondary."
    )
    secondary <- sf::st_transform(pts_utm[leftover_idx, ], 4326L)
  } else {
    cli::cli_inform(
      "    Selecting {n_required} secondary point{?s} from {length(leftover_idx)} remaining..."
    )
    secondary_draw <- draw_with_distance(
      pts_utm,
      leftover_idx,
      n_required,
      min_distance
    )
    secondary <- sf::st_transform(
      pts_utm[secondary_draw$selected, ],
      4326L
    )
  }

  cli::cli_inform(
    "    {nrow(primary)} primary + {nrow(secondary)} secondary point{?s}."
  )

  list(primary = primary, secondary = secondary)
}


#' Fetch OSM roads for an area with caching and retry
#'
#' Downloads road linestrings from the OSM Overpass API. If
#' `cache_file` is provided and already exists on disk, reads from
#' cache instead of downloading. On a successful download the result
#' is saved to `cache_file` for reuse. If the initial query returns
#' no roads, the bounding box is automatically expanded and retried.
#'
#' @param area An `sf`, `sfc`, or `bbox` object. Its bounding box
#'   defines the Overpass query extent.
#' @param road_types Character vector of OSM `highway=*` values.
#'   Default covers main road categories.
#' @param cache_file Optional file path (`.gpkg`) for disk caching.
#'   If `NULL` (default), no caching.
#' @param timeout Overpass API timeout in seconds. Default `120`.
#' @return An `sf` LINESTRING of roads, or `NULL` if none found.
#' @noRd
fetch_roads <- function(
  area,
  road_types = c(
    "primary",
    "secondary",
    "tertiary",
    "residential",
    "trunk",
    "unclassified"
  ),
  cache_file = NULL,
  timeout = 120
) {
  bbox <- if (inherits(area, "bbox")) {
    area
  } else {
    sf::st_bbox(sf::st_transform(area, 4326L))
  }
  # --- Check cache first ---
  if (!is.null(cache_file) && file.exists(cache_file)) {
    cli::cli_inform("    Loading cached roads from {.path {cache_file}}...")
    roads <- tryCatch(
      sf::st_read(cache_file, quiet = TRUE),
      error = function(e) {
        cli::cli_warn("Failed to read road cache: {e$message}")
        NULL
      }
    )
    if (!is.null(roads) && nrow(roads) > 0L) {
      return(roads)
    }
  }

  # --- Download from Overpass ---
  cli::cli_inform("    Downloading roads from OSM (timeout: {timeout}s)...")
  roads <- tryCatch(
    {
      osm <- osmdata::opq(bbox = bbox, timeout = timeout) |>
        osmdata::add_osm_feature(
          key = "highway",
          value = road_types
        ) |>
        osmdata::osmdata_sf()
      osm$osm_lines
    },
    error = function(e) {
      cli::cli_warn("OSM road query failed: {e$message}")
      NULL
    }
  )

  # --- Retry with expanded bbox if empty ---
  if (is.null(roads) || nrow(roads) == 0L) {
    bbox_expanded <- bbox + c(-0.005, -0.005, 0.005, 0.005)
    cli::cli_inform("    No roads in bbox, retrying with expanded area...")
    roads <- tryCatch(
      {
        osm <- osmdata::opq(
          bbox = bbox_expanded,
          timeout = timeout
        ) |>
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

  # --- Save to cache ---
  if (!is.null(roads) && nrow(roads) > 0L && !is.null(cache_file)) {
    tryCatch(
      {
        fs::dir_create(fs::path_dir(cache_file), recurse = TRUE)
        sf::st_write(roads, cache_file, quiet = TRUE, delete_dsn = TRUE)
        cli::cli_inform("    Cached roads to {.path {cache_file}}.")
      },
      error = function(e) {
        cli::cli_warn("Failed to cache roads: {e$message}")
      }
    )
  }

  roads
}


#' Fetch OSM roads for all communities
#'
#' Downloads and caches road linestrings for each community in a
#' polygon layer. Roads are saved as `.gpkg` files in `road_dir`,
#' one per community, using the community ID as filename. On
#' subsequent calls, cached files are reused without re-downloading.
#'
#' This function can be called independently before
#' [sample_communities()] to pre-download roads (e.g. while online),
#' then pass the same `road_dir` to [sample_communities()] for
#' offline use.
#'
#' @param communities_sf An `sf` POLYGON of community boundaries.
#' @param community_id_col Column name for community ID in
#'   `communities_sf`. Default `"name"`.
#' @param road_dir Directory where road `.gpkg` files are cached.
#'   Created if it does not exist.
#' @param road_types Character vector of OSM `highway=*` values.
#' @param timeout Overpass API timeout in seconds. Default `120`.
#' @return A named list of `sf` LINESTRING objects (one per
#'   community). Communities where no roads were found have `NULL`.
#' @export
#' @examples
#' \dontrun{
#' roads <- fetch_community_roads(
#'   communities,
#'   community_id_col = "name",
#'   road_dir = "output/roads"
#' )
#' }
fetch_community_roads <- function(
  communities_sf,
  community_id_col = "name",
  road_dir,
  road_types = c(
    "primary",
    "secondary",
    "tertiary",
    "residential",
    "trunk",
    "unclassified"
  ),
  timeout = 120
) {
  checkmate::assert_class(communities_sf, "sf")
  checkmate::assert_string(community_id_col)
  checkmate::assert_choice(community_id_col, names(communities_sf))
  checkmate::assert_string(road_dir)

  # Dissolve duplicate rows per community (multipolygon stored as

  # separate features with the same name)
  communities_sf <- communities_sf |>
    dplyr::select(dplyr::all_of(community_id_col)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(community_id_col))) |>
    dplyr::summarise(.groups = "drop")
  sf::st_agr(communities_sf) <- "constant"

  community_names <- communities_sf[[community_id_col]]
  n <- length(community_names)
  cli::cli_inform(
    "Fetching roads for {n} communit{?y/ies} into {.path {road_dir}}..."
  )

  fs::dir_create(road_dir, recurse = TRUE)
  result <- list()

  for (i in seq_along(community_names)) {
    nm <- community_names[i]
    cache_file <- as.character(
      fs::path(road_dir, paste0(nm, ".gpkg"))
    )
    community_row <- communities_sf[i, ]

    cli::cli_inform("  {.val {nm}} ({i}/{n})...")
    result[[nm]] <- fetch_roads(
      community_row,
      road_types = road_types,
      cache_file = cache_file,
      timeout = timeout
    )
  }

  n_ok <- sum(vapply(result, function(x) !is.null(x), logical(1L)))
  cli::cli_inform(
    "Done: {n_ok}/{n} communit{?y/ies} with roads."
  )

  result
}


#' Order selected points by route proximity
#'
#' After random selection, reorders the selected points so field workers
#' walk minimal distances. The ordering starts from the selected point
#' closest to an OSM road, then chains to the nearest unordered point
#' at each step (greedy nearest-neighbour).
#'
#' @param selected_sf An `sf` POINT of selected buildings for one
#'   community (the `$primary` output of [select_sample_points()]).
#' @param road_types Character vector of OSM `highway=*` values.
#' @param roads_sf Optional pre-fetched roads `sf` LINESTRING. If
#'   provided, skips the OSM download entirely.
#' @return The same `sf` reordered with a `selection_order` column
#'   (1 = closest to road, then nearest-neighbour chain).
#' @noRd
order_selected_points <- function(
  selected_sf,
  road_types = c(
    "primary",
    "secondary",
    "tertiary",
    "residential",
    "trunk",
    "unclassified"
  ),
  roads_sf = NULL
) {
  n <- nrow(selected_sf)
  if (n <= 1L) {
    selected_sf$selection_order <- seq_len(n)
    return(selected_sf)
  }

  cli::cli_inform("    Ordering {n} selected point{?s} by road proximity...")

  utm_crs <- auto_utm_crs(selected_sf)
  pts_utm <- sf::st_transform(selected_sf, utm_crs)

  # --- find the selected point closest to a road ---
  roads <- roads_sf
  if (is.null(roads) || nrow(roads) == 0L) {
    bbox <- sf::st_bbox(sf::st_transform(selected_sf, 4326L))
    roads <- fetch_roads(bbox, road_types)
  }

  if (is.null(roads) || nrow(roads) == 0L) {
    cli::cli_warn(c(
      "No roads found for ordering.",
      "i" = "Falling back to centroid-nearest start."
    ))
    centroid <- sf::st_centroid(sf::st_union(pts_utm))
    dists <- as.numeric(sf::st_distance(pts_utm, centroid))
    start_idx <- which.min(dists)
  } else {
    roads_utm <- sf::st_transform(roads, utm_crs)
    nearest_road_idx <- sf::st_nearest_feature(pts_utm, roads_utm)
    dists <- sf::st_distance(
      pts_utm,
      roads_utm[nearest_road_idx, ],
      by_element = TRUE
    )
    min_dist <- min(dists)
    ties <- which(dists == min_dist)
    start_idx <- ties[which.min(selected_sf$id[ties])]
  }

  # --- nearest-neighbour chain from the start point ---
  cli::cli_inform("    Building nearest-neighbour chain...")
  ordered_idx <- integer(n)
  ordered_idx[1L] <- start_idx
  remaining <- setdiff(seq_len(n), start_idx)

  for (i in 2L:n) {
    ordered_geom <- sf::st_geometry(pts_utm[ordered_idx[seq_len(i - 1L)], ])
    rem_geom <- sf::st_geometry(pts_utm[remaining, ])

    dist_mat <- sf::st_distance(rem_geom, ordered_geom)
    min_per_remaining <- apply(dist_mat, 1L, min)
    nearest <- which.min(min_per_remaining)

    ordered_idx[i] <- remaining[nearest]
    remaining <- remaining[-nearest]
  }

  result <- sf::st_transform(pts_utm[ordered_idx, ], 4326L)
  result$selection_order <- seq_len(n)
  result
}


#' Compute pairwise distance statistics for a set of points
#'
#' Returns a named list with min, mean, median distances (meters) and
#' the count of pairs closer than a threshold.
#'
#' @param pts_sf An `sf` POINT object.
#' @param threshold Numeric distance threshold in meters.
#' @return A named list: `min_dist`, `mean_dist`, `median_dist`,
#'   `n_violations` (pairs closer than `threshold`), `n_points`.
#' @noRd
compute_distance_stats <- function(pts_sf, threshold = 50) {
  n <- nrow(pts_sf)
  if (n < 2L) {
    return(list(
      min_dist = NA_real_,
      mean_dist = NA_real_,
      median_dist = NA_real_,
      n_violations = 0L,
      n_points = n
    ))
  }

  utm_crs <- auto_utm_crs(pts_sf)
  pts_utm <- sf::st_transform(pts_sf, utm_crs)
  dmat <- as.numeric(sf::st_distance(pts_utm))
  dmat <- matrix(dmat, nrow = n)
  diag(dmat) <- NA_real_

  # Upper triangle only (avoid double-counting pairs)
  upper_dists <- dmat[upper.tri(dmat)]

  list(
    min_dist = round(min(upper_dists, na.rm = TRUE), 1),
    mean_dist = round(mean(upper_dists, na.rm = TRUE), 1),
    median_dist = round(stats::median(upper_dists, na.rm = TRUE), 1),
    n_violations = sum(upper_dists < threshold, na.rm = TRUE),
    n_points = n
  )
}


#' Sample buildings across communities
#'
#' Top-level function that orchestrates reproducible spatial sampling
#' across all communities. Points are selected randomly with minimum-
#' distance constraints, then reordered by proximity to roads for
#' efficient field work.
#'
#' @param buildings_list Named list of `sf` POINT objects (output of
#'   [crop_buildings()]).
#' @param n_required Named integer vector of required sample sizes per
#'   community. Names must match `buildings_list`. A single unnamed
#'   integer applies the same size to all communities.
#' @param min_distance Numeric, minimum distance in meters between any
#'   two selected points. Default `50`.
#' @param seed Integer RNG seed for reproducibility (**required**, no
#'   default). A per-community seed is derived from `seed` and the
#'   community name, so adding or removing a community does not change
#'   the selection in other communities. Results are reproducible across
#'   machines given the same seed, input data, and R version (>= 3.6.0).
#'   R 3.6.0 changed the default sampling algorithm
#'   (`sample.kind = "Rejection"`), so results from R < 3.6 and
#'   R >= 3.6 will differ even with the same seed.
#' @param joint Logical. If `TRUE`, primary and secondary points are
#'   drawn together in a single pass, enforcing the minimum distance
#'   across both sets. This reduces clustering in the secondary points.
#'   The first `n_required` drawn become primary; the rest become
#'   secondary. Default `FALSE` (independent draws).
#' @param print_table Logical. If `TRUE` (default), prints a
#'   [flextable::flextable()] summary at the end of sampling with
#'   per-community statistics: buildings available, points drawn,
#'   distance metrics, constraint violations, and coverage.
#' @param road_types Character vector of OSM `highway=*` values used
#'   for the post-selection proximity ordering.
#' @param road_dir Optional directory for cached road files. If
#'   provided, roads are read from / saved to
#'   `road_dir/{community_name}.gpkg`. Use [fetch_community_roads()]
#'   to pre-download roads. Default `NULL` (no caching).
#' @return A named list. Each community element contains:
#'   `$buildings` (all candidates), `$primary` (selected points with
#'   `selection_order` and `point_id`), `$secondary` (replacement
#'   points with `selection_order` and `point_id`, at most `n_required`
#'   per community), `$min_distance`, and `$seed`. Both primary and
#'   secondary are ordered by road proximity (nearest-neighbour chain).
#'   The `point_id` column is globally unique across all communities
#'   and sets: primary IDs are numbered 1..N_total_primary, secondary
#'   IDs continue from N_total_primary + 1. When `print_table = TRUE`,
#'   the result carries two attributes: `attr(, "summary_table")` (a
#'   [flextable::flextable()] object ready for rendering in reports)
#'   and `attr(, "summary_df")` (the underlying data frame). Access
#'   via `attr(result, "summary_table")`.
#' @export
#' @examples
#' \dontrun{
#' samples <- sample_communities(
#'   buildings_list,
#'   n_required = c(community_one = 30, community_two = 80),
#'   min_distance = 50,
#'   seed = 12345L
#' )
#'
#' # Joint sampling (less clustered secondary points)
#' samples <- sample_communities(
#'   buildings_list,
#'   n_required = c(community_one = 30, community_two = 80),
#'   min_distance = 50,
#'   seed = 12345L,
#'   joint = TRUE
#' )
#' }
sample_communities <- function(
  buildings_list,
  n_required,
  min_distance = 50,
  seed,
  joint = FALSE,
  print_table = TRUE,
  road_types = c(
    "primary",
    "secondary",
    "tertiary",
    "residential",
    "trunk",
    "unclassified"
  ),
  road_dir = NULL
) {
  checkmate::assert_list(buildings_list, types = "sf", min.len = 1L)
  checkmate::assert_number(min_distance, lower = 0)
  checkmate::assert_int(seed)
  checkmate::assert_flag(joint)
  checkmate::assert_flag(print_table)

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

  sorted_names <- sort(community_names)

  cli::cli_inform(
    "Sampling {length(sorted_names)} communit{?y/ies} with seed {seed}..."
  )

  res <- list()
  for (nm in sorted_names) {
    community_seed <- derive_community_seed(seed, nm)
    cli::cli_inform(
      "  Sampling {.val {nm}} ({n_required[[nm]]} points, seed {community_seed})..."
    )
    sampled <- withr::with_seed(community_seed, {
      select_sample_points(
        buildings_list[[nm]],
        n_required[[nm]],
        min_distance,
        joint = joint
      )
    })
    # Fetch roads once per community (cached if road_dir provided)
    cache_file <- if (!is.null(road_dir)) {
      as.character(fs::path(road_dir, paste0(nm, ".gpkg")))
    }
    roads <- fetch_roads(
      buildings_list[[nm]],
      road_types,
      cache_file = cache_file
    )

    cli::cli_inform("  Ordering {.val {nm}} by road proximity...")
    ordered_primary <- order_selected_points(
      sampled$primary,
      road_types,
      roads_sf = roads
    )
    ordered_secondary <- if (nrow(sampled$secondary) > 0L) {
      order_selected_points(sampled$secondary, road_types, roads_sf = roads)
    } else {
      sampled$secondary
    }
    res[[nm]] <- list(
      buildings = buildings_list[[nm]],
      primary = ordered_primary,
      secondary = ordered_secondary,
      min_distance = min_distance,
      seed = community_seed
    )
  }

  # --- Assign globally unique point_id across all communities ---
  # Primary: 1..N_total_primary (sequentially across communities)
  # Secondary: (N_total_primary + 1).. (sequentially across communities)
  primary_offset <- 0L
  for (nm in sorted_names) {
    n_pri <- nrow(res[[nm]]$primary)
    res[[nm]]$primary$point_id <- seq(
      from = primary_offset + 1L,
      length.out = n_pri
    )
    primary_offset <- primary_offset + n_pri
  }

  secondary_offset <- primary_offset
  for (nm in sorted_names) {
    n_sec <- nrow(res[[nm]]$secondary)
    if (n_sec > 0L) {
      res[[nm]]$secondary$point_id <- seq(
        from = secondary_offset + 1L,
        length.out = n_sec
      )
      secondary_offset <- secondary_offset + n_sec
    }
  }

  cli::cli_inform(
    "Assigned {primary_offset} primary + {secondary_offset - primary_offset} secondary point IDs."
  )

  # --- Summary table ---
  if (print_table) {
    rows <- list()
    for (nm in sorted_names) {
      n_buildings <- nrow(res[[nm]]$buildings)
      n_pri <- nrow(res[[nm]]$primary)
      n_sec <- nrow(res[[nm]]$secondary)
      n_all <- n_pri + n_sec

      pri_stats <- compute_distance_stats(res[[nm]]$primary, min_distance)
      sec_stats <- compute_distance_stats(res[[nm]]$secondary, min_distance)
      all_pts <- dplyr::bind_rows(res[[nm]]$primary, res[[nm]]$secondary)
      all_stats <- compute_distance_stats(all_pts, min_distance)

      rows <- c(
        rows,
        list(data.frame(
          community = nm,
          buildings = n_buildings,
          n_primary = n_pri,
          n_secondary = n_sec,
          n_total = n_all,
          coverage_pct = round(n_all / n_buildings * 100, 1),
          min_dist_requested = min_distance,
          min_dist_primary = pri_stats$min_dist,
          min_dist_secondary = sec_stats$min_dist,
          min_dist_all = all_stats$min_dist,
          mean_dist_all = all_stats$mean_dist,
          median_dist_all = all_stats$median_dist,
          violations = all_stats$n_violations,
          mode = if (joint) "joint" else "independent",
          seed = res[[nm]]$seed,
          stringsAsFactors = FALSE
        ))
      )
    }

    summary_df <- do.call(rbind, rows)

    # Add totals row
    totals <- data.frame(
      community = "TOTAL",
      buildings = sum(summary_df$buildings),
      n_primary = sum(summary_df$n_primary),
      n_secondary = sum(summary_df$n_secondary),
      n_total = sum(summary_df$n_total),
      coverage_pct = round(
        sum(summary_df$n_total) / sum(summary_df$buildings) * 100,
        1
      ),
      min_dist_requested = min_distance,
      min_dist_primary = round(
        min(summary_df$min_dist_primary, na.rm = TRUE),
        1
      ),
      min_dist_secondary = round(
        min(summary_df$min_dist_secondary, na.rm = TRUE),
        1
      ),
      min_dist_all = round(
        min(summary_df$min_dist_all, na.rm = TRUE),
        1
      ),
      mean_dist_all = round(
        mean(summary_df$mean_dist_all, na.rm = TRUE),
        1
      ),
      median_dist_all = round(
        stats::median(summary_df$median_dist_all, na.rm = TRUE),
        1
      ),
      violations = sum(summary_df$violations),
      mode = if (joint) "joint" else "independent",
      seed = NA_integer_,
      stringsAsFactors = FALSE
    )
    summary_df <- rbind(summary_df, totals)

    ft <- flextable::flextable(summary_df) |>
      flextable::set_header_labels(
        community = "Community",
        buildings = "Buildings",
        n_primary = "Primary",
        n_secondary = "Secondary",
        n_total = "Total Pts",
        coverage_pct = "Coverage %",
        min_dist_requested = "Min Dist\nRequested (m)",
        min_dist_primary = "Min Dist\nPrimary (m)",
        min_dist_secondary = "Min Dist\nSecondary (m)",
        min_dist_all = "Min Dist\nAll (m)",
        mean_dist_all = "Mean Dist\nAll (m)",
        median_dist_all = "Median Dist\nAll (m)",
        violations = "Pairs <\nThreshold",
        mode = "Mode",
        seed = "Seed"
      ) |>
      flextable::bold(i = nrow(summary_df)) |>
      flextable::hline(i = nrow(summary_df) - 1L) |>
      flextable::colformat_num(j = "seed", big.mark = "") |>
      flextable::autofit() |>
      flextable::set_caption("Sampling Summary")

    # Highlight violations
    violation_rows <- which(summary_df$violations > 0L)
    if (length(violation_rows) > 0L) {
      ft <- ft |>
        flextable::color(
          i = violation_rows,
          j = "violations",
          color = "red"
        ) |>
        flextable::bold(
          i = violation_rows,
          j = "violations"
        )
    }

    # Highlight min_dist_all below threshold
    below_threshold <- which(
      !is.na(summary_df$min_dist_all) &
        summary_df$min_dist_all < min_distance
    )
    if (length(below_threshold) > 0L) {
      ft <- ft |>
        flextable::color(
          i = below_threshold,
          j = "min_dist_all",
          color = "red"
        ) |>
        flextable::bold(
          i = below_threshold,
          j = "min_dist_all"
        )
    }

    attr(res, "summary_table") <- ft
    attr(res, "summary_df") <- summary_df
  }

  res
}
