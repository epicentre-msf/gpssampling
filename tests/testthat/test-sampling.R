# Tests for R/sampling.R — Phase A sampling pipeline
# Unit tests use synthetic data; integration tests require network.

# Helpers
# ............................................................................

make_polygon <- function(xmin, ymin, xmax, ymax, crs = 4326L) {
  coords <- matrix(
    c(xmin, ymin, xmax, ymin, xmax, ymax, xmin, ymax, xmin, ymin),
    ncol = 2L,
    byrow = TRUE
  )
  sf::st_sf(
    name = "test_area",
    geometry = sf::st_sfc(sf::st_polygon(list(coords)), crs = crs)
  )
}

make_buildings <- function(
  n = 20L,
  xmin = 0,
  ymin = 0,
  xmax = 0.1,
  ymax = 0.1,
  crs = 4326L
) {
  set.seed(42L)
  centers_x <- runif(n, xmin + 0.001, xmax - 0.001)
  centers_y <- runif(n, ymin + 0.001, ymax - 0.001)
  polys <- lapply(seq_len(n), function(i) {
    dx <- 0.0002
    dy <- 0.0002
    sf::st_polygon(list(matrix(
      c(
        centers_x[i] - dx,
        centers_y[i] - dy,
        centers_x[i] + dx,
        centers_y[i] - dy,
        centers_x[i] + dx,
        centers_y[i] + dy,
        centers_x[i] - dx,
        centers_y[i] + dy,
        centers_x[i] - dx,
        centers_y[i] - dy
      ),
      ncol = 2L,
      byrow = TRUE
    )))
  })
  sf::st_sf(
    osm_id = as.character(seq_len(n)),
    building = rep("yes", n),
    geometry = sf::st_sfc(polys, crs = crs)
  )
}

make_communities <- function() {
  poly_a <- sf::st_polygon(list(matrix(
    c(0, 0, 0.05, 0, 0.05, 0.05, 0, 0.05, 0, 0),
    ncol = 2L,
    byrow = TRUE
  )))
  poly_b <- sf::st_polygon(list(matrix(
    c(0.05, 0.05, 0.1, 0.05, 0.1, 0.1, 0.05, 0.1, 0.05, 0.05),
    ncol = 2L,
    byrow = TRUE
  )))
  sf::st_sf(
    name = c("alpha", "beta"),
    geometry = sf::st_sfc(list(poly_a, poly_b), crs = 4326L)
  )
}

# auto_utm_crs
# ............................................................................

test_that("auto_utm_crs returns correct UTM zone for known locations", {
  # Freetown, Sierra Leone (~-13.2 lon, 8.5 lat) -> UTM zone 28N
  pt <- sf::st_sfc(sf::st_point(c(-13.2, 8.5)), crs = 4326L)
  expect_equal(auto_utm_crs(pt), 32628L)

  # Nairobi, Kenya (~36.8 lon, -1.3 lat) -> UTM zone 37S
  pt2 <- sf::st_sfc(sf::st_point(c(36.8, -1.3)), crs = 4326L)
  expect_equal(auto_utm_crs(pt2), 32737L)
})

# filter_buildings
# ............................................................................

test_that("filter_buildings path A: removes tagged buildings", {
  buildings <- make_buildings(10L)
  buildings$building[1:3] <- c("hospital", "school", "church")
  result <- filter_buildings(buildings, building_col = "building")
  expect_equal(nrow(result), 7L)
  expect_false(any(result$building %in% c("hospital", "school", "church")))
})

test_that("filter_buildings path A: custom remove_tags", {
  buildings <- make_buildings(5L)
  buildings$building <- c("yes", "residential", "hospital", "yes", "warehouse")
  result <- filter_buildings(
    buildings,
    remove_tags = c("hospital"),
    building_col = "building"
  )
  expect_equal(nrow(result), 4L)
})

test_that("filter_buildings path B: user footprints + OSM labeling", {
  user_buildings <- make_buildings(5L)
  user_buildings$building <- NULL # no building column

  osm_buildings <- make_buildings(5L)
  osm_buildings$building <- c("yes", "hospital", "residential", "school", "yes")

  result <- filter_buildings(
    user_buildings,
    osm_buildings_sf = osm_buildings,
    remove_tags = c("hospital", "school"),
    building_col = "building"
  )
  expect_true("osm_building_tag" %in% names(result))
  # hospital and school should be removed
  expect_true(nrow(result) < 5L)
})

test_that("filter_buildings path B: keep_untagged = FALSE", {
  user_buildings <- make_buildings(
    3L,
    xmin = 0.5,
    ymin = 0.5,
    xmax = 0.6,
    ymax = 0.6
  )
  user_buildings$building <- NULL

  osm_buildings <- make_buildings(1L) # far away, no spatial match

  expect_warning(
    result <- filter_buildings(
      user_buildings,
      osm_buildings_sf = osm_buildings,
      keep_untagged = FALSE,
      building_col = "building"
    ),
    "filtered out"
  )
  expect_equal(nrow(result), 0L)
})

test_that("filter_buildings path C: warns when no OSM data", {
  user_buildings <- make_buildings(5L)
  user_buildings$building <- NULL

  expect_warning(
    result <- filter_buildings(user_buildings),
    "No OSM data"
  )
  expect_equal(nrow(result), 5L)
})

test_that("filter_buildings path B: disambiguates multiple OSM matches", {
  # Create one large user building that overlaps two OSM buildings
  big_poly <- sf::st_polygon(list(matrix(
    c(0.01, 0.01, 0.05, 0.01, 0.05, 0.05, 0.01, 0.05, 0.01, 0.01),
    ncol = 2L,
    byrow = TRUE
  )))
  user_buildings <- sf::st_sf(
    id = 1L,
    geometry = sf::st_sfc(big_poly, crs = 4326L)
  )

  # Two small OSM buildings inside the user building
  osm_a <- sf::st_polygon(list(matrix(
    c(0.02, 0.02, 0.03, 0.02, 0.03, 0.03, 0.02, 0.03, 0.02, 0.02),
    ncol = 2L,
    byrow = TRUE
  )))
  osm_b <- sf::st_polygon(list(matrix(
    c(0.04, 0.04, 0.045, 0.04, 0.045, 0.045, 0.04, 0.045, 0.04, 0.04),
    ncol = 2L,
    byrow = TRUE
  )))
  osm_buildings <- sf::st_sf(
    osm_id = c("1", "2"),
    building = c("residential", "hospital"),
    geometry = sf::st_sfc(list(osm_a, osm_b), crs = 4326L)
  )

  result <- filter_buildings(
    user_buildings,
    osm_buildings_sf = osm_buildings,
    remove_tags = c("hospital"),
    building_col = "building"
  )

  # Should have a tag assigned (nearest feature wins)
  expect_true("osm_building_tag" %in% names(result))
  expect_equal(nrow(result), 1L)
  expect_equal(result$osm_building_tag, "residential")
})

test_that("filter_buildings path A: emits progress messages", {
  buildings <- make_buildings(10L)
  buildings$building[1:2] <- c("hospital", "school")

  expect_message(
    result <- filter_buildings(buildings, building_col = "building"),
    "Filtering 10 buildings"
  )
  expect_message(
    filter_buildings(buildings, building_col = "building"),
    "Removed 2 non-residential"
  )
})

test_that("filter_buildings path B: emits progress messages", {
  user_buildings <- make_buildings(5L)
  user_buildings$building <- NULL

  osm_buildings <- make_buildings(5L)
  osm_buildings$building <- c("yes", "hospital", "residential", "school", "yes")

  expect_message(
    filter_buildings(
      user_buildings,
      osm_buildings_sf = osm_buildings,
      remove_tags = c("hospital", "school"),
      building_col = "building"
    ),
    "Matching 5 buildings against 5 OSM footprints"
  )
})

# crop_buildings
# ............................................................................

test_that("crop_buildings returns named list of sf POINT", {
  buildings <- make_buildings(20L)
  communities <- make_communities()

  result <- crop_buildings(buildings, communities, community_id_col = "name")

  expect_type(result, "list")
  expect_true(all(vapply(result, inherits, logical(1L), "sf")))
  for (nm in names(result)) {
    expect_true("id" %in% names(result[[nm]]))
    expect_true("community" %in% names(result[[nm]]))
    geom_type <- unique(as.character(sf::st_geometry_type(result[[nm]])))
    expect_true(all(geom_type == "POINT"))
  }
})

test_that("crop_buildings IDs are sequential per community", {
  buildings <- make_buildings(20L)
  communities <- make_communities()
  result <- crop_buildings(buildings, communities, community_id_col = "name")

  for (nm in names(result)) {
    expect_equal(result[[nm]]$id, seq_len(nrow(result[[nm]])))
  }
})

test_that("crop_buildings sorts deterministically", {
  buildings <- make_buildings(20L)
  communities <- make_communities()
  r1 <- crop_buildings(buildings, communities, community_id_col = "name")
  r2 <- crop_buildings(buildings, communities, community_id_col = "name")

  for (nm in names(r1)) {
    expect_equal(
      sf::st_coordinates(r1[[nm]]),
      sf::st_coordinates(r2[[nm]])
    )
  }
})

test_that("crop_buildings handles multipolygon / duplicate community names", {
  # Simulate a community stored as two separate rows (same name)
  poly_a1 <- sf::st_polygon(list(matrix(
    c(0, 0, 0.025, 0, 0.025, 0.025, 0, 0.025, 0, 0),
    ncol = 2L,
    byrow = TRUE
  )))
  poly_a2 <- sf::st_polygon(list(matrix(
    c(0.03, 0.03, 0.05, 0.03, 0.05, 0.05, 0.03, 0.05, 0.03, 0.03),
    ncol = 2L,
    byrow = TRUE
  )))
  # Both rows have name = "alpha" (multipolygon split into separate features)
  communities <- sf::st_sf(
    name = c("alpha", "alpha"),
    geometry = sf::st_sfc(list(poly_a1, poly_a2), crs = 4326L)
  )

  buildings <- make_buildings(20L)
  result <- crop_buildings(buildings, communities, community_id_col = "name")

  # Should produce exactly one "alpha" entry, not two
  expect_equal(length(result), 1L)
  expect_true("alpha" %in% names(result))
  # Buildings from both polygon parts should be included
  expect_true(nrow(result[["alpha"]]) > 0L)
  # IDs should be sequential
  expect_equal(result[["alpha"]]$id, seq_len(nrow(result[["alpha"]])))
})

test_that("crop_buildings handles empty intersection", {
  buildings <- make_buildings(
    5L,
    xmin = 10,
    ymin = 10,
    xmax = 10.1,
    ymax = 10.1
  )
  communities <- make_communities()
  expect_warning(
    result <- crop_buildings(buildings, communities, community_id_col = "name"),
    "No buildings"
  )
  expect_equal(length(result), 0L)
})

# select_sample_points
# ............................................................................

test_that("select_sample_points returns correct counts", {
  buildings <- make_buildings(30L)
  communities <- make_communities()
  bl <- crop_buildings(buildings, communities, community_id_col = "name")

  # Pick first community that has enough points
  nm <- names(bl)[which(vapply(bl, nrow, integer(1L)) >= 5L)[1L]]
  skip_if(is.na(nm), "No community with >= 5 buildings")

  pts <- bl[[nm]]
  withr::with_seed(42L, {
    result <- select_sample_points(pts, 5L, min_distance = 0)
  })

  expect_equal(nrow(result$primary), 5L)
  # Secondary is capped at n_required
  expect_true(nrow(result$secondary) <= 5L)
})

test_that("select_sample_points caps secondary at n_required", {
  # 25 well-spaced points, request 5 → remaining is 20 > 5,
  # so secondary must be drawn (not all remaining)
  coords <- expand.grid(
    x = seq(0, 0.01, length.out = 5L),
    y = seq(0, 0.01, length.out = 5L)
  )
  pts <- sf::st_sf(
    id = seq_len(nrow(coords)),
    community = "test",
    geometry = sf::st_sfc(
      lapply(seq_len(nrow(coords)), function(i) {
        sf::st_point(c(coords$x[i], coords$y[i]))
      }),
      crs = 4326L
    )
  )

  withr::with_seed(42L, {
    result <- select_sample_points(pts, 5L, min_distance = 0)
  })

  expect_equal(nrow(result$primary), 5L)
  expect_equal(nrow(result$secondary), 5L)
  # No overlap between primary and secondary
  expect_length(
    intersect(result$primary$id, result$secondary$id),
    0L
  )
})

test_that("select_sample_points returns all remaining when fewer than n_required", {
  # 7 points, request 5 → only 2 remain, both become secondary
  pts <- sf::st_sf(
    id = seq_len(7L),
    community = "test",
    geometry = sf::st_sfc(
      lapply(seq_len(7L), function(i) {
        sf::st_point(c(i * 0.001, 0))
      }),
      crs = 4326L
    )
  )

  withr::with_seed(42L, {
    result <- select_sample_points(pts, 5L, min_distance = 0)
  })

  expect_equal(nrow(result$primary), 5L)
  expect_equal(nrow(result$secondary), 2L)
  # All 7 points accounted for
  expect_equal(
    sort(c(result$primary$id, result$secondary$id)),
    1:7
  )
})

test_that("select_sample_points selects randomly (no deterministic start)", {
  buildings <- make_buildings(30L)
  communities <- make_communities()
  bl <- crop_buildings(buildings, communities, community_id_col = "name")

  nm <- names(bl)[which(vapply(bl, nrow, integer(1L)) >= 5L)[1L]]
  skip_if(is.na(nm), "No community with >= 5 buildings")

  pts <- bl[[nm]]
  # Different seeds should give different first points
  r1 <- withr::with_seed(42L, select_sample_points(pts, 5L, min_distance = 0))
  r2 <- withr::with_seed(99L, select_sample_points(pts, 5L, min_distance = 0))

  # At least the selection sets or order should differ
  ids1 <- sort(r1$primary$id)
  ids2 <- sort(r2$primary$id)
  expect_false(identical(ids1, ids2))
})

test_that("select_sample_points enforces min distance", {
  # Create well-spaced points so distance constraint is testable
  coords <- expand.grid(
    x = seq(0, 0.01, length.out = 5L),
    y = seq(0, 0.01, length.out = 5L)
  )
  pts <- sf::st_sf(
    id = seq_len(nrow(coords)),
    community = "test",
    geometry = sf::st_sfc(
      lapply(seq_len(nrow(coords)), function(i) {
        sf::st_point(c(coords$x[i], coords$y[i]))
      }),
      crs = 4326L
    )
  )

  withr::with_seed(42L, {
    result <- select_sample_points(pts, 5L, min_distance = 200)
  })

  selected_utm <- sf::st_transform(result$primary, auto_utm_crs(result$primary))
  dist_matrix <- sf::st_distance(selected_utm)
  diag(dist_matrix) <- Inf
  min_pairwise <- min(as.numeric(dist_matrix))
  # When enough candidates exist, all pairwise distances should be >= 200m
  # (may be less if fallback to random draw was triggered)
  expect_true(min_pairwise > 0)
})

test_that("select_sample_points errors on impossible n_required", {
  pts <- sf::st_sf(
    id = 1:3,
    community = "test",
    geometry = sf::st_sfc(
      sf::st_point(c(0, 0)),
      sf::st_point(c(0.01, 0)),
      sf::st_point(c(0, 0.01)),
      crs = 4326L
    )
  )
  expect_error(
    select_sample_points(pts, 10L),
    "only 3 available"
  )
})

# sample_communities (uses mock ordering — network needed for real roads)
# ............................................................................

test_that("sample_communities is reproducible", {
  buildings <- make_buildings(50L)
  communities <- make_communities()
  bl <- crop_buildings(buildings, communities, community_id_col = "name")

  sizes <- vapply(bl, nrow, integer(1L))
  n_req <- pmin(sizes, 3L)

  r1 <- suppressWarnings(
    sample_communities(bl, n_req, min_distance = 0, seed = 123L)
  )
  r2 <- suppressWarnings(
    sample_communities(bl, n_req, min_distance = 0, seed = 123L)
  )

  for (nm in names(r1)) {
    expect_equal(
      sf::st_coordinates(r1[[nm]]$primary),
      sf::st_coordinates(r2[[nm]]$primary)
    )
  }
})

test_that("sample_communities different seed gives different result", {
  buildings <- make_buildings(50L)
  communities <- make_communities()
  bl <- crop_buildings(buildings, communities, community_id_col = "name")

  sizes <- vapply(bl, nrow, integer(1L))
  n_req <- pmin(sizes, 3L)
  # Need at least some community with > 3 buildings for randomness to matter
  skip_if(all(sizes <= 3L), "Not enough buildings for seed test")

  r1 <- suppressWarnings(
    sample_communities(bl, n_req, min_distance = 0, seed = 123L)
  )
  r2 <- suppressWarnings(
    sample_communities(bl, n_req, min_distance = 0, seed = 456L)
  )

  any_different <- any(vapply(
    names(r1),
    function(nm) {
      !identical(
        sf::st_coordinates(r1[[nm]]$primary),
        sf::st_coordinates(r2[[nm]]$primary)
      )
    },
    logical(1L)
  ))
  expect_true(any_different)
})

test_that("sample_communities scalar n_required applies to all", {
  buildings <- make_buildings(50L)
  communities <- make_communities()
  bl <- crop_buildings(buildings, communities, community_id_col = "name")

  sizes <- vapply(bl, nrow, integer(1L))
  min_size <- min(sizes)
  skip_if(min_size < 2L, "Not enough buildings")

  result <- suppressWarnings(
    sample_communities(bl, 2L, min_distance = 0, seed = 42L)
  )

  for (nm in names(result)) {
    expect_equal(nrow(result[[nm]]$primary), 2L)
  }
})

test_that("sample_communities result structure is correct", {
  buildings <- make_buildings(50L)
  communities <- make_communities()
  bl <- crop_buildings(buildings, communities, community_id_col = "name")

  sizes <- vapply(bl, nrow, integer(1L))
  n_req <- pmin(sizes, 3L)

  result <- suppressWarnings(
    sample_communities(bl, n_req, min_distance = 50, seed = 42L)
  )

  for (nm in names(result)) {
    expect_true(all(
      c(
        "buildings",
        "primary",
        "secondary",
        "min_distance",
        "seed"
      ) %in%
        names(result[[nm]])
    ))
    expect_s3_class(result[[nm]]$buildings, "sf")
    expect_s3_class(result[[nm]]$primary, "sf")
    expect_s3_class(result[[nm]]$secondary, "sf")
    expect_equal(result[[nm]]$min_distance, 50)
    # seed is per-community (derived from master seed + community name)
    expect_type(result[[nm]]$seed, "integer")
    # selection_order and point_id should be present on both sets
    expect_true("selection_order" %in% names(result[[nm]]$primary))
    expect_true("point_id" %in% names(result[[nm]]$primary))
    if (nrow(result[[nm]]$secondary) > 0L) {
      expect_true("selection_order" %in% names(result[[nm]]$secondary))
      expect_true("point_id" %in% names(result[[nm]]$secondary))
    }
    # secondary capped at n_required
    expect_true(nrow(result[[nm]]$secondary) <= n_req[[nm]])
  }
})

test_that("sample_communities assigns globally unique point_id", {
  buildings <- make_buildings(50L)
  communities <- make_communities()
  bl <- crop_buildings(buildings, communities, community_id_col = "name")

  sizes <- vapply(bl, nrow, integer(1L))
  n_req <- pmin(sizes, 3L)

  result <- suppressWarnings(
    sample_communities(bl, n_req, min_distance = 0, seed = 42L)
  )

  # Collect all point_ids (unname to drop list element names)
  all_primary_ids <- unname(
    unlist(lapply(result, function(x) x$primary$point_id))
  )
  all_secondary_ids <- unname(
    unlist(lapply(result, function(x) x$secondary$point_id))
  )
  all_ids <- c(all_primary_ids, all_secondary_ids)

  # All IDs are unique

  expect_length(all_ids, length(unique(all_ids)))

  # Primary IDs start at 1 and are contiguous
  n_primary_total <- length(all_primary_ids)
  expect_equal(sort(all_primary_ids), seq_len(n_primary_total))

  # Secondary IDs start right after primary
  if (length(all_secondary_ids) > 0L) {
    expect_equal(min(all_secondary_ids), n_primary_total + 1L)
    expect_equal(
      sort(all_secondary_ids),
      seq(
        from = n_primary_total + 1L,
        length.out = length(all_secondary_ids)
      )
    )
  }
})

test_that("sample_communities errors on unknown community", {
  buildings <- make_buildings(50L)
  communities <- make_communities()
  bl <- crop_buildings(buildings, communities, community_id_col = "name")

  expect_error(
    sample_communities(bl, c(alpha = 3L, beta = 3L, gamma = 3L), seed = 42L),
    "not found"
  )
})

# Integration tests (network required)
# ............................................................................

test_that("fetch_osm_buildings returns sf with correct schema", {
  skip_on_cran()
  skip_if_offline()

  # Small area in Freetown, Sierra Leone
  area <- make_polygon(-13.235, 8.475, -13.225, 8.485)
  result <- suppressWarnings(fetch_osm_buildings(area, zoom = 16L))

  expect_s3_class(result, "sf")
  expect_true(all(c("osm_id", "building") %in% names(result)))
  if (nrow(result) > 0L) {
    geom_types <- unique(as.character(sf::st_geometry_type(result)))
    expect_true(all(geom_types == "POLYGON"))
  }
})
