# Tests for R/sampling_map.R — Phase C static mapping

# Helpers
# ............................................................................

make_map_data <- function() {
  community <- sf::st_sf(
    name = "test_community",
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(
        c(0, 0, 0.05, 0, 0.05, 0.05, 0, 0.05, 0, 0),
        ncol = 2L,
        byrow = TRUE
      ))),
      crs = 4326L
    )
  )

  pts <- sf::st_sf(
    id = 1:6,
    community = "test_community",
    selection_order = 1:6,
    assigned_batch = rep(1:2, each = 3L),
    geometry = sf::st_sfc(
      lapply(1:6, function(i) sf::st_point(c(i * 0.008, i * 0.008))),
      crs = 4326L
    )
  )

  buffers <- create_buffers(pts, radius = 50)

  list(community = community, points = pts, buffers = buffers)
}

# map_community
# ............................................................................

test_that("map_community returns a ggplot object", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("ggspatial")
  skip_if_not_installed("tidyterra")

  d <- make_map_data()
  p <- map_community(
    "test_community",
    d$community,
    d$points,
    buffers_sf = d$buffers,
    basemap = "OpenStreetMap.HOT"
  )
  expect_s3_class(p, "ggplot")
})

test_that("map_community works without buffers", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("ggspatial")
  skip_if_not_installed("tidyterra")

  d <- make_map_data()
  p <- map_community(
    "test_community",
    d$community,
    d$points,
    buffers_sf = NULL
  )
  expect_s3_class(p, "ggplot")
})

test_that("map_community uniform color mode works", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("ggspatial")
  skip_if_not_installed("tidyterra")

  d <- make_map_data()
  p <- map_community(
    "test_community",
    d$community,
    d$points,
    batch_colors = FALSE
  )
  expect_s3_class(p, "ggplot")
})

# map_overview
# ............................................................................

test_that("map_overview returns a ggplot object", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("ggspatial")
  skip_if_not_installed("tidyterra")

  d <- make_map_data()

  samples_list <- list(
    test_community = list(
      primary = d$points,
      secondary = d$points[1:2, ]
    )
  )

  p <- map_overview(
    samples_list,
    d$community,
    community_id_col = "name",
    set = "primary",
    buffer_radius = 50
  )
  expect_s3_class(p, "ggplot")
})

# map_all_communities
# ............................................................................

test_that("map_all_communities returns named list of ggplots", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("ggspatial")
  skip_if_not_installed("tidyterra")

  d <- make_map_data()

  samples_list <- list(
    test_community = list(
      primary = d$points,
      secondary = d$points[1:2, ]
    )
  )

  result <- map_all_communities(
    samples_list,
    d$community,
    community_id_col = "name",
    buffer_radius = 50
  )

  expect_type(result, "list")
  expect_true("overview" %in% names(result))
  expect_true("test_community_primary" %in% names(result))
  expect_true("test_community_secondary" %in% names(result))
  for (nm in names(result)) {
    expect_s3_class(result[[nm]], "ggplot")
  }
})

# map_cropped_buildings
# ............................................................................

test_that("map_cropped_buildings returns a patchwork object", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")

  community_a <- sf::st_sf(
    name = "alpha",
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(
        c(0, 0, 0.05, 0, 0.05, 0.05, 0, 0.05, 0, 0),
        ncol = 2L,
        byrow = TRUE
      ))),
      crs = 4326L
    )
  )
  community_b <- sf::st_sf(
    name = "beta",
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(
        c(0.05, 0.05, 0.1, 0.05, 0.1, 0.1, 0.05, 0.1, 0.05, 0.05),
        ncol = 2L,
        byrow = TRUE
      ))),
      crs = 4326L
    )
  )
  communities <- rbind(community_a, community_b)

  pts_a <- sf::st_sf(
    id = 1:3,
    community = "alpha",
    geometry = sf::st_sfc(
      sf::st_point(c(0.01, 0.01)),
      sf::st_point(c(0.02, 0.03)),
      sf::st_point(c(0.04, 0.02)),
      crs = 4326L
    )
  )
  pts_b <- sf::st_sf(
    id = 1:2,
    community = "beta",
    geometry = sf::st_sfc(
      sf::st_point(c(0.06, 0.06)),
      sf::st_point(c(0.08, 0.09)),
      crs = 4326L
    )
  )

  buildings_list <- list(alpha = pts_a, beta = pts_b)

  p <- map_cropped_buildings(
    buildings_list,
    communities,
    community_id_col = "name"
  )
  expect_s3_class(p, "patchwork")
})

test_that("map_cropped_buildings handles single community", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("patchwork")

  community <- sf::st_sf(
    name = "solo",
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(
        c(0, 0, 0.05, 0, 0.05, 0.05, 0, 0.05, 0, 0),
        ncol = 2L,
        byrow = TRUE
      ))),
      crs = 4326L
    )
  )

  pts <- sf::st_sf(
    id = 1:2,
    community = "solo",
    geometry = sf::st_sfc(
      sf::st_point(c(0.01, 0.01)),
      sf::st_point(c(0.03, 0.04)),
      crs = 4326L
    )
  )

  p <- map_cropped_buildings(
    list(solo = pts),
    community,
    community_id_col = "name"
  )
  expect_s3_class(p, "ggplot")
})

test_that("map_all_communities saves PNGs when out_dir given", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("ggspatial")
  skip_if_not_installed("tidyterra")

  d <- make_map_data()

  samples_list <- list(
    test_community = list(
      primary = d$points,
      secondary = d$points[1:2, ]
    )
  )

  tmp_dir <- tempfile("map_test")
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  result <- map_all_communities(
    samples_list,
    d$community,
    community_id_col = "name",
    out_dir = tmp_dir,
    buffer_radius = 50
  )

  expect_true(fs::file_exists(fs::path(tmp_dir, "overview.png")))
  expect_true(fs::file_exists(
    fs::path(tmp_dir, "test_community_primary.png")
  ))
  expect_true(fs::file_exists(
    fs::path(tmp_dir, "test_community_secondary.png")
  ))
})
