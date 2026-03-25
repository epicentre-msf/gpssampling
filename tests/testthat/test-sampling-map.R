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

test_that("map_cropped_buildings returns named list of ggplots", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("ggspatial")
  skip_if_not_installed("tidyterra")

  communities <- sf::st_sf(
    name = c("alpha", "beta"),
    geometry = sf::st_sfc(
      sf::st_polygon(list(matrix(
        c(0, 0, 0.05, 0, 0.05, 0.05, 0, 0.05, 0, 0),
        ncol = 2L,
        byrow = TRUE
      ))),
      sf::st_polygon(list(matrix(
        c(0.05, 0.05, 0.1, 0.05, 0.1, 0.1, 0.05, 0.1, 0.05, 0.05),
        ncol = 2L,
        byrow = TRUE
      ))),
      crs = 4326L
    )
  )

  set.seed(42L)
  n <- 20L
  cx <- runif(n, 0.001, 0.099)
  cy <- runif(n, 0.001, 0.099)
  polys <- lapply(seq_len(n), function(i) {
    dx <- 0.0002
    sf::st_polygon(list(matrix(
      c(
        cx[i] - dx,
        cy[i] - dx,
        cx[i] + dx,
        cy[i] - dx,
        cx[i] + dx,
        cy[i] + dx,
        cx[i] - dx,
        cy[i] + dx,
        cx[i] - dx,
        cy[i] - dx
      ),
      ncol = 2L,
      byrow = TRUE
    )))
  })
  buildings <- sf::st_sf(
    osm_id = as.character(seq_len(n)),
    building = rep("yes", n),
    geometry = sf::st_sfc(polys, crs = 4326L)
  )

  result <- map_cropped_buildings(
    buildings,
    communities,
    community_id_col = "name"
  )
  expect_type(result, "list")
  expect_true(length(result) >= 1L)
  for (nm in names(result)) {
    expect_s3_class(result[[nm]], "ggplot")
  }
})

test_that("map_cropped_buildings handles single community", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("ggspatial")
  skip_if_not_installed("tidyterra")

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

  set.seed(42L)
  n <- 10L
  cx <- runif(n, 0.001, 0.049)
  cy <- runif(n, 0.001, 0.049)
  polys <- lapply(seq_len(n), function(i) {
    dx <- 0.0002
    sf::st_polygon(list(matrix(
      c(
        cx[i] - dx,
        cy[i] - dx,
        cx[i] + dx,
        cy[i] - dx,
        cx[i] + dx,
        cy[i] + dx,
        cx[i] - dx,
        cy[i] + dx,
        cx[i] - dx,
        cy[i] - dx
      ),
      ncol = 2L,
      byrow = TRUE
    )))
  })
  buildings <- sf::st_sf(
    osm_id = as.character(seq_len(n)),
    building = rep("yes", n),
    geometry = sf::st_sfc(polys, crs = 4326L)
  )

  result <- map_cropped_buildings(
    buildings,
    community,
    community_id_col = "name"
  )
  expect_type(result, "list")
  expect_equal(length(result), 1L)
  expect_s3_class(result[["solo"]], "ggplot")
})

test_that("map_cropped_buildings clip = TRUE works", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("ggspatial")
  skip_if_not_installed("tidyterra")

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

  set.seed(42L)
  n <- 10L
  cx <- runif(n, 0.001, 0.049)
  cy <- runif(n, 0.001, 0.049)
  polys <- lapply(seq_len(n), function(i) {
    dx <- 0.0002
    sf::st_polygon(list(matrix(
      c(
        cx[i] - dx,
        cy[i] - dx,
        cx[i] + dx,
        cy[i] - dx,
        cx[i] + dx,
        cy[i] + dx,
        cx[i] - dx,
        cy[i] + dx,
        cx[i] - dx,
        cy[i] - dx
      ),
      ncol = 2L,
      byrow = TRUE
    )))
  })
  buildings <- sf::st_sf(
    osm_id = as.character(seq_len(n)),
    building = rep("yes", n),
    geometry = sf::st_sfc(polys, crs = 4326L)
  )

  result <- map_cropped_buildings(
    buildings,
    community,
    community_id_col = "name",
    clip = TRUE
  )
  expect_type(result, "list")
  expect_equal(length(result), 1L)
  expect_s3_class(result[["solo"]], "ggplot")
})

test_that("map_cropped_buildings handles NA values in character columns", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("ggspatial")
  skip_if_not_installed("tidyterra")

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

  set.seed(42L)
  n <- 10L
  cx <- runif(n, 0.001, 0.049)
  cy <- runif(n, 0.001, 0.049)
  polys <- lapply(seq_len(n), function(i) {
    dx <- 0.0002
    sf::st_polygon(list(matrix(
      c(
        cx[i] - dx,
        cy[i] - dx,
        cx[i] + dx,
        cy[i] - dx,
        cx[i] + dx,
        cy[i] + dx,
        cx[i] - dx,
        cy[i] + dx,
        cx[i] - dx,
        cy[i] - dx
      ),
      ncol = 2L,
      byrow = TRUE
    )))
  })
  # Inject NA values in character columns (triggers the vapply bug)
  building_tags <- rep("yes", n)
  building_tags[c(3L, 7L)] <- NA_character_
  osm_ids <- as.character(seq_len(n))
  osm_ids[5L] <- NA_character_

  buildings <- sf::st_sf(
    osm_id = osm_ids,
    building = building_tags,
    geometry = sf::st_sfc(polys, crs = 4326L)
  )

  # Both clip modes should work without vapply errors
  result_fast <- map_cropped_buildings(
    buildings,
    community,
    community_id_col = "name",
    clip = FALSE
  )
  expect_s3_class(result_fast[["solo"]], "ggplot")

  result_clip <- map_cropped_buildings(
    buildings,
    community,
    community_id_col = "name",
    clip = TRUE
  )
  expect_s3_class(result_clip[["solo"]], "ggplot")
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
