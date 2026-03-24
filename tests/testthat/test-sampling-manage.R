# Tests for R/sampling_manage.R — Phase B GPS point management

# sf::st_write GPKG triggers dyn.load() that segfaults on macOS CI runners
# (GDAL binary incompatibility). Tests pass on Ubuntu CI and local macOS.
# File-level skip required because segfault crashes the subprocess before
# any in-test skip can execute.
is_ci <- isTRUE(as.logical(Sys.getenv("CI", "false")))
is_mac <- Sys.info()[["sysname"]] == "Darwin"
if (is_ci && is_mac) {
  skip("sf::st_write GPKG segfaults on macOS CI (GDAL binary issue)")
}

# Helpers (reuse from test-sampling.R patterns)
# ............................................................................

make_sample_result <- function() {
  set.seed(42L)
  pts_a <- sf::st_sf(
    id = 1:10,
    community = "alpha",
    selection_order = 1:10,
    geometry = sf::st_sfc(
      lapply(1:10, function(i) sf::st_point(c(i * 0.001, i * 0.001))),
      crs = 4326L
    )
  )
  pts_b <- sf::st_sf(
    id = 1:8,
    community = "beta",
    selection_order = 1:8,
    geometry = sf::st_sfc(
      lapply(
        1:8,
        function(i) sf::st_point(c(0.05 + i * 0.001, 0.05 + i * 0.001))
      ),
      crs = 4326L
    )
  )
  sec_a <- sf::st_sf(
    id = 11:15,
    community = "alpha",
    geometry = sf::st_sfc(
      lapply(11:15, function(i) sf::st_point(c(i * 0.001, i * 0.001))),
      crs = 4326L
    )
  )
  sec_b <- sf::st_sf(
    id = 9:12,
    community = "beta",
    geometry = sf::st_sfc(
      lapply(
        9:12,
        function(i) sf::st_point(c(0.05 + i * 0.001, 0.05 + i * 0.001))
      ),
      crs = 4326L
    )
  )
  list(
    alpha = list(
      buildings = pts_a,
      primary = pts_a,
      secondary = sec_a,
      min_distance = 50,
      seed = 42L
    ),
    beta = list(
      buildings = pts_b,
      primary = pts_b,
      secondary = sec_b,
      min_distance = 50,
      seed = 42L
    )
  )
}

# split_batches
# ............................................................................

test_that("split_batches assigns round-robin batch numbers", {
  samples <- make_sample_result()
  result <- split_batches(samples, n_batches = 3L, set = "primary")

  expect_type(result, "list")
  expect_named(result, c("alpha", "beta"))

  for (nm in names(result)) {
    expect_true("assigned_batch" %in% names(result[[nm]]))
    batches <- result[[nm]]$assigned_batch
    expect_true(all(batches %in% 1:3))
  }

  # First 3 points of alpha should be batches 1, 2, 3
  expect_equal(result$alpha$assigned_batch[1:3], c(1L, 2L, 3L))
})

test_that("split_batches works with named n_batches", {
  samples <- make_sample_result()
  result <- split_batches(
    samples,
    n_batches = c(alpha = 2L, beta = 4L),
    set = "primary"
  )
  expect_true(all(result$alpha$assigned_batch %in% 1:2))
  expect_true(all(result$beta$assigned_batch %in% 1:4))
})

test_that("split_batches works on secondary set", {
  samples <- make_sample_result()
  result <- split_batches(samples, n_batches = 2L, set = "secondary")
  expect_equal(nrow(result$alpha), 5L)
  expect_equal(nrow(result$beta), 4L)
  expect_true(all(result$alpha$assigned_batch %in% 1:2))
})

# create_buffers
# ............................................................................

test_that("create_buffers works on sf POINT", {
  pts <- sf::st_sf(
    id = 1:3,
    geometry = sf::st_sfc(
      sf::st_point(c(0, 0)),
      sf::st_point(c(0.01, 0)),
      sf::st_point(c(0, 0.01)),
      crs = 4326L
    )
  )
  result <- create_buffers(pts, radius = 100)
  expect_s3_class(result, "sf")
  expect_equal(nrow(result), 3L)
  expect_true("buffer_radius_m" %in% names(result))
  expect_equal(unique(result$buffer_radius_m), 100)
  geom_types <- unique(as.character(sf::st_geometry_type(result)))
  expect_true(all(geom_types == "POLYGON"))
})

test_that("create_buffers works on sample_communities list", {
  samples <- make_sample_result()
  result <- create_buffers(samples, radius = 50, set = "primary")
  expect_type(result, "list")
  expect_named(result, c("alpha", "beta"))
  for (nm in names(result)) {
    expect_s3_class(result[[nm]], "sf")
    expect_true("buffer_radius_m" %in% names(result[[nm]]))
  }
})

test_that("create_buffers works on split_batches output", {
  samples <- make_sample_result()
  batched <- split_batches(samples, n_batches = 2L, set = "primary")
  # split_batches returns sf directly (not nested list)
  result <- create_buffers(batched, radius = 50)
  expect_type(result, "list")
  expect_named(result, c("alpha", "beta"))
})

# create_buffer_tiles
# ............................................................................

test_that("create_buffer_tiles creates valid SQLite database", {
  pts <- sf::st_sf(
    id = 1:3,
    geometry = sf::st_sfc(
      sf::st_point(c(0, 0)),
      sf::st_point(c(0.01, 0)),
      sf::st_point(c(0, 0.01)),
      crs = 4326L
    )
  )
  buffers <- create_buffers(pts, radius = 500)

  tmp_db <- tempfile(fileext = ".sqlitedb")
  on.exit(unlink(tmp_db), add = TRUE)

  result <- create_buffer_tiles(buffers, tmp_db, min_zoom = 10L, max_zoom = 11L)
  expect_equal(result, tmp_db)
  expect_true(file.exists(tmp_db))

  con <- DBI::dbConnect(RSQLite::SQLite(), tmp_db)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  tables <- DBI::dbListTables(con)
  expect_true("tiles" %in% tables)
  expect_true("info" %in% tables)

  info <- DBI::dbReadTable(con, "info")
  expect_equal(info$minzoom, 10L)
  expect_equal(info$maxzoom, 11L)

  tile_count <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM tiles")$n
  expect_true(tile_count > 0L)
})

# export_points
# ............................................................................

test_that("export_points creates expected directory structure", {
  samples <- make_sample_result()
  batched <- split_batches(samples, n_batches = 2L, set = "primary")

  tmp_dir <- tempfile("export_test")
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  manifest <- export_points(
    batched,
    out_dir = tmp_dir,
    formats = c("gpkg"),
    include_buffers = FALSE,
    set = "primary"
  )

  expect_s3_class(manifest, "tbl_df")
  expect_true(nrow(manifest) > 0L)
  expect_true(all(
    c("community", "set", "batch", "type", "format", "path") %in%
      names(manifest)
  ))

  # Check alpha directory exists
  alpha_dir <- fs::path(tmp_dir, "primary", "alpha")
  expect_true(fs::dir_exists(alpha_dir))

  # Check all files exist
  all_gpkg <- fs::path(alpha_dir, "alpha_primary_all.gpkg")
  expect_true(fs::file_exists(all_gpkg))
  batch1_gpkg <- fs::path(alpha_dir, "alpha_primary_batch_1.gpkg")
  expect_true(fs::file_exists(batch1_gpkg))
})

test_that("export_points with buffers creates sqlitedb files", {
  samples <- make_sample_result()
  batched <- split_batches(samples, n_batches = 2L, set = "primary")

  tmp_dir <- tempfile("export_buf_test")
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  manifest <- export_points(
    batched,
    out_dir = tmp_dir,
    formats = c("gpkg"),
    include_buffers = TRUE,
    buffer_radius = 100,
    set = "primary"
  )

  tiles_rows <- manifest[manifest$type == "tiles", ]
  expect_true(nrow(tiles_rows) > 0L)

  for (p in tiles_rows$path) {
    expect_true(file.exists(p))
  }
})

# zip_points
# ............................................................................

test_that("zip_points creates zip archive", {
  samples <- make_sample_result()
  batched <- split_batches(samples, n_batches = 2L, set = "primary")

  tmp_dir <- tempfile("zip_test")
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  export_points(
    batched,
    out_dir = tmp_dir,
    formats = c("gpkg", "gpx"),
    include_buffers = FALSE,
    set = "primary"
  )

  zips <- zip_points(tmp_dir, sets = "primary", prefix = "test-")
  expect_length(zips, 1L)
  expect_true(file.exists(zips[1L]))
  expect_true(grepl("test-primary-points\\.zip$", zips[1L]))
})

# email_points — only test that it requires emayili
# ............................................................................

test_that("email_points requires emayili package", {
  skip_if(rlang::is_installed("emayili"), "emayili is installed")
  expect_error(
    email_points("fake.zip", to = "test@example.com"),
    "emayili"
  )
})
