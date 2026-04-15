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
  pts_a$point_id <- 1:10
  pts_a$named_point_id <- sprintf("%03d", 1:10)
  pts_b$point_id <- 11:18
  pts_b$named_point_id <- sprintf("%03d", 11:18)
  sec_a <- sf::st_sf(
    id = 11:15,
    community = "alpha",
    selection_order = 1:5,
    point_id = 19:23,
    named_point_id = sprintf("%03d", 19:23),
    geometry = sf::st_sfc(
      lapply(11:15, function(i) sf::st_point(c(i * 0.001, i * 0.001))),
      crs = 4326L
    )
  )
  sec_b <- sf::st_sf(
    id = 9:12,
    community = "beta",
    selection_order = 1:4,
    point_id = 24:27,
    named_point_id = sprintf("%03d", 24:27),
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

test_that("split_batches returns enriched structure with metadata", {
  samples <- make_sample_result()
  result <- split_batches(samples, n_batches = 3L, set = "primary")

  expect_type(result, "list")
  expect_named(result, c("alpha", "beta"))

  for (nm in names(result)) {
    expect_true("batches" %in% names(result[[nm]]))
    expect_true("min_distance" %in% names(result[[nm]]))
    expect_true("n_buildings" %in% names(result[[nm]]))
    expect_true("seed" %in% names(result[[nm]]))
    expect_true("n_batches" %in% names(result[[nm]]))
    expect_true("buildings" %in% names(result[[nm]]))
    expect_s3_class(result[[nm]]$batches, "sf")
    expect_true("assigned_batch" %in% names(result[[nm]]$batches))
    batches <- result[[nm]]$batches$assigned_batch
    expect_true(all(batches %in% 1:3))
  }

  # First 3 points of alpha should be batches 1, 2, 3
  expect_equal(result$alpha$batches$assigned_batch[1:3], c(1L, 2L, 3L))
  # Metadata preserved
  expect_equal(result$alpha$min_distance, 50)
  expect_equal(result$alpha$n_buildings, 10L)
})

test_that("split_batches works with named n_batches", {
  samples <- make_sample_result()
  result <- split_batches(
    samples,
    n_batches = c(alpha = 2L, beta = 4L),
    set = "primary"
  )
  expect_true(all(result$alpha$batches$assigned_batch %in% 1:2))
  expect_true(all(result$beta$batches$assigned_batch %in% 1:4))
})

test_that("split_batches works on secondary set", {
  samples <- make_sample_result()
  result <- split_batches(samples, n_batches = 2L, set = "secondary")
  expect_equal(nrow(result$alpha$batches), 5L)
  expect_equal(nrow(result$beta$batches), 4L)
  expect_true(all(result$alpha$batches$assigned_batch %in% 1:2))
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
  # split_batches returns enriched list with $batches
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

test_that("export_points with buffers creates sqlitedb files with size in name", {
  samples <- make_sample_result()
  batched <- split_batches(samples, n_batches = 2L, set = "primary")

  tmp_dir <- tempfile("export_buf_test")
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  manifest <- export_points(
    batched,
    out_dir = tmp_dir,
    formats = c("gpkg"),
    include_buffers = TRUE,
    set = "primary"
  )

  tiles_rows <- manifest[manifest$type == "tiles", ]
  expect_true(nrow(tiles_rows) > 0L)

  for (p in tiles_rows$path) {
    expect_true(file.exists(p))
  }

  # Buffer filenames include size suffix
  buf_rows <- manifest[manifest$type == "buffers", ]
  expect_true(all(grepl("_50m_", buf_rows$path)))
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

# GPX uses point_id as name
# ............................................................................

test_that("write_gpx uses point_id as waypoint name", {
  pts <- sf::st_sf(
    id = c("bldg_a", "bldg_b"),
    point_id = c(42L, 43L),
    community = "alpha",
    geometry = sf::st_sfc(
      sf::st_point(c(0.001, 0.001)),
      sf::st_point(c(0.002, 0.002)),
      crs = 4326L
    )
  )

  tmp <- tempfile(fileext = ".gpx")
  on.exit(unlink(tmp), add = TRUE)
  write_gpx(pts, tmp)

  gpx_back <- sf::st_read(tmp, layer = "waypoints", quiet = TRUE)
  expect_equal(gpx_back$name, c("42", "43"))
})

test_that("write_gpx prefers named_point_id over point_id", {
  pts <- sf::st_sf(
    id = c("bldg_a", "bldg_b"),
    point_id = c(42L, 43L),
    named_point_id = c("042", "043"),
    community = "alpha",
    geometry = sf::st_sfc(
      sf::st_point(c(0.001, 0.001)),
      sf::st_point(c(0.002, 0.002)),
      crs = 4326L
    )
  )

  tmp <- tempfile(fileext = ".gpx")
  on.exit(unlink(tmp), add = TRUE)
  write_gpx(pts, tmp)

  gpx_back <- sf::st_read(tmp, layer = "waypoints", quiet = TRUE)
  expect_equal(gpx_back$name, c("042", "043"))
})

# create_earth_project
# ............................................................................

test_that("create_earth_project creates valid KML", {
  samples <- make_sample_result()

  tmp_kml <- tempfile(fileext = ".kml")
  on.exit(unlink(tmp_kml), add = TRUE)

  # buffer_radius=NULL derives from $min_distance
  result <- create_earth_project(
    samples,
    tmp_kml
  )

  expect_equal(result, tmp_kml)
  expect_true(file.exists(tmp_kml))

  kml_text <- readLines(tmp_kml)
  kml_full <- paste(kml_text, collapse = "\n")

  # Check structure
  expect_true(grepl("<kml", kml_full))
  expect_true(grepl("Primary Points", kml_full))
  expect_true(grepl("Secondary Points", kml_full))
  expect_true(grepl("Primary Buffers", kml_full))
  expect_true(grepl("Secondary Buffers", kml_full))

  # Check named_point_ids appear as placemark names (zero-padded)
  expect_true(grepl("<name>001</name>", kml_full))
  expect_true(grepl("<name>019</name>", kml_full))

  # Check community folders
  expect_true(grepl("<name>alpha</name>", kml_full))
  expect_true(grepl("<name>beta</name>", kml_full))

  # Check buffer placemarks (uses named_point_id)
  expect_true(grepl("<name>Buffer 001</name>", kml_full))
  expect_true(grepl("Polygon", kml_full))
})

# extract_metadata
# ............................................................................

test_that("extract_metadata returns correct structure", {
  samples <- make_sample_result()
  pri <- split_batches(samples, n_batches = 2L, set = "primary")

  meta <- extract_metadata(pri)

  expect_s3_class(meta, "data.frame")
  expect_true(all(
    c("community", "point_id", "named_point_id", "assigned_batch", "set") %in%
      names(meta)
  ))
  expect_equal(unique(meta$set), "primary")
  expect_equal(sort(unique(meta$community)), c("alpha", "beta"))
  expect_equal(nrow(meta), 10L + 8L)
})

test_that("extract_metadata with primary and secondary", {
  samples <- make_sample_result()
  pri <- split_batches(samples, n_batches = 2L, set = "primary")
  sec <- split_batches(samples, n_batches = 2L, set = "secondary")

  meta <- extract_metadata(pri, sec)

  expect_equal(sort(unique(meta$set)), c("primary", "secondary"))
  expect_equal(nrow(meta), 10L + 8L + 5L + 4L)
})

test_that("extract_metadata carries buffer_size and n_teams attributes", {
  samples <- make_sample_result()
  pri <- split_batches(samples, n_batches = 3L, set = "primary")

  meta <- extract_metadata(pri)

  buf_sizes <- attr(meta, "buffer_size")
  expect_named(buf_sizes, c("alpha", "beta"))
  expect_equal(buf_sizes[["alpha"]], 50)

  n_teams <- attr(meta, "n_teams")
  expect_named(n_teams, c("alpha", "beta"))
  expect_equal(n_teams[["alpha"]], 3L)
})
