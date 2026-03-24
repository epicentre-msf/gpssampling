# Create test data
strata_sf <-
  sf::st_polygon(list(matrix(c(0L, 0L, 0.1, 0L, 0.1, 0.1, 0L, 0.1, 0L, 0L), ncol = 2L, byrow = 2L))) |>
  sf::st_sfc()
strata_sf <-
  sf::st_sf(id = 1L, geometry = strata_sf) |>
  sf::st_set_crs(4326L)

# Test 1: Check if the function returns an object of class "sf"
test_that('add_tilenum returns an object of class sf', {
  result <- add_tilenum(strata_sf)
  expect_s3_class(result, 'sf')
})

# Test 2: Check if the function adds two new columns to the input object
test_that('add_tilenum adds two new columns to the input object', {
  result <- add_tilenum(strata_sf)
  expect_identical(ncol(result), ncol(strata_sf) + 2L)
})

# Test 3: Check if the function correctly calculates tile numbers in Mercator projection
test_that('add_tilenum correctly calculates tile numbers in Mercator projection', {
  result <- add_tilenum(strata_sf, mercator = TRUE, zoom = 10L)
  expect_identical(result$x[1L], 512L)
  expect_identical(result$y[1L], 511L)
})

# Test 4: Check if the function correctly calculates tile numbers in WGS84 projection
test_that('add_tilenum correctly calculates tile numbers in WGS84 projection', {
  result <- add_tilenum(strata_sf, mercator = FALSE, zoom = 10L)
  expect_identical(result$x[1L], 512L)
  expect_identical(result$y[1L], 511L)
})

