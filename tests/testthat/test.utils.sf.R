# styler: block

expect_layer <- function(file, layer, classes, type, n, key) {

  layer <- readSpatialLayer(file = file, layer = layer)

  layer_sf <- layer$layer
  layer_df <- sf::st_drop_geometry(layer_sf)

  expect_identical(layer$layer_geometry_types, c_named(n, type))
  expect_s3_class(layer_sf, classes)
  expect_identical(nrow(layer_sf), n)
  expect_identical(layer_df[, key], layer_df$lf_name)
  expect_identical(sf::st_crs(layer_sf)$input, 'WGS 84')

  layer
}

# testthat::test_file(testthat::test_path('test.utils.sf.R'))
test_that('ReadSpatialLayer SHP POLYGON', {

  shp_files <- unzip(f_uga_admbnda_shp)
  shp_file <- fs::path(fs::path_dir(path = shp_files$files[1L]), 'uga_admbnda_adm1_ubos_20200824.shp')

  layer <- readSpatialLayer(file = shp_file, verbose = FALSE)

  expect_s3_class(layer$layer, c('sf', 'data.frame'))
  expect_identical(nrow(layer$layer), 4L)
  expect_identical(layer$layer_geometry_types, c(POLYGON = 4L))
})

test_that('ReadSpatialLayer ZIP SHP POLYGON', {

  layer <- readSpatialLayer(file = f_uga_admbnda_shp, layer = 'uga_admbnda_adm1_ubos_20200824', verbose = FALSE)

  expect_s3_class(layer$layer, c('sf', 'data.frame'))
  expect_identical(nrow(layer$layer), 4L)
  expect_identical(layer$layer_geometry_types, c(POLYGON = 4L))
})

# test_that('ReadSpatialLayer ZIP SHP MULTIPOLYGON (BIG)', {
#   skip('Disabled')
#   layer <- readSpatialLayer(file = f_uga_east_buildings_shp, layer = 'hotosm_uga_east_buildings_polygons')
#   expect_s3_class(layer$layer, c('sf', 'data.frame'))
#   expect_identical(nrow(layer$layer), 1213621)
#   expect_identical(layer$layer_geometry_types, c(MULTIPOLYGON = 1213621))
# })

test_that('ReadSpatialLayer ZIP KMZ', {
  layer <- readSpatialLayer(file = f_uga_admbnda_shp, layer = 'uga_admbnda_adm1_ubos_20200824', verbose = FALSE)

  expect_s3_class(layer$layer, c('sf', 'data.frame'))
  expect_identical(nrow(layer$layer), 4L)
  expect_identical(layer$layer_geometry_types, c(POLYGON = 4L))
})

test_that('Function st_sample ', {

  set.seed(123L)

  layer <- readSpatialLayer(file = f_uga_sample_shp, verbose = FALSE)

  expect_sample <- function(type) {
    samples <- st_sample(layer$layer[1L, ], type = type, n = 100L)
    expect_s3_class(sf::st_geometry(samples), c('sfc_POINT', 'sfc'))
    expect_snapshot_file(
      save_png({
        terra::plot(terra::vect(layer$layer), col = heat.colors(6L))
        terra::plot(samples, add = TRUE)
      }),
      sprintf('sample_%s.png', type)
    )
    samples
  }

  expect_sample(type = 'random')
  expect_sample(type = 'regular')
  expect_sample(type = 'stratified')
  expect_sample(type = 'nonaligned')
  expect_sample(type = 'hexagonal')

})
