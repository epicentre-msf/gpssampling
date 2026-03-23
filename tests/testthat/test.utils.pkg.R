# devtools::test(filter = 'test.utils.pkg', stop_on_failure = TRUE)

source(testthat::test_path('prepare.R'), local = TRUE)

polygon_sfc <-
  st_bbox_polygon(c(13.105, 11.825, 13.11, 11.83)) |>
  sf::st_sfc() |>
  sf::st_set_crs(4326L)

zone_path <- system.file("data-ext", "zone.zip", package = "gpssampling")
if (!nzchar(zone_path)) {
  zone_path <- tryCatch(
    here::here("inst", "data-ext", "zone.zip"),
    error = function(e) ""
  )
}
if (!nzchar(zone_path) || !file.exists(zone_path)) {
  skip("zone.zip test data not found")
}
zone <- readSpatialLayer(file = zone_path)$layer

test_that('Plot Map Sat', {
  skip_on_ci()
  expect_snapshot_file(save_png(plotMap(zone, force = TRUE)), 'mapsat.png')
})

# test_that('Plot Map Road', {
#   expect_snapshot_file(save_png(plotMap(zone, force = TRUE, road = TRUE)), 'maproad.png')
# })

test_that('Read Basemap (Google)', {
  polygon_sfc <-
    st_bbox_polygon(c(13.105, 11.825, 13.11, 11.83)) |>
    sf::st_sfc() |>
    sf::st_set_crs(4326L)

  tiles <- readBasemapGoogle(polygon_sfc)

  expect_type(tiles, 'list')
  expect_named(tiles, c('tiles_count', 'polygons'))
  expect_identical(tiles$tiles_count, 366L)
})

test_that('Add Roofs (Google)', {
  roofs <- addRoofsGoogle(polygons = polygon_sfc, dir = getDirAppTemp())

  expect_type(roofs, 'list')
  expect_named(roofs, c('tiles', 'roofs'))

  expect_s3_class(roofs$tiles, c('tbl_df', 'tbl', 'data.frame'))
  expect_named(roofs$tiles, c('x', 'y', 'building', 'cells'))
  expect_identical(nrow(roofs$tiles), 25L)
  expect_true(all(unique(roofs$tiles$cells) %in%
    c('111111111', '111111110', '110111111', '111111000')))
  expect_identical(range(roofs$tiles$x), c(140614L, 140618L))
  expect_identical(range(roofs$tiles$y), c(122395L, 122399L))

  expect_s3_class(roofs$roofs, c('sf', 'data.frame'))
  expect_identical(as.character(unique(sf::st_geometry_type(roofs$roofs$geometry))), 'POINT')
  expect_named(roofs$roofs, c('geometry', 'polygon', 'x', 'y'))
  expect_true(nrow(roofs$roofs) > 500L)
  expect_identical(unique(roofs$roofs$polygon), 1L)
  expect_identical(range(roofs$roofs$x), c(140614L, 140618L))
  expect_identical(range(roofs$roofs$y), c(122395L, 122399L))
})

test_that('Add Roofs (Open Building)', {
  roofs <- addRoofsOpenBuilding(polygons = polygon_sfc, dir = getDirAppTemp())

  expect_type(roofs, 'list')
  expect_true('roofs' %in% names(roofs))

  if (!is.null(roofs$roofs)) {
    expect_s3_class(roofs$roofs, c('sf', 'data.frame'))
    expect_identical(
      as.character(unique(sf::st_geometry_type(roofs$roofs$geometry))),
      'POINT'
    )
    expect_true(nrow(roofs$roofs) > 0L)
    expect_identical(unique(roofs$roofs$polygon), 1L)
  }
})

test_that('Check polygonToRasterCells', {
  # 50x50 km

  layer <- readSpatialLayer(file = f_uga_admbnda_shp, layer = 'uga_admin2')

  polygon <- layer$layer[1L, ]
  polygon_cells_rst <- polygonToRasterCells(polygon = polygon)

  expect_identical(as.character(class(polygon_cells_rst)), 'SpatRaster')
  expect_equal(nrow(polygon_cells_rst), 1767L)
  expect_equal(ncol(polygon_cells_rst), 1131L)
  expect_identical(sum(terra::values(polygon_cells_rst), na.rm = TRUE), 1073102L)
  expect_equal(sqrt(units::set_units(sf::st_area(polygon), "km^2")), units::set_units(52.576972, "km"))

  # 250x250 km

  layer <- readSpatialLayer(file = f_uga_admbnda_shp, layer = 'uga_admin1')

  polygon <- layer$layer[1L, ]
  polygon_cells_rst <- polygonToRasterCells(polygon = polygon)

  expect_identical(as.character(class(polygon_cells_rst)), 'SpatRaster')
  expect_equal(nrow(polygon_cells_rst), 5847L)
  expect_equal(ncol(polygon_cells_rst), 5556L)
  expect_identical(sum(terra::values(polygon_cells_rst), na.rm = TRUE), 24309823L)
  expect_equal(sqrt(units::set_units(sf::st_area(polygon), "km^2")), units::set_units(250.87365, "km"))
})

test_that('Check rasterCellsToPolygon', {
  # 50x50 km

  layer <- readSpatialLayer(file = f_uga_admbnda_shp, layer = 'uga_admin4')

  polygon <- layer$layer[1L, ]
  polygon_cells_rst <- polygonToRasterCells(polygon = polygon)

  polygon_cells_vect <- rasterCellsToPolygon(raster = polygon_cells_rst)

  expect_identical(as.character(class(polygon_cells_vect)), 'SpatVector')

  # 250x250 km

  layer <- readSpatialLayer(file = f_uga_admbnda_shp, layer = 'uga_admin1')

  polygon <- layer$layer[1L, ]
  polygon_cells_rst <- polygonToRasterCells(polygon = polygon)

  polygon_cells_vect <- rasterCellsToPolygon(raster = polygon_cells_rst)

  expect_identical(as.character(class(polygon_cells_vect)), 'SpatVector')
})

test_that('Check polygonToTiles', {
  # 50x50 km

  layer <- readSpatialLayer(file = f_uga_admbnda_shp, layer = 'uga_admin4')

  polygon <- layer$layer[1L, ]
  polygon_cells_rst <- polygonToRasterCells(polygon = polygon)

  polygon_cells_vect <- rasterCellsToPolygon(raster = polygon_cells_rst)

  expect_identical(as.character(class(polygon_cells_vect)), 'SpatVector')

  # 250x250 km

  layer <- readSpatialLayer(file = f_uga_admbnda_shp, layer = 'uga_admin1')

  polygon <- layer$layer[1L, ]
  polygon_cells_rst <- polygonToRasterCells(polygon = polygon)

  polygon_cells_vect <- rasterCellsToPolygon(raster = polygon_cells_rst)

  expect_identical(as.character(class(polygon_cells_vect)), 'SpatVector')

  # country

  layer <- readSpatialLayer(file = f_uga_admbnda_shp, layer = 'uga_admin0')

  polygon <- layer$layer[1L, ]
  polygon_cells_rst <- polygonToRasterCells(polygon = polygon)

  polygon_cells_vect <- rasterCellsToPolygon(raster = polygon_cells_rst)

  expect_identical(as.character(class(polygon_cells_vect)), 'SpatVector')
})
