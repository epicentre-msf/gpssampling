# devtools::test(filter = 'test.utils.pkg', stop_on_failure = TRUE)

polygon_sfc <-
  st_bbox_polygon(c(13.105, 11.825, 13.11, 11.83)) |>
  sf::st_sfc() |>
  sf::st_set_crs(4326L)

zone <- readSpatialLayer(file = here::here('inst/data-ext/zone.zip'))$layer

test_that('Plot Map Sat', {
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
  expect_identical(unique(roofs$tiles$cells), c('111111111', '111111110'))
  expect_identical(range(roofs$tiles$x), c(140614L, 140618L))
  expect_identical(range(roofs$tiles$y), c(122395L, 122399L))

  expect_s3_class(roofs$roofs, c('sf', 'data.frame'))
  expect_identical(as.character(unique(sf::st_geometry_type(roofs$roofs$geometry))), 'POINT')
  expect_named(roofs$roofs, c('geometry', 'polygon', 'x', 'y'))
  expect_identical(nrow(roofs$roofs), 1147L)
  expect_identical(unique(roofs$roofs$polygon), 1L)
  expect_identical(range(roofs$roofs$x), c(140614L, 140618L))
  expect_identical(range(roofs$roofs$y), c(122395L, 122399L))
})

test_that('Add Roofs (Open Building)', {
  roofs <- addRoofsOpenBuilding(polygons = polygon_sfc, dir = getDirAppTemp())

  expect_type(roofs, 'list')
  expect_named(roofs, c('tiles', 'roofs'))

  expect_s3_class(roofs$tiles, c('tbl_df', 'tbl', 'data.frame'))
  expect_named(roofs$tiles, c('x', 'y', 'building', 'cells'))
  expect_identical(nrow(roofs$tiles), 25L)
  expect_identical(unique(roofs$tiles$cells), c('111111111', '111111110'))
  expect_identical(range(roofs$tiles$x), c(140614L, 140618L))
  expect_identical(range(roofs$tiles$y), c(122395L, 122399L))

  expect_s3_class(roofs$roofs, c('sf', 'data.frame'))
  expect_identical(as.character(unique(sf::st_geometry_type(roofs$roofs$geometry))), 'POINT')
  expect_named(roofs$roofs, c('geometry', 'polygon', 'x', 'y'))
  expect_identical(nrow(roofs$roofs), 1147L)
  expect_identical(unique(roofs$roofs$polygon), 1L)
  expect_identical(range(roofs$roofs$x), c(140614L, 140618L))
  expect_identical(range(roofs$roofs$y), c(122395L, 122399L))
})

test_that('Add Roofs (Google): Big', {
  skip('Disabled')

  polygons_sf <- readSpatialLayer(file = here::here('vignettes/data/polygons - 2022-06-21'))$layer
  polygons_sf <- polygons_sf |>
    dplyr::filter(admin3Name == 'Matoto') |>
    sf::st_as_sfc() |>
    sf::st_cast('POLYGON') |>
    sf::st_set_crs(4326L)

  roofs <- addRoofsGoogle(polygons_sf)
})

test_that('Add Roofs (Google): Big', {
  skip('Disabled')

  polygons_sf <- readSpatialLayer(file = here::here('vignettes/data/gin_admbnda_ocha_fis.zip'), layer = 'gin_admbnda_adm3_ocha')$layer
  polygons_sf <- polygons_sf |>
    dplyr::filter(admin3Name == 'Matoto') |>
    sf::st_as_sfc() |>
    sf::st_cast('POLYGON') |>
    sf::st_set_crs(4326L)

  roofs <- addRoofsGoogle(polygons_sf)
})

test_that('Add Roofs (Google): Very Big', {
  skip('Disabled')

  roofs <- readSpatialLayer(file = here::here('vignettes/data/uga_east_buildings.zip'))$layer

  # microbenchmark::microbenchmark(
  #   "st_make_valid" = {sf::st_make_valid(roofs[1:1000,])},
  #   "geos_make_valid" = {geos::geos_make_valid(roofs[1:1000,])},
  #   replications = 1,
  #   columns = c("test", "replications", "elapsed")
  # )

  roofs_geos <- geos::as_geos_geometry(roofs[1:500000, ])
  roofs_geos <- geos::geos_make_collection(roofs_geos)
  roofs_geos <- geos::geos_unary_union(roofs_geos)
  roofs_geos <-
    sf::st_as_sf(roofs_geos) |>
    sf::st_cast('POLYGON') |>
    sf::st_set_crs(4326L)

  layer$DISID <- 1L

  # benchmark sf
  benchmark(
    unionSpatialPolygons = sf::st_as_sf(unionSpatialPolygons(as_Spatial(layer), rep(1L, nrow(layer)))),
    gUnaryUnion = sf::st_as_sf(gUnaryUnion(as_Spatial(layer), id = layer$DISID)),
    st_union = sf::st_union(layer),
    gBuffer = sf::st_as_sf(gBuffer(as_Spatial(layer), byid = FALSE, width = 0L)),
    replications = 5L,
    columns = c('test', 'replications', 'elapsed')
  )

  roofs <- addRoofsGoogle()
})

test_that('Check polygonToRasterCells', {
  skip('Disabled')

  # 50x50 km

  layer <- readSpatialLayer(file = f_uga_admbnda_shp, layer = 'uga_admbnda_adm2_ubos_20200824')

  polygon <- layer$layer[1L, ]
  polygon_cells_rst <- polygonToRasterCells(polygon = polygon)

  expect_identical(as.character(class(polygon_cells_rst)), 'SpatRaster')
  expect_identical(nrow(polygon_cells_rst), 1767L)
  expect_identical(ncol(polygon_cells_rst), 1131L)
  expect_identical(sum(terra::values(polygon_cells_rst), na.rm = TRUE), 1073102L)
  expect_identical(sqrt(units::set_units(sf::st_area(polygon), km^2L)), units::set_units(52.576972, km))

  # 250x250 km

  layer <- readSpatialLayer(file = f_uga_admbnda_shp, layer = 'uga_admbnda_adm1_ubos_20200824')

  polygon <- layer$layer[1L, ]
  polygon_cells_rst <- polygonToRasterCells(polygon = polygon)

  expect_identical(as.character(class(polygon_cells_rst)), 'SpatRaster')
  expect_identical(nrow(polygon_cells_rst), 5847L)
  expect_identical(ncol(polygon_cells_rst), 5556L)
  expect_identical(sum(terra::values(polygon_cells_rst), na.rm = TRUE), 24309823L)
  expect_identical(sqrt(units::set_units(sf::st_area(polygon), km^2L)), units::set_units(250.87365, km))
})

test_that('Check rasterCellsToPolygon', {
  skip('Disabled')

  # 50x50 km

  layer <- readSpatialLayer(file = f_uga_admbnda_shp, layer = 'uga_admbnda_adm4_ubos_20200824')

  polygon <- layer$layer[1L, ]
  polygon_cells_rst <- polygonToRasterCells(polygon = polygon)

  polygon_cells_vect <- rasterCellsToPolygon(raster = polygon_cells_rst)

  expect_identical(as.character(class(polygon_cells_vect)), 'SpatVector')

  # 250x250 km

  layer <- readSpatialLayer(file = f_uga_admbnda_shp, layer = 'uga_admbnda_adm1_ubos_20200824')

  polygon <- layer$layer[1L, ]
  polygon_cells_rst <- polygonToRasterCells(polygon = polygon)

  polygon_cells_vect <- rasterCellsToPolygon(raster = polygon_cells_rst)

  expect_identical(as.character(class(polygon_cells_vect)), 'SpatVector')

  # On génère des cellules vides
  # x <- terra::values(polygon_cells_rst, mat = FALSE)
  # x[!is.na(x)] <- sample(c(NA, 1), length(x[!is.na(x)]), replace = TRUE)

  # terra::values(polygon_cells_rst) <- x

  # polygon_cells_vect <- terra::as.polygons(polygon_cells_rst, extent = FALSE)

  # zoom_ext <- terra::ext(32, 32.01, 0.0, 0.01)

  # terra::plot(polygon_cells_rst)
  # terra::plot(terra::crop(polygon_cells_rst , zoom_ext))
  # terra::plot(terra::crop(polygon_cells_vect, zoom_ext), add = TRUE)
})


test_that('Check polygonToTiles', {
  skip('Disabled')

  # 50x50 km

  layer <- readSpatialLayer(file = f_uga_admbnda_shp, layer = 'uga_admbnda_adm4_ubos_20200824')

  polygon <- layer$layer[1L, ]
  polygon_cells_rst <- polygonToRasterCells(polygon = polygon)

  polygon_cells_vect <- rasterCellsToPolygon(raster = polygon_cells_rst)

  expect_identical(as.character(class(polygon_cells_vect)), 'SpatVector')

  # 250x250 km

  layer <- readSpatialLayer(file = f_uga_admbnda_shp, layer = 'uga_admbnda_adm1_ubos_20200824')

  polygon <- layer$layer[1L, ]
  polygon_cells_rst <- polygonToRasterCells(polygon = polygon)

  polygon_cells_vect <- rasterCellsToPolygon(raster = polygon_cells_rst)

  expect_identical(as.character(class(polygon_cells_vect)), 'SpatVector')

  # country

  layer <- readSpatialLayer(file = f_uga_admbnda_shp, layer = 'uga_admbnda_adm0_ubos_20200824')

  polygon <- layer$layer[1L, ]
  polygon_cells_rst <- polygonToRasterCells(polygon = polygon)

  polygon_cells_vect <- rasterCellsToPolygon(raster = polygon_cells_rst)

  expect_identical(as.character(class(polygon_cells_vect)), 'SpatVector')

  # On génère des cellules vides
  # x <- terra::values(polygon_cells_rst, mat = FALSE)
  # x[!is.na(x)] <- sample(c(NA, 1), length(x[!is.na(x)]), replace = TRUE)

  # terra::values(polygon_cells_rst) <- x

  # polygon_cells_vect <- terra::as.polygons(polygon_cells_rst, extent = FALSE)

  # zoom_ext <- terra::ext(32, 32.01, 0.0, 0.01)

  # terra::plot(polygon_cells_rst)
  # terra::plot(terra::crop(polygon_cells_rst , zoom_ext))
  # terra::plot(terra::crop(polygon_cells_vect, zoom_ext), add = TRUE)
})
