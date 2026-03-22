tests_assets_dir <- here::here('tests', 'testthat', '_assets')

fs::dir_create(tests_assets_dir, recurse = TRUE)

f_uga_admbnda_shp <- download(
  url = 'https://data.humdata.org/dataset/6d6d1495-196b-49d0-86b9-dc9022cde8e7/resource/7984457f-d12e-438f-8d6d-7e1a75694864/download/uga_admin_boundaries.shp.zip',
  destdir = tests_assets_dir
)
f_uga_sample_shp <- fs::path(tests_assets_dir, 'uga_sample_shp.zip')
f_uga_sample_light_shp <- fs::path(tests_assets_dir, 'uga_sample_light_shp.zip')

if (!is.null(f_uga_admbnda_shp) && !fs::file_exists(f_uga_sample_shp)) {
  layer <- readSpatialLayer(file = f_uga_admbnda_shp, layer = 'uga_admin2')

  polygons_sf <- layer$layer[1:10, ]
  polygons_sf$lf_key[1:5] <- 1L
  polygons_sf$lf_name[1:5] <- 'Merged'
  polygons_sf <- polygons_sf |>
    dplyr::group_by(lf_key, lf_name) |>
    dplyr::summarize()

  writeSpatialLayer(sf = polygons_sf, file = f_uga_sample_shp, layer = 'uga_sample', fieldname = 'lf_key')
}

if (!is.null(f_uga_admbnda_shp) && !fs::file_exists(f_uga_sample_light_shp)) {
  layer <- readSpatialLayer(file = f_uga_admbnda_shp, layer = 'uga_admin4')

  polygons_sf <- dplyr::filter(layer$layer, adm2_name == 'Alebtong')
  polygons_sf$lf_key[1:3] <- 1L
  polygons_sf$lf_name[1:3] <- 'Merged'
  polygons_sf <- polygons_sf |>
    dplyr::group_by(lf_key, lf_name) |>
    dplyr::summarize()

  writeSpatialLayer(sf = polygons_sf, file = f_uga_sample_light_shp, layer = 'f_uga_sample_light', fieldname = 'lf_key')
}
