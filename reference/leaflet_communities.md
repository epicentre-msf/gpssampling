# Interactive leaflet map of all communities

Creates an interactive
[`leaflet::leaflet()`](https://rstudio.github.io/leaflet/reference/leaflet.html)
map with toggleable layers for communities, buildings, roads, primary
points, and secondary points. Points are colored by batch when
`color_batches = TRUE`. Primary points are shown as circles, secondary
as triangles. Layers are z-ordered via custom panes: communities
(bottom), roads, buildings, buffers, points (top).

## Usage

``` r
leaflet_communities(
  primary_batches,
  communities_sf,
  community_id_col = "name",
  secondary_batches = NULL,
  buildings_list = NULL,
  roads_list = NULL,
  color_batches = TRUE,
  buffer_radius = 50,
  primary_color = "#e97a52",
  secondary_color = "#1E90FF",
  primary_buffer_color = "#90EE90",
  secondary_buffer_color = "#ADD8E6",
  community_color = "#808380",
  building_color = "#8B6914",
  road_color = "#555555",
  out_file = NULL
)
```

## Arguments

- primary_batches:

  Named list of `sf` POINT objects (output of
  [`split_batches()`](https://epicentre-msf.github.io/gpssampling/reference/split_batches.md)
  with `set = "primary"`).

- communities_sf:

  Community polygons (`sf`).

- community_id_col:

  Column name for community ID.

- secondary_batches:

  Optional named list of `sf` POINT objects (output of
  [`split_batches()`](https://epicentre-msf.github.io/gpssampling/reference/split_batches.md)
  with `set = "secondary"`).

- buildings_list:

  Optional named list of `sf` objects, one per community (output of
  [`crop_buildings()`](https://epicentre-msf.github.io/gpssampling/reference/crop_buildings.md)).
  Accepts both POINT (centroids) and POLYGON (footprints). Hidden by
  default; toggle via layer control.

- roads_list:

  Optional named list of `sf` LINESTRING objects (output of
  [`fetch_community_roads()`](https://epicentre-msf.github.io/gpssampling/reference/fetch_community_roads.md)).
  One entry per community.

- color_batches:

  Logical. If `TRUE` (default) and points have `assigned_batch`, color
  by batch.

- buffer_radius:

  Buffer radius in meters. Default `50`.

- primary_color:

  Default color for primary points. Default `"#e97a52"`.

- secondary_color:

  Default color for secondary points. Default `"#1E90FF"`.

- primary_buffer_color:

  Fill for primary buffers. Default `"#90EE90"`.

- secondary_buffer_color:

  Fill for secondary buffers. Default `"#ADD8E6"`.

- community_color:

  Boundary stroke color. Default `"#808380"`.

- building_color:

  Fill/stroke for building footprints. Default `"#8B6914"`.

- road_color:

  Stroke color for roads. Default `"#555555"`.

- out_file:

  Optional file path for saving as a self-contained HTML file. Requires
  `htmlwidgets`. If `NULL` (default), no file is saved.

## Value

A `leaflet` htmlwidget object.

## Details

Includes fullscreen control (if `leaflet.extras` is installed), a
community navigation panel for quick zoom, and multiple base map
options.

## Examples

``` r
if (FALSE) { # \dontrun{
pri <- split_batches(samples, n_batches = 5L, set = "primary")
sec <- split_batches(samples, n_batches = 5L, set = "secondary")
m <- leaflet_communities(
  pri, communities,
  secondary_batches = sec,
  buildings_list = buildings_cropped,
  roads_list = roads
)
m
} # }
```
