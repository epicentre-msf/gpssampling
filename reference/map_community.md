# Create a static map for one community

Renders a publication-quality map showing a community polygon, sampled
points (colored by batch), and buffer zones for a single point set
(primary or secondary). Returns a `ggplot` object.

## Usage

``` r
map_community(
  community_name,
  community_sf,
  points_sf,
  buffers_sf = NULL,
  color_batches = TRUE,
  show_labels = TRUE,
  label_size = 1.8,
  point_shape = 16,
  basemap = "OpenStreetMap.HOT",
  point_color = "#e97a52",
  buffer_color = "#90EE9066",
  community_color = "#808380",
  community_fill = "#f6efdd",
  title = community_name,
  subtitle = NULL
)
```

## Arguments

- community_name:

  Character, community name (used in title).

- community_sf:

  An `sf` POLYGON for the community boundary.

- points_sf:

  An `sf` POINT of sampled points. If it has an `assigned_batch` column,
  batch coloring is applied.

- buffers_sf:

  Optional `sf` POLYGON of buffers.

- color_batches:

  Logical. If `TRUE` and `points_sf` has an `assigned_batch` column,
  color points by batch. Default `TRUE`.

- show_labels:

  Logical. If `TRUE` and `points_sf` has a `point_id` column, display
  point IDs as text labels. Default `TRUE`.

- label_size:

  Numeric, text size for point ID labels. Default `1.8`.

- point_shape:

  Marker shape. Default `16` (filled circle).

- basemap:

  Tile provider name for
  [`maptiles::get_tiles()`](https://rdrr.io/pkg/maptiles/man/get_tiles.html).
  Default `"OpenStreetMap.HOT"`.

- point_color:

  Uniform color when no `assigned_batch`. Default `"#e97a52"`.

- buffer_color:

  Buffer fill color (with alpha). Default `"#90EE9066"`.

- community_color:

  Boundary stroke color. Default `"#808380"`.

- community_fill:

  Boundary fill color. Default `"#f6efdd"`.

- title:

  Map title. Defaults to `community_name`.

- subtitle:

  Optional subtitle. If `NULL`, auto-generated from point count, ID
  range, and min pairwise distance.

## Value

A `ggplot` object.

## Details

Requires `ggplot2`, `ggspatial`, and `tidyterra` (all in Suggests).

## Examples

``` r
if (FALSE) { # \dontrun{
p <- map_community("community_one", comm_poly, pri_pts, bufs)
ggplot2::ggsave("community_one.png", p, width = 10, height = 12)
} # }
```
