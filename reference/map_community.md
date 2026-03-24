# Create a static map for one community

Renders a publication-quality map showing a community polygon, sampled
points (optionally colored by batch), and buffer zones. Returns a
`ggplot` object that can be further customized before saving.

## Usage

``` r
map_community(
  community_name,
  community_sf,
  points_sf,
  buffers_sf = NULL,
  batch_colors = TRUE,
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
  batch coloring is available.

- buffers_sf:

  Optional `sf` POLYGON of buffers. If `NULL`, no buffers are drawn.

- batch_colors:

  Logical. If `TRUE` and `points_sf` has `assigned_batch`, color by
  batch. Default `TRUE`.

- basemap:

  Tile provider name for
  [`maptiles::get_tiles()`](https://rdrr.io/pkg/maptiles/man/get_tiles.html).
  Default `"OpenStreetMap.HOT"`.

- point_color:

  Uniform color when `batch_colors = FALSE`. Default `"#e97a52"`.

- buffer_color:

  Buffer fill color (with alpha). Default `"#90EE9066"`.

- community_color:

  Boundary stroke color. Default `"#808380"`.

- community_fill:

  Boundary fill color. Default `"#f6efdd"`.

- title:

  Map title. Defaults to `community_name`.

- subtitle:

  Optional subtitle.

## Value

A `ggplot` object.

## Details

Requires `ggplot2`, `ggspatial`, and `tidyterra` (all in Suggests).

## Examples

``` r
if (FALSE) { # \dontrun{
p <- map_community("community_one", community_poly, sampled_pts, bufs)
ggplot2::ggsave("community_one.png", p, width = 10, height = 12)
} # }
```
