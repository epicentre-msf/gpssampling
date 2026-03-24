# Create an overview map of all communities

Renders a zoomed-out map showing all community polygons, sampled points
(uniform color), and buffers. No batch coloring on the overview for
clarity.

## Usage

``` r
map_overview(
  samples_list,
  communities_sf,
  community_id_col = "name",
  set = "primary",
  buffer_radius = 50,
  basemap = "OpenStreetMap.HOT",
  point_color = "#e97a52",
  buffer_color = "#90EE9066",
  community_color = "#808380",
  community_fill = "#f6efdd",
  title = "Sampling Overview",
  subtitle = NULL
)
```

## Arguments

- samples_list:

  Output of
  [`sample_communities()`](https://epicentre-msf.github.io/gpssampling/reference/sample_communities.md)
  or
  [`split_batches()`](https://epicentre-msf.github.io/gpssampling/reference/split_batches.md).

- communities_sf:

  All community polygons (`sf`).

- community_id_col:

  Column name for community ID.

- set:

  Which point set: `"primary"` or `"secondary"`.

- buffer_radius:

  Buffer radius in meters. Default `50`.

- basemap:

  Tile provider name. Default `"OpenStreetMap.HOT"`.

- point_color:

  Uniform point color. Default `"#e97a52"`.

- buffer_color:

  Buffer fill color. Default `"#90EE9066"`.

- community_color:

  Boundary stroke color. Default `"#808380"`.

- community_fill:

  Boundary fill color. Default `"#f6efdd"`.

- title:

  Map title. Default `"Sampling Overview"`.

- subtitle:

  Optional subtitle.

## Value

A `ggplot` object.

## Examples

``` r
if (FALSE) { # \dontrun{
p <- map_overview(samples, communities, community_id_col = "name")
} # }
```
