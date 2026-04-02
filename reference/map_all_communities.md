# Generate all maps and optionally save to files

Creates separate primary and secondary maps per community
(batch-colored), plus an overview map. Each map shows one point set with
its buffers, point IDs, and min pairwise distance. Always returns the
ggplot objects. Optionally saves as PNG files.

## Usage

``` r
map_all_communities(
  primary_batches,
  communities_sf,
  community_id_col = "name",
  secondary_batches = NULL,
  color_batches = TRUE,
  out_dir = NULL,
  buffer_radius = 50,
  primary_shape = 16,
  secondary_shape = 17,
  primary_buffer_color = "#90EE9066",
  secondary_buffer_color = "#ADD8E666",
  width = 10,
  height = 12,
  dpi = 300,
  ...
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
  with `set = "secondary"`). If `NULL`, only primary maps are created.

- color_batches:

  Logical. If `TRUE` (default) and points have `assigned_batch`, color
  by batch.

- out_dir:

  Optional output directory. If provided, maps are saved as PNG. If
  `NULL`, maps are only returned.

- buffer_radius:

  Buffer radius in meters. Default `50`.

- primary_shape:

  Marker shape for primary maps. Default `16` (filled circle).

- secondary_shape:

  Marker shape for secondary maps. Default `17` (filled triangle).

- primary_buffer_color:

  Buffer fill for primary maps. Default `"#90EE9066"`.

- secondary_buffer_color:

  Buffer fill for secondary maps. Default `"#ADD8E666"`.

- width:

  Plot width in inches. Default `10`.

- height:

  Plot height in inches. Default `12`.

- dpi:

  Plot resolution. Default `300`.

- ...:

  Additional arguments passed to
  [`map_community()`](https://epicentre-msf.github.io/gpssampling/reference/map_community.md)
  (basemap, label_size, etc.).

## Value

A named list of `ggplot` objects: `overview`, `{name}_primary`, and
`{name}_secondary` for each community.

## Examples

``` r
if (FALSE) { # \dontrun{
pri <- split_batches(samples, n_batches = 5L, set = "primary")
sec <- split_batches(samples, n_batches = 5L, set = "secondary")
maps <- map_all_communities(
  pri, communities,
  secondary_batches = sec,
  out_dir = "output/maps"
)
} # }
```
