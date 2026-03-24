# Generate all maps and optionally save to files

Creates per-community maps (primary + secondary, batch-colored) and an
overview map. Always returns the ggplot objects. Optionally saves as PNG
files.

## Usage

``` r
map_all_communities(
  samples_list,
  communities_sf,
  community_id_col = "name",
  out_dir = NULL,
  buffer_radius = 50,
  width = 10,
  height = 12,
  dpi = 300,
  ...
)
```

## Arguments

- samples_list:

  Output of
  [`sample_communities()`](https://epicentre-msf.github.io/gpssampling/reference/sample_communities.md)
  or
  [`split_batches()`](https://epicentre-msf.github.io/gpssampling/reference/split_batches.md).

- communities_sf:

  Community polygons (`sf`).

- community_id_col:

  Column name for community ID.

- out_dir:

  Optional output directory. If provided, maps are saved as PNG. If
  `NULL`, maps are only returned.

- buffer_radius:

  Buffer radius in meters. Default `50`.

- width:

  Plot width in inches. Default `10`.

- height:

  Plot height in inches. Default `12`.

- dpi:

  Plot resolution. Default `300`.

- ...:

  Additional arguments passed to
  [`map_community()`](https://epicentre-msf.github.io/gpssampling/reference/map_community.md)
  (colors, basemap, etc.).

## Value

A named list of `ggplot` objects: `overview`, `{name}_primary`,
`{name}_secondary` for each community.

## Examples

``` r
if (FALSE) { # \dontrun{
maps <- map_all_communities(
  samples, communities,
  community_id_col = "name",
  out_dir = "output/maps"
)
} # }
```
