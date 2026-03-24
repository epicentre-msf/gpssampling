# Export sample points, buffers, and tile overlays

Saves points in multiple formats, optionally generates buffer polygons
and OsmAnd-compatible SQLite tile overlays. Each community gets a
self-contained folder.

## Usage

``` r
export_points(
  samples_list,
  out_dir,
  formats = c("gpkg", "gpx"),
  include_buffers = TRUE,
  buffer_radius = 50,
  set = c("primary", "secondary")
)
```

## Arguments

- samples_list:

  Output of
  [`split_batches()`](https://epicentre-msf.github.io/gpssampling/reference/split_batches.md)
  (preferred) or
  [`sample_communities()`](https://epicentre-msf.github.io/gpssampling/reference/sample_communities.md).

- out_dir:

  Root output directory.

- formats:

  Character vector of export formats: `"gpkg"`, `"gpx"`, `"shp"`,
  `"kml"`. Default `c("gpkg", "gpx")`.

- include_buffers:

  Whether to generate and export buffer polygons and SQLite tile
  overlays. Default `TRUE`.

- buffer_radius:

  Buffer radius in meters. Default `50`.

- set:

  Which point set to export: `"primary"` (default) or `"secondary"`.

## Value

Invisibly, a tibble of exported file paths with columns: `community`,
`set`, `batch`, `type`, `format`, `path`.

## Examples

``` r
if (FALSE) { # \dontrun{
export_points(batched, "output", set = "primary")
} # }
```
