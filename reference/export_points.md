# Export sample points, buffers, and tile overlays

Saves points in multiple formats, optionally generates buffer polygons
and OsmAnd-compatible SQLite tile overlays. Each community gets a
self-contained folder. Buffer radius is derived from the per-community
`$min_distance` in the enriched
[`split_batches()`](https://epicentre-msf.github.io/gpssampling/reference/split_batches.md)
output.

## Usage

``` r
export_points(
  samples_list,
  out_dir,
  formats = c("gpkg", "gpx"),
  include_buffers = TRUE,
  set = c("primary", "secondary"),
  print_table = FALSE,
  overwrite = TRUE,
  quiet = FALSE
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

- set:

  Which point set to export: `"primary"` (default) or `"secondary"`.

- print_table:

  Logical. If `TRUE`, computes buffer-level statistics (buildings per
  buffer) and attaches a
  [`flextable::flextable()`](https://davidgohel.github.io/flextable/reference/flextable.html)
  as `attr(, "summary_table")` and the underlying data frame as
  `attr(, "summary_df")`. Default `FALSE`.

- overwrite:

  Logical. If `TRUE` (default), removes existing output directory for
  the selected set before exporting. This prevents "database is locked"
  errors when re-exporting SQLite tile overlays.

- quiet:

  Logical. If `TRUE`, suppress progress messages. Default `FALSE`.

## Value

Invisibly, a tibble of exported file paths with columns: `community`,
`set`, `batch`, `type`, `format`, `path`. When `print_table = TRUE`,
carries `summary_table`, `summary_df`, and `buffer_details` attributes.
`buffer_details` is a data frame with per-buffer building counts
(`community`, `buffer_idx`, `n_buildings`, `buffer_radius_m`) suitable
for
[`plot_buffer_distribution()`](https://epicentre-msf.github.io/gpssampling/reference/plot_buffer_distribution.md).

## Examples

``` r
if (FALSE) { # \dontrun{
export_points(batched, "output", set = "primary")
} # }
```
