# Create zip archives for field distribution

Bundles GPX files and SQLite tile overlays into zip archives ready for
copying to field devices.

## Usage

``` r
zip_points(
  export_dir,
  out_dir = export_dir,
  sets = c("primary", "secondary"),
  prefix = ""
)
```

## Arguments

- export_dir:

  Root export directory (output of
  [`export_points()`](https://epicentre-msf.github.io/gpssampling/reference/export_points.md)).

- out_dir:

  Where to write zip files. Defaults to `export_dir`.

- sets:

  Character vector of sets to zip: `"primary"`, `"secondary"`, or both.

- prefix:

  Optional prefix for zip filenames (e.g., project name).

## Value

Invisibly, character vector of created zip file paths.

## Examples

``` r
if (FALSE) { # \dontrun{
zips <- zip_points("output", prefix = "kgh-")
} # }
```
