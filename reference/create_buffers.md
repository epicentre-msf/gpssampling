# Create circular buffers around points

Generates buffer polygons around sampled points using an auto-detected
UTM projection for accurate metric distances. Accepts either a single
`sf` POINT or a named list of communities from
[`sample_communities()`](https://epicentre-msf.github.io/gpssampling/reference/sample_communities.md)
or
[`split_batches()`](https://epicentre-msf.github.io/gpssampling/reference/split_batches.md).

## Usage

``` r
create_buffers(x, radius = 50, set = c("primary", "secondary"))
```

## Arguments

- x:

  An `sf` POINT object, or a named list of communities (output of
  [`sample_communities()`](https://epicentre-msf.github.io/gpssampling/reference/sample_communities.md)
  or
  [`split_batches()`](https://epicentre-msf.github.io/gpssampling/reference/split_batches.md)).

- radius:

  Buffer radius in meters. Default `50`.

- set:

  Which point set to buffer when `x` is a list: `"primary"` (default) or
  `"secondary"`. Ignored when `x` is a plain `sf`.

## Value

If `x` is `sf`: an `sf` POLYGON with `buffer_radius_m` column. If `x` is
a list: a named list of `sf` POLYGON, one per community.

## Examples

``` r
if (FALSE) { # \dontrun{
buffers <- create_buffers(samples, radius = 50, set = "primary")
} # }
```
