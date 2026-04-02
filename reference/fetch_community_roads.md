# Fetch OSM roads for all communities

Downloads and caches road linestrings for each community in a polygon
layer. Roads are saved as `.gpkg` files in `road_dir`, one per
community, using the community ID as filename. On subsequent calls,
cached files are reused without re-downloading.

## Usage

``` r
fetch_community_roads(
  communities_sf,
  community_id_col = "name",
  road_dir,
  road_types = c("primary", "secondary", "tertiary", "residential", "trunk",
    "unclassified"),
  timeout = 120
)
```

## Arguments

- communities_sf:

  An `sf` POLYGON of community boundaries.

- community_id_col:

  Column name for community ID in `communities_sf`. Default `"name"`.

- road_dir:

  Directory where road `.gpkg` files are cached. Created if it does not
  exist.

- road_types:

  Character vector of OSM `highway=*` values.

- timeout:

  Overpass API timeout in seconds. Default `120`.

## Value

A named list of `sf` LINESTRING objects (one per community). Communities
where no roads were found have `NULL`.

## Details

This function can be called independently before
[`sample_communities()`](https://epicentre-msf.github.io/gpssampling/reference/sample_communities.md)
to pre-download roads (e.g. while online), then pass the same `road_dir`
to
[`sample_communities()`](https://epicentre-msf.github.io/gpssampling/reference/sample_communities.md)
for offline use.

## Examples

``` r
if (FALSE) { # \dontrun{
roads <- fetch_community_roads(
  communities,
  community_id_col = "name",
  road_dir = "output/roads"
)
} # }
```
