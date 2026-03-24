# Crop buildings to community polygons

Intersects building footprints with community polygons, converts to
centroids, and returns a named list (one `sf` POINT per community).
Points are sorted deterministically by longitude then latitude.

## Usage

``` r
crop_buildings(buildings_sf, communities_sf, community_id_col = "name")
```

## Arguments

- buildings_sf:

  An `sf` POLYGON of building footprints.

- communities_sf:

  An `sf` POLYGON/MULTIPOLYGON with one row per community.

- community_id_col:

  Character. Name of the column in `communities_sf` that uniquely
  identifies each community.

## Value

A named list of `sf` POINT objects (keyed by community name). Each has
columns: `id` (integer), `community` (character), and `geometry`
(sfc_POINT). If `buildings_sf` has an `osm_id` column, it is preserved.

## Examples

``` r
if (FALSE) { # \dontrun{
buildings_list <- buildings |>
  crop_buildings(communities, community_id_col = "name")
} # }
```
