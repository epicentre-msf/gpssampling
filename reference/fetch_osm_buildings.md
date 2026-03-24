# Fetch OSM building footprints

Downloads OpenStreetMap building footprints for a given area using
tile-based Overpass API queries.

## Usage

``` r
fetch_osm_buildings(area_sf, zoom = ZOOM_OSM)
```

## Arguments

- area_sf:

  An `sf` polygon or multipolygon defining the area of interest. Any CRS
  is accepted (transformed internally to EPSG:4326).

- zoom:

  Integer zoom level for tile-based queries (default `13L`, matching the
  package's `ZOOM_OSM` constant).

## Value

An `sf` POLYGON with columns `osm_id` (character), `building`
(character), and `geometry`. Returns an empty `sf` with the correct
schema if no buildings are found.

## Examples

``` r
if (FALSE) { # \dontrun{
state <- sf::st_read("boundary.gpkg")
buildings <- fetch_osm_buildings(state)
} # }
```
