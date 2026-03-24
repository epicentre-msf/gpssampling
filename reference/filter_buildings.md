# Filter building footprints by type

Removes non-residential buildings (hospitals, schools, etc.) using OSM
`building` tags. Supports three workflows: OSM-only, user footprints
with OSM labeling, and user footprints without OSM.

## Usage

``` r
filter_buildings(
  buildings_sf,
  osm_buildings_sf = NULL,
  remove_tags = c("hospital", "school", "church", "mosque", "industrial", "commercial",
    "warehouse", "government", "public"),
  keep_untagged = TRUE
)
```

## Arguments

- buildings_sf:

  An `sf` POLYGON of building footprints. Can be output from
  [`fetch_osm_buildings()`](https://epicentre-msf.github.io/gpssampling/reference/fetch_osm_buildings.md)
  (has `building` column) or a user-provided dataset.

- osm_buildings_sf:

  Optional `sf` POLYGON of OSM buildings (output of
  [`fetch_osm_buildings()`](https://epicentre-msf.github.io/gpssampling/reference/fetch_osm_buildings.md)).
  Required when `buildings_sf` is user-provided and lacks a `building`
  column.

- remove_tags:

  Character vector of OSM `building=*` tag values to exclude. Defaults
  to common non-residential types.

- keep_untagged:

  Logical. When intersecting user footprints with OSM, keep buildings
  that have no OSM match? Default `TRUE` (assumes unlabeled buildings
  are residential).

## Value

An `sf` POLYGON of filtered buildings.

## Examples

``` r
if (FALSE) { # \dontrun{
# OSM-only
buildings <- fetch_osm_buildings(area) |> filter_buildings()

# User footprints + OSM labeling
osm <- fetch_osm_buildings(area)
filtered <- filter_buildings(my_buildings, osm_buildings_sf = osm)
} # }
```
