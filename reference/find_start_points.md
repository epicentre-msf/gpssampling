# Find buildings closest to roads

For each community, identifies the building closest to an OSM road. This
is a standalone utility; the main pipeline uses
`order_selected_points()` internally for proximity-based ordering after
random selection.

## Usage

``` r
find_start_points(
  buildings_list,
  road_types = c("primary", "secondary", "tertiary", "residential", "trunk",
    "unclassified")
)
```

## Arguments

- buildings_list:

  Named list of `sf` POINT objects (output of
  [`crop_buildings()`](https://epicentre-msf.github.io/gpssampling/reference/crop_buildings.md)).

- road_types:

  Character vector of OSM `highway=*` values to consider as roads.

## Value

A named integer vector of starting point `id` values, one per community.

## Examples

``` r
if (FALSE) { # \dontrun{
starts <- find_start_points(buildings_list)
# c(community_one = 17, community_two = 78)
} # }
```
