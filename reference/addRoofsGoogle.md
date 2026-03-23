# Add Roofs using Google Maps API

This function takes a set of polygons and retrieves roof data using the
Google Maps API.

## Usage

``` r
addRoofsGoogle(polygons, dir = getDirAppTemp(), async_queue = NULL)
```

## Arguments

- polygons:

  A set of polygons in 'sfc_POLYGON' or 'sfc_MULTIPOLYGON' format.

- dir:

  The directory to save the retrieved images.

- async_queue:

  An optional asynchronous queue for progress tracking.

## Value

A list containing the tiles and roofs retrieved from the Google Maps
API.

## Examples

``` r
if (FALSE) { # \dontrun{
  polygon <-  sf::st_set_crs(sf::st_sfc(st_bbox_polygon(c(13.105, 11.825, 13.11, 11.83))), 4326L)

  addRoofsGoogle(polygons = polygon)
} # }
```
