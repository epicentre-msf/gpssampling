# Read Google Maps basemap tiles for given polygons

This function reads Google Maps basemap tiles for the given polygons and
returns a list of tile grids.

## Usage

``` r
readBasemapGoogle(polygons, async_queue = NULL)
```

## Arguments

- polygons:

  A spatial feature collection of polygons or multipolygons in EPSG:4326
  projection

- async_queue:

  An optional async queue object to track progress

## Value

A list of tile grids

## Examples

``` r
# Create a polygon from North Carolina
nc <- sf::st_read(system.file('shape/nc.shp', package = 'sf'))
#> Reading layer `nc' from data source 
#>   `/home/runner/work/_temp/Library/sf/shape/nc.shp' using driver `ESRI Shapefile'
#> Simple feature collection with 100 features and 14 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -84.3238525391 ymin: 33.8819923401 xmax: -75.4569778442 ymax: 36.5896492004
#> Geodetic CRS:  NAD27
nc <- sf::st_transform(nc, 'EPSG:4326')

# Read Google Maps basemap tiles for the polygon
readBasemapGoogle(nc[1L, ])
#> Error in readBasemapGoogle(nc[1L, ]): could not find function "readBasemapGoogle"
```
