# Add Tile Numbers to Spatial Features

This function adds tile numbers to a spatial features object based on
its centroid coordinates.

## Usage

``` r
add_tilenum(x, mercator = TRUE, zoom = 18L)
```

## Arguments

- x:

  A spatial features object.

- mercator:

  A logical value indicating whether the coordinates are in Mercator
  projection. Default is TRUE.

- zoom:

  An integer indicating the zoom level for the tile numbers. Default is
  18.

## Value

A spatial features object with added tile numbers.

## Examples

``` r
nc <- sf::st_read(system.file('shape/nc.shp', package = 'sf'))
#> Reading layer `nc' from data source 
#>   `/home/runner/work/_temp/Library/sf/shape/nc.shp' using driver `ESRI Shapefile'
#> Simple feature collection with 100 features and 14 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -84.3238525391 ymin: 33.8819923401 xmax: -75.4569778442 ymax: 36.5896492004
#> Geodetic CRS:  NAD27
add_tilenum(nc)
#> Error in add_tilenum(nc): could not find function "add_tilenum"
```
