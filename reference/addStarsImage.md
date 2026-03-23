# Add stars layer to a leaflet map

Add stars layer to a leaflet map

## Usage

``` r
addStarsImage(
  map,
  x,
  band = 1L,
  colors = "Spectral",
  opacity = 1L,
  attribution = NULL,
  layerId = NULL,
  group = NULL,
  project = FALSE,
  method = c("auto", "bilinear", "ngb"),
  maxBytes = 4L * 1024L * 1024L,
  data = leaflet::getMapData(map),
  ...
)
```

## Arguments

- map:

  a mapview or leaflet object.

- x:

  a stars layer.

- band:

  the band number to be plotted.

- colors:

  the color palette (see colorNumeric) or function to use to color the
  raster values (hint: if providing a function, set na.color to
  "#00000000" to make NA areas transparent)

- opacity:

  the base opacity of the raster, expressed from 0 to 1

- attribution:

  the HTML string to show as the attribution for this layer

- layerId:

  the layer id

- group:

  the name of the group this raster image should belong to (see the same
  parameter under addTiles)

- project:

  if TRUE, automatically project x to the map projection expected by
  Leaflet (EPSG:3857); if FALSE, it's the caller's responsibility to
  ensure that x is already projected, and that extent(x) is expressed in
  WGS84 latitude/longitude coordinates

- method:

  the method used for computing values of the new, projected raster
  image. "bilinear" (the default) is appropriate for continuous data,
  "ngb" - nearest neighbor - is appropriate for categorical data.
  Ignored if project = FALSE. See projectRaster for details.

- maxBytes:

  the maximum number of bytes to allow for the projected image (before
  base64 encoding); defaults to 4MB.

- data:

  the data object from which the argument values are derived; by
  default, it is the `data` object provided to
  [`leaflet()`](https://rstudio.github.io/leaflet/reference/leaflet.html)
  initially, but can be overridden.

- ...:

  currently not used.

## Details

This is an adaption of
[`addRasterImage`](https://rstudio.github.io/leaflet/reference/addRasterImage.html).
See that documentation for details.

## Examples

``` r
# \donttest{
library(stars)
#> Loading required package: abind
#> Loading required package: sf
#> Linking to GEOS 3.12.1, GDAL 3.8.4, PROJ 9.4.0; sf_use_s2() is TRUE
library(leaflet)

tif <- system.file('tif/L7_ETMs.tif', package = 'stars')
x <- read_stars(tif)
leaflet() |>
  addProviderTiles('OpenStreetMap') |>
  addStarsImage(x, project = TRUE)
#> Error in addStarsImage(addProviderTiles(leaflet(), "OpenStreetMap"), x,     project = TRUE): could not find function "addStarsImage"
# }
```
