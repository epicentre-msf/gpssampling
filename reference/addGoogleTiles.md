# Adds Google Tiles Layer

Adds Google Tiles Layer

## Usage

``` r
addGoogleTiles(
  map,
  type = c("roadmap", "satellite", "terrain"),
  layerId = NULL,
  group = NULL,
  ...
)
```

## Arguments

- map:

  The Map widget

- type:

  String. Type of Tiles to display

- layerId:

  String. An optional unique ID for the layer

- group:

  String. An optional group name for the layer

- ...:

  Optional Parameters required by the Google API described at
  <https://msdn.microsoft.com/en-us/library/ff701716.aspx>

## See also

Get a Google Maps API Key:
<https://msdn.microsoft.com/en-us/library/ff428642.aspx>

## Examples

``` r
if (FALSE) { # \dontrun{
library(leaflet)
map <- leaflet() |>
  addTiles() |>
  setView(lng = -71.06, lat = 42.36, zoom = 12L)
addGoogleTiles(map)
} # }
```
