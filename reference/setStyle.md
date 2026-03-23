# Efficiently style a group that has already been added to the map

Call with a group and a vector of style lists of length N. The first N
features of the group will be restyled.

## Usage

``` r
setStyle(map, group, styles, label = NULL, offset = 0L)
```

## Examples

``` r
# \donttest{
renderLeaflet('map', {
  leaflet() |> addPolygons(data = zones, group = 'zones', color = 'red')
})
#> Warning: restarting interrupted promise evaluation
#> Warning: restarting interrupted promise evaluation
#> Error: object 'zones' not found
colour <- 'blue'
styles <- lapply(pal(values), function(colour) {
  list(fillColor = colour, color = colour)
})
#> Error in pal(values): could not find function "pal"
leafletProxy('map') |>
  setStyle('zones', styles)
#> Error in setStyle(leafletProxy("map"), "zones", styles): could not find function "setStyle"
# }
```
