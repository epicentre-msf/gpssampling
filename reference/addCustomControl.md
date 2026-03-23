# Add a custom control to a Leaflet map.

This function adds a custom control to a Leaflet map allowing users to
customize the control with specified parameters.

## Usage

``` r
addCustomControl(
  map,
  position = "bottomright",
  id = "hollow",
  title = "",
  classes = "",
  content = ""
)
```

## Arguments

- map:

  A Leaflet map object.

- position:

  A character string specifying the position of the control on the map
  (default is 'bottomright').

- id:

  A character string providing an identifier for the custom control
  (default is 'hollow').

- title:

  A character string representing the title of the custom control.

- classes:

  A character string containing additional CSS classes to be applied to
  the custom control.

- content:

  A character string defining the content of the custom control.

## Value

Updated Leaflet map with the custom control added.

## Examples

``` r
if (FALSE) { # \dontrun{
map <- leaflet() |>
  addTiles() |>
  setView(lng = 0L, lat = 0L, zoom = 2L)
addCustomControl(map, position = 'topright', id = 'custom', title = 'Custom Control', content = 'This is a custom control.')
} # }
```
