# Add a PaintPolygon Control

Add a PaintPolygon control to a leaflet map widget.

## Usage

``` r
addPaintPolygonControl(
  map,
  position = "bottomright",
  radius = 30L,
  minRadius = 10L,
  maxRadius = 50L,
  layerOptions = list(),
  drawOptions = list(weight = 1L),
  eraseOptions = list(color = "#ff324a", weight = 1L),
  menu = TRUE
)
```

## Arguments

- map:

  The leaflet map widget.

- position:

  The position of the control (default is 'bottomright').

- radius:

  The radius of the brush (default is 30).

- minRadius:

  The minimum radius of the brush (default is 10).

- maxRadius:

  The maximum radius of the brush (default is 50).

- layerOptions:

  Additional options for the painted layer (default is an empty list).

- drawOptions:

  Options for drawing with the brush (default is a list with weight =
  1).

- eraseOptions:

  Options for erasing with the brush (default is a list with color =
  '#ff324a' and weight = 1).

- menu:

  Logical, whether to display the menu options (default is TRUE).
