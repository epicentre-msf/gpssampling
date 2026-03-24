# Create SQLite tile overlay for OsmAnd

Renders buffer polygons into a tiled SQLite database compatible with
OsmAnd's overlay/underlay tile format. Each tile is a 256x256 PNG with
transparent background.

## Usage

``` r
create_buffer_tiles(
  buffers_sf,
  out_file,
  min_zoom = 8L,
  max_zoom = 14L,
  fill_color = "#90EE9066",
  boundary_color = "#228B22CC"
)
```

## Arguments

- buffers_sf:

  An `sf` POLYGON of buffer zones (output of
  [`create_buffers()`](https://epicentre-msf.github.io/gpssampling/reference/create_buffers.md)).

- out_file:

  Path for the output `.sqlitedb` file.

- min_zoom:

  Minimum zoom level for tiles. Default `8L`.

- max_zoom:

  Maximum zoom level for tiles. Default `14L`.

- fill_color:

  Fill color in `#RRGGBBAA` format. Default `"#90EE9066"` (light green,
  40% opacity).

- boundary_color:

  Boundary color in `#RRGGBBAA` format. Default `"#228B22CC"` (forest
  green, 80% opacity).

## Value

Invisibly, the path to the created `.sqlitedb` file.

## Examples

``` r
if (FALSE) { # \dontrun{
create_buffer_tiles(buffers, "output/buffers.sqlitedb")
} # }
```
