# Map cropped buildings per community

Produces one large map per community showing the community boundary and
building footprint shapes over an OSM basemap. Returns a named list of
`ggplot` objects (one per community).

## Usage

``` r
map_cropped_buildings(
  buildings_sf,
  communities_sf,
  community_id_col = "name",
  clip = FALSE,
  building_fill = "#8B6914",
  building_stroke = "#5B3A29",
  community_color = "#c98585",
  community_fill = "#faf3e820",
  basemap = "OpenStreetMap",
  out_dir = NULL,
  width = 12,
  height = 12,
  dpi = 300
)
```

## Arguments

- buildings_sf:

  An `sf` POLYGON of building footprints (the same input you passed to
  [`crop_buildings()`](https://epicentre-msf.github.io/gpssampling/reference/crop_buildings.md)).

- communities_sf:

  Community polygons (`sf`).

- community_id_col:

  Column name for community ID in `communities_sf`. Default `"name"`.

- clip:

  Logical. If `TRUE`, buildings are geometrically clipped to the
  community boundary (slower but precise). If `FALSE` (default),
  buildings that intersect the community are drawn with their full
  footprint (fast).

- building_fill:

  Fill color for building footprints. Default `"#8B6914"` (brown).

- building_stroke:

  Stroke color for building outlines. Default `"#5B3A29"` (dark brown).

- community_color:

  Boundary stroke color. Default `"#c98585"`.

- community_fill:

  Boundary fill color (with transparency). Default `"#faf3e820"`.

- basemap:

  Tile provider name for
  [`maptiles::get_tiles()`](https://rdrr.io/pkg/maptiles/man/get_tiles.html).
  Default `"OpenStreetMap"`.

- out_dir:

  Optional output directory. If provided, maps are saved as PNG. If
  `NULL`, maps are only returned.

- width:

  Plot width in inches. Default `12`.

- height:

  Plot height in inches. Default `12`.

- dpi:

  Plot resolution. Default `300`.

## Value

A named list of `ggplot` objects, one per community.

## Details

By default, buildings are filtered to those overlapping each community
(fast spatial predicate) but not geometrically clipped. Set
`clip = TRUE` for exact boundary clipping (slower, needed only when
boundary-edge precision matters for publication).

Requires `ggplot2`, `ggspatial`, and `tidyterra` (all in Suggests).

## Examples

``` r
if (FALSE) { # \dontrun{
maps <- map_cropped_buildings(buildings, communities)
ggplot2::ggsave("community_one.png", maps[["community_one"]])
} # }
```
