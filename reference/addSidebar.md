# Add a Sidebar Leaflet Control

The sidebar plugin only works in a reactive environment (e.g Shiny), as
the HTML must be created by using
[`sidebar_tabs`](https://epicentre-msf.github.io/gpssampling/reference/sidebar_tabs.md)
and
[`sidebar_pane`](https://epicentre-msf.github.io/gpssampling/reference/sidebar_pane.md)
and it must be created before
[`leafletOutput`](https://rstudio.github.io/leaflet/reference/map-shiny.html).

## Usage

``` r
addSidebar(
  map,
  options = list(container = "sidebar", position = "left", fit = TRUE)
)
```

## Arguments

- map:

  A leaflet map widget

- options:

  A named list with `position` and `fit` elements.

- id:

  Id of the sidebar-div. Must match with the `id` of
  [`sidebar_tabs`](https://epicentre-msf.github.io/gpssampling/reference/sidebar_tabs.md)

## Value

the new `map` object

## References

<https://github.com/Turbo87/sidebar-v2>

## See also

Other Sidebar Functions:
[`closeSidebar()`](https://epicentre-msf.github.io/gpssampling/reference/closeSidebar.md),
[`openSidebar()`](https://epicentre-msf.github.io/gpssampling/reference/openSidebar.md),
[`removeSidebar()`](https://epicentre-msf.github.io/gpssampling/reference/removeSidebar.md),
[`sidebar_pane()`](https://epicentre-msf.github.io/gpssampling/reference/sidebar_pane.md),
[`sidebar_tabs()`](https://epicentre-msf.github.io/gpssampling/reference/sidebar_tabs.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(shiny)
runApp(paste0(
  system.file('examples', package = 'leaflet.extras2'),
  '/sidebar_app.R'
))
} # }
```
