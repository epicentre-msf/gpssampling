# Create a Sidebar

Create a Sidebar

## Usage

``` r
sidebar_tabs(id = "sidebar", iconList = NULL, ...)
```

## Arguments

- id:

  The id of the sidebar, which must match the `id` of
  [`addSidebar`](https://yves-amevoin.github.io/gpssampling/reference/addSidebar.md).
  Default is `"sidebar"`

- iconList:

  A list of icons to be shown, when the sidebar is collapsed. The list
  is required and must match the amount of
  [`sidebar_pane`](https://yves-amevoin.github.io/gpssampling/reference/sidebar_pane.md).

- ...:

  The individual
  [`sidebar_pane`](https://yves-amevoin.github.io/gpssampling/reference/sidebar_pane.md).

## Value

A `shiny.tag` with individual sidebar panes

## References

<https://github.com/Turbo87/sidebar-v2>,
<https://github.com/Turbo87/sidebar-v2/blob/master/doc/usage.md>

## See also

Other Sidebar Functions:
[`addSidebar()`](https://yves-amevoin.github.io/gpssampling/reference/addSidebar.md),
[`closeSidebar()`](https://yves-amevoin.github.io/gpssampling/reference/closeSidebar.md),
[`openSidebar()`](https://yves-amevoin.github.io/gpssampling/reference/openSidebar.md),
[`removeSidebar()`](https://yves-amevoin.github.io/gpssampling/reference/removeSidebar.md),
[`sidebar_pane()`](https://yves-amevoin.github.io/gpssampling/reference/sidebar_pane.md)

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
