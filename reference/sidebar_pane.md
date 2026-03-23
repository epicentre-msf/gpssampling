# Create a Sidebar Pane

Create a Sidebar Pane

## Usage

``` r
sidebar_pane(
  title = "Sidebar Title",
  id = NULL,
  icon = icon("caret-right"),
  ...
)
```

## Arguments

- title:

  A title for the sidebar panel

- id:

  An id for the sidebar panel

- icon:

  An icon for the sidebar panel

- ...:

  List of elements to include in the panel

## Value

A `shiny.tag` with sidebar-specific HTML classes

## References

<https://github.com/Turbo87/sidebar-v2>,
<https://github.com/Turbo87/sidebar-v2/blob/master/doc/usage.md>

## See also

Other Sidebar Functions:
[`addSidebar()`](https://yves-amevoin.github.io/gpssampling/reference/addSidebar.md),
[`closeSidebar()`](https://yves-amevoin.github.io/gpssampling/reference/closeSidebar.md),
[`openSidebar()`](https://yves-amevoin.github.io/gpssampling/reference/openSidebar.md),
[`removeSidebar()`](https://yves-amevoin.github.io/gpssampling/reference/removeSidebar.md),
[`sidebar_tabs()`](https://yves-amevoin.github.io/gpssampling/reference/sidebar_tabs.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(shiny)
sidebar_pane(id = 'id', icon = icon('cars'), tags$div())
} # }
```
