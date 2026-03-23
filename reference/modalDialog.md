# Modal Dialog

This function creates a modal dialog using the `modalDialog` function
from the `shiny` package.

## Usage

``` r
modalDialog(...)
```

## Arguments

- ...:

  arguments to be passed to the `modalDialog` function

## Examples

``` r
shiny::shinyApp(
  ui = bslib::page_fillable(
    theme = theme(),
    shiny::actionButton('show', 'Show modal dialog')
  ),
  server = function(input, output) {
    shiny::observeEvent(input$show, {
      shiny::showModal(modalDialog(
        title = 'Important message',
        icon = 'alert'
      ))
    })
  }
)
#> Error in theme(): could not find function "theme"
```
