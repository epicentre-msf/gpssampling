# Radio Group Buttons

This function creates a radio group input control with buttons in a
Shiny app.

## Usage

``` r
radioGroupButtons(...)
```

## Arguments

- ...:

  Arguments passed to shinyWidgets::radioGroupButtons function.

## Value

A shinyWidgets::radioGroupButtons wrapped in a shiny::div container with
rounded corners.

## Examples

``` r
radioGroupButtons('radio_buttons', label = 'Choose an option', choices = c('Option 1', 'Option 2', 'Option 3'))
#> Error in radioGroupButtons("radio_buttons", label = "Choose an option",     choices = c("Option 1", "Option 2", "Option 3")): could not find function "radioGroupButtons"
```
