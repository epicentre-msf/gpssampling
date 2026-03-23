# Create a button widget

This function creates a button with specified characteristics.

## Usage

``` r
button(..., loading = FALSE, loading_label = .("Loading..."))
```

## Arguments

- ...:

  Additional arguments to be passed to the button function

- loading:

  Logical indicating if the button is in a loading state

- loading_label:

  The label to display when the button is in a loading state

- inputId:

  The input ID of the button

- label:

  The text to display on the button

- icon:

  An optional icon to display alongside the label

- class:

  An optional class for styling the button

## Value

A button widget with specified characteristics

## Examples

``` r
button('button_id', 'Click me', icon = 'check')
#> Error in button("button_id", "Click me", icon = "check"): could not find function "button"
button('submit', 'Submit', loading = TRUE)
#> Error in button("submit", "Submit", loading = TRUE): could not find function "button"
```
