# Create a button with optional loading state and loading label

Create a button with optional loading state and loading label

## Usage

``` r
buttonLoading(
  ...,
  margin = 2L,
  semantic = "default",
  size = NULL,
  hide = FALSE,
  outline = FALSE
)
```

## Arguments

- ...:

  Arguments passed on to
  [`shinyFeedback::loadingButton`](https://rdrr.io/pkg/shinyFeedback/man/loadingButton.html)

  `inputId`

  :   the input id

  `label`

  :   the button text (label)

  `class`

  :   the class(es) to apply to the button

  `style`

  :   style for button (pre-loading); character string w/ CSS styling
      format: "color: black; background-color: red;"

  `loadingLabel`

  :   text to show after button is clicked (e.g. during loading)

  `loadingSpinner`

  :   the loading spinner icon. Valid values are NULL, "spinner",
      "circle-notch", "sync", and "cog"

  `loadingClass`

  :   the loading button css class(es).

  `loadingStyle`

  :   style for button (while loading); character string w/ CSS styling
      format: "color: black; background-color: red;"

## Value

HTML button with optional loading behavior

## Examples

``` r
ui <- tagList(
  buttonLoading('act_submit', label = 'Submit'),
  buttonLoading('act_submit', label = 'Submit', semantic = 'danger')
)
#> Error in tagList(buttonLoading("act_submit", label = "Submit"), buttonLoading("act_submit",     label = "Submit", semantic = "danger")): could not find function "tagList"

shinyAppMinimal(ui = ui)
#> Error in shinyAppMinimal(ui = ui): could not find function "shinyAppMinimal"
```
