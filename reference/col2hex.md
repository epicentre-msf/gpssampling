# Convert Color Names to Hexadecimal Format

This function takes a color name and converts it to hexadecimal format.

## Usage

``` r
col2hex(col, alpha = FALSE)
```

## Arguments

- col:

  a character vector specifying the color name(s) to be converted.

- alpha:

  a logical value indicating whether to include alpha channel in the
  resulting hexadecimal color(s). Default is `FALSE`.

## Value

a character vector of the color(s) in hexadecimal format.

## Examples

``` r
col2hex('red') # '#FF0000'
#> Error in col2hex("red"): could not find function "col2hex"
col2hex(c('blue', 'green')) # c('#0000FF', '#00FF00')
#> Error in col2hex(c("blue", "green")): could not find function "col2hex"
```
