# Function: matchValue

Description: Function that matches values in a data frame based on given
criteria.

## Usage

``` r
matchValue(tbl, x, var_x, var_y)
```

## Arguments

- tbl:

  Data frame containing the data.

- x:

  Vector of values to match in the data frame.

- var_x:

  Name of the variable in the data frame to match against x.

- var_y:

  Name of the variable in the data frame where the matched values are
  retrieved from.

## Value

A vector of values from var_y that match the values in x with respect to
var_x.

## Examples

``` r
tbl <- data.frame(a = 1:10, b = rev(1:10))
matchValue(tbl, c(5L, 1L), 'a', 'b')
#> Error in matchValue(tbl, c(5L, 1L), "a", "b"): could not find function "matchValue"
```
