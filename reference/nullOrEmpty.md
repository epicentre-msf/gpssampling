# Check if a value is null or empty

This function checks if a value is `NULL` or has zero length.

## Usage

``` r
nullOrEmpty(x)
```

## Arguments

- x:

  The value to be checked.

## Value

`TRUE` if the value is `NULL` or has zero length, `FALSE` otherwise.

## Examples

``` r
nullOrEmpty(NULL)
#> Error in nullOrEmpty(NULL): could not find function "nullOrEmpty"
# [1] TRUE
nullOrEmpty(character())
#> Error in nullOrEmpty(character()): could not find function "nullOrEmpty"
# [1] TRUE
nullOrEmpty(5L)
#> Error in nullOrEmpty(5L): could not find function "nullOrEmpty"
# [1] FALSE
```
