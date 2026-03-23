# Drops NA Values

This function takes a vector and removes any NA values from it.

## Usage

``` r
dropNA(x)
```

## Arguments

- x:

  The input vector

## Value

A vector with NA values removed

## Examples

``` r
dropNA(c(1L, 2L, NA, 3L, NA, 4L))
#> Error in dropNA(c(1L, 2L, NA, 3L, NA, 4L)): could not find function "dropNA"
# [1] 1 2 3 4

dropNA(c('a', 'b', NA, 'c', NA, 'd'))
#> Error in dropNA(c("a", "b", NA, "c", NA, "d")): could not find function "dropNA"
# [1] 'a' 'b' 'c' 'd'

dropNA(c(TRUE, FALSE, NA, TRUE, NA, FALSE))
#> Error in dropNA(c(TRUE, FALSE, NA, TRUE, NA, FALSE)): could not find function "dropNA"
# [1]  TRUE FALSE  TRUE FALSE
```
