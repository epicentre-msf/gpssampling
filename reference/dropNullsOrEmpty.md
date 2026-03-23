# Drop Nulls or Empty Values

Remove elements that are either NULL or empty from a vector or list.

## Usage

``` r
dropNullsOrEmpty(x)
```

## Arguments

- x:

  A vector or a list.

## Value

A vector or a list with the NULL or empty values removed.

## Examples

``` r
dropNullsOrEmpty(c(1L, 2L, NULL, '', 5L))
#> Error in dropNullsOrEmpty(c(1L, 2L, NULL, "", 5L)): could not find function "dropNullsOrEmpty"
# [1] 1 2 5

dropNullsOrEmpty(list(1L, 2L, NULL, '', 5L))
#> Error in dropNullsOrEmpty(list(1L, 2L, NULL, "", 5L)): could not find function "dropNullsOrEmpty"
# [[1]]
# [1] 1
#
# [[2]]
# [1] 2
#
# [[3]]
# [1] 5
```
