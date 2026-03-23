# Drop Null Values

This function removes null values from a given input.

## Usage

``` r
dropNulls(x)
```

## Arguments

- x:

  The input vector or list.

## Value

A vector or list with null values removed.

## Examples

``` r
dropNulls(c(1L, NULL, 3L, NULL))
#> Error in dropNulls(c(1L, NULL, 3L, NULL)): could not find function "dropNulls"
# Output: [1] 1 3

dropNulls(list('a', NULL, 'b', NULL))
#> Error in dropNulls(list("a", NULL, "b", NULL)): could not find function "dropNulls"
# Output: [[1]] [1] 'a'
#         [[2]] [1] 'b'
```
