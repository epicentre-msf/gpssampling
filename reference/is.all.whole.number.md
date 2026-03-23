# Check if all elements in a vector are whole numbers

This function takes a numeric vector and returns TRUE if all elements in
the vector are whole numbers, and FALSE otherwise.

## Usage

``` r
is.all.whole.number(x)
```

## Arguments

- x:

  a numeric vector

## Value

TRUE if all elements in `x` are whole numbers, FALSE otherwise

## Examples

``` r
is.all.whole.number(c(1L, 2L, 3L)) # should return TRUE
#> Error in is.all.whole.number(c(1L, 2L, 3L)): could not find function "is.all.whole.number"
is.all.whole.number(c(1.5, 2.5, 3.5)) # should return FALSE
#> Error in is.all.whole.number(c(1.5, 2.5, 3.5)): could not find function "is.all.whole.number"
```
