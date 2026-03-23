# Assign Names to a Vector

This function assigns names to elements of a vector.

## Usage

``` r
c_named(x, names)
```

## Arguments

- x:

  A vector.

- names:

  A character vector specifying the names to assign to the elements of
  the vector `x`.

## Value

A vector with assigned names.

## Examples

``` r
c_named(c(1L, 2L, 3L), c('one', 'two', 'three'))
#> Error in c_named(c(1L, 2L, 3L), c("one", "two", "three")): could not find function "c_named"
```
