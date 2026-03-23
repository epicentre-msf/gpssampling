# Check if a vector has only unique elements

This function takes a vector as input and checks if it contains only
unique elements.

## Usage

``` r
is.unique(x)
```

## Arguments

- x:

  A vector of any type

## Value

TRUE if all elements of the vector are unique, FALSE otherwise.

## Examples

``` r
is.unique(c(1L, 2L, 3L)) # TRUE
#> Error in is.unique(c(1L, 2L, 3L)): could not find function "is.unique"
is.unique(c(1L, 1L, 2L)) # FALSE
#> Error in is.unique(c(1L, 1L, 2L)): could not find function "is.unique"
```
