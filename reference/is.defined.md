# Check if an object is defined

This function checks if an object is defined in the R environment.

## Usage

``` r
is.defined(x)
```

## Arguments

- x:

  An object to be checked.

## Value

A logical value indicating if the object is defined or not.

## Examples

``` r
is.defined(a)
#> Error in is.defined(a): could not find function "is.defined"
# FALSE
a <- 5L
is.defined(a)
#> Error in is.defined(a): could not find function "is.defined"
# TRUE
```
