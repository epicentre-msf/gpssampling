# Check if an object is not null

This function checks if an R object is not null.

## Usage

``` r
is.not.null(x)
```

## Arguments

- x:

  The object to be checked

## Value

A logical value indicating whether the object is not null

## Examples

``` r
is.not.null(NULL)
#> Error in is.not.null(NULL): could not find function "is.not.null"
# FALSE

is.not.null(1L)
#> Error in is.not.null(1L): could not find function "is.not.null"
# TRUE
```
