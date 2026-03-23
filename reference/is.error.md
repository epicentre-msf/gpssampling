# Determine if an object is a try-error

This function takes an object as input and returns `TRUE` if the object
is a try-error object and `FALSE` otherwise.

## Usage

``` r
is.error(x)
```

## Arguments

- x:

  An object to check for being a try-error

## Value

`TRUE` if `x` is a try-error, `FALSE` otherwise

## Examples

``` r
is.error(tryCatch(stop('An error occurred'), error = function(e) e))
#> Error in is.error(tryCatch(stop("An error occurred"), error = function(e) e)): could not find function "is.error"
# Returns TRUE

is.error(10L)
#> Error in is.error(10L): could not find function "is.error"
# Returns FALSE

is.error(NULL)
#> Error in is.error(NULL): could not find function "is.error"
# Returns FALSE
```
