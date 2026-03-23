# Checks if a given object is NULL, empty, or containing only empty strings.

This function returns TRUE if the object is NULL, has a length of 0, or
is an empty string (”). Otherwise, it returns FALSE.

## Usage

``` r
is.null.or.empty(x)
```

## Arguments

- x:

  The object to be checked.

## Value

A logical value indicating if the object is NULL, empty, or containing
only empty strings.

## Examples

``` r
is.null.or.empty(NULL)
#> Error in is.null.or.empty(NULL): could not find function "is.null.or.empty"
# TRUE
is.null.or.empty(character())
#> Error in is.null.or.empty(character()): could not find function "is.null.or.empty"
# TRUE
is.null.or.empty('')
#> Error in is.null.or.empty(""): could not find function "is.null.or.empty"
# TRUE
is.null.or.empty(0L)
#> Error in is.null.or.empty(0L): could not find function "is.null.or.empty"
# FALSE
```
