# Check if a package is loaded in the development environment

This function checks if a package called 'devtools_shims' is loaded in
the development environment by searching for its associated file using
the [`utils::find`](https://rdrr.io/r/utils/apropos.html) function.

## Usage

``` r
isLoadedInDev()
```

## Value

A logical value indicating if the package is loaded in the development
environment.

## Examples

``` r
isLoadedInDev()
#> Error in isLoadedInDev(): could not find function "isLoadedInDev"
# [1] TRUE
```
