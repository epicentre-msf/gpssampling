# Get Package Version

This function returns the version of the package.

## Usage

``` r
getPackageVersion(...)
```

## Arguments

- ...:

  Arguments passed on to
  [`utils::packageVersion`](https://rdrr.io/r/utils/packageDescription.html)

  `lib.loc`

  :   a character vector of directory names of R libraries, or `NULL`.
      The default value of `NULL` corresponds to all libraries currently
      known. If the default is used, the loaded packages and namespaces
      are searched before the libraries.

## Value

A character string representing the version of the package.

## Examples

``` r
getPackageVersion()
#> Error in getPackageVersion(): could not find function "getPackageVersion"
```
