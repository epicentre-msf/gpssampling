# Wrapper to fs::path_package for the package

This function is a wrapper for the
[`fs::path_package`](https://fs.r-lib.org/reference/path_package.html)
function. It returns the path to the installed package.

## Usage

``` r
getPackagePath(...)
```

## Arguments

- ...:

  Other parameters to be passed to
  [fs::path_package](https://fs.r-lib.org/reference/path_package.html).

## Value

The path to the installed package.

## Details

This function internally calls
[fs::path_package](https://fs.r-lib.org/reference/path_package.html) and
returns the package path by providing the current package name using
[`utils::packageName()`](https://rdrr.io/r/utils/packageName.html).

## See also

[fs::path_package](https://fs.r-lib.org/reference/path_package.html)
which this function wraps.

## Examples

``` r
getPackagePath('data.csv')
#> Error in getPackagePath("data.csv"): could not find function "getPackagePath"
```
