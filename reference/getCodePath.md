# Get code path

This function takes in two arguments, `codes` and `values`, and returns
the code path for the given values.

## Usage

``` r
getCodePath(codes, values)
```

## Arguments

- codes:

  A data frame containing code information.

- values:

  A vector of values for which code paths are needed.

## Value

A vector of code paths for each value in `values`.

## Details

This function replaces all `NA` values in `paths` with '.' and then
retrieves the code path for each value in `values` using the matching
`cde_code` in `codes`.

## Examples

``` r
codes <- data.frame(cde_code = c(1L, 2L, 3L), cde_path = c('path1', 'path2', 'path3'))
values <- c(2L, 1L)
getCodePath(codes, values)
#> Error in getCodePath(codes, values): could not find function "getCodePath"
```
