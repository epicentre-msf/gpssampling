# Convert values to character string with specified format

This function takes a vector of numeric values and converts them to
character string using the specified format. The format can be any valid
format specification used in the `sprintf` function. Any occurrences of
'NA' in the resulting string are replaced with '-'.

## Usage

``` r
snumber(x, format = "%f")
```

## Arguments

- x:

  A vector of numeric values

- format:

  A character string specifying the format to use for converting the
  values. Default is "%f".

## Value

A character vector with the converted values

## Examples

``` r
snumber(1:5, '%.2f')
#> Error in snumber(1:5, "%.2f"): could not find function "snumber"
snumber(c(1.234, NA), '%0.3f')
#> Error in snumber(c(1.234, NA), "%0.3f"): could not find function "snumber"
```
