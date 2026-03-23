# Collapse a character vector into a single string with elements separated by a delimiter.

This function takes a character vector and collapses it into a single
string by concatenating its elements with a specified delimiter. By
default, each element is quoted using double quotes. The delimiter can
be specified using the `sep` parameter.

## Usage

``` r
collapse(x, quote = TRUE, sep = ", ")
```

## Arguments

- x:

  A character vector to be collapsed.

- quote:

  If `TRUE`, each element of `x` will be quoted using double quotes.
  Default is `TRUE`.

- sep:

  The delimiter to be used to separate the elements of `x`. Default is
  `', '`.

## Value

A single string with collapsed elements separated by the specified
delimiter.

## See also

[`paste`](https://rdrr.io/r/base/paste.html)

## Examples

``` r
collapse(c('a', 'b', 'c'))
#> Error in collapse(c("a", "b", "c")): could not find function "collapse"
# Output: 'a, b, c'

collapse(c('a', 'b', 'c'), quote = FALSE, sep = '-')
#> Error in collapse(c("a", "b", "c"), quote = FALSE, sep = "-"): could not find function "collapse"
# Output: a-b-c
```
