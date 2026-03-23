# Logging

This function prints formatted text to the console.

## Usage

``` r
catf(fmt, ...)
```

## Arguments

- fmt:

  A character string containing format specifications.

- ...:

  Optional arguments to be formatted.

## Examples

``` r
catf('Hello, %s', 'world!')
#> Error in catf("Hello, %s", "world!"): could not find function "catf"
# Output: Hello, world!
```
