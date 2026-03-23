# Drop a class from an object

This function can be used to drop a specific class from an object.

## Usage

``` r
dropClass(x, what)
```

## Arguments

- x:

  An object

- what:

  The class to drop

## Value

An object with the specified class removed

## Examples

``` r
x <- structure(1L, class = c('a', 'b'))
dropClass(x, 'b')
#> Error in dropClass(x, "b"): could not find function "dropClass"
```
