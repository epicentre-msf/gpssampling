# Empty Operator

This function returns the non-empty input among two given inputs.

## Usage

``` r
a %empty% b
```

## Arguments

- a:

  The first input.

- b:

  The second input.

## Value

The non-empty input between `a` and `b`.

## Examples

``` r
x <- 'Hello'
y <- ''
z <- x %empty% y
#> Error in x %empty% y: could not find function "%empty%"
# z will be 'Hello'

x <- ''
y <- 'World'
z <- x %empty% y
#> Error in x %empty% y: could not find function "%empty%"
# z will be 'World'

x <- ''
y <- ''
z <- x %empty% y
#> Error in x %empty% y: could not find function "%empty%"
# z will be ''
```
