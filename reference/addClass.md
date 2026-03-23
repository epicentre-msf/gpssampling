# Add a class to an object

This function adds a new class to an object.

## Usage

``` r
addClass(x, what)
```

## Arguments

- x:

  An object

- what:

  The name of the class to add

## Value

The object with the new class

## Examples

``` r
x <- 1L
addClass(x, 'foo')
#> Error in addClass(x, "foo"): could not find function "addClass"
# Output:
# [1] 1
# attr(,'class')
# [1] 'numeric' 'foo'
class(x)
#> [1] "integer"
# Output:
# [1] 'numeric' 'foo'
```
