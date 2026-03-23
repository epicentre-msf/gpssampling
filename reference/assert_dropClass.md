# Assertive check for dropClass

This function checks if a given object belongs to a specific class and
raises an error if it does.

## Usage

``` r
assert_dropClass(x, what)
```

## Arguments

- x:

  An object

- what:

  The class to be dropped

## Examples

``` r
assert_dropClass('abc', 'character')
#> Error in assert_dropClass("abc", "character"): could not find function "assert_dropClass"
# Error: !any(class('abc') == 'character') is not TRUE
assert_dropClass(123L, 'numeric')
#> Error in assert_dropClass(123L, "numeric"): could not find function "assert_dropClass"
# Error: !any(class(123) == 'numeric') is not TRUE
```
