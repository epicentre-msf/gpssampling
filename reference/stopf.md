# Stop Function

This function creates an error object with the given message and stops
execution.

## Usage

``` r
stopf(msg, ...)
```

## Arguments

- msg:

  A character string representing the error message.

- ...:

  Additional arguments to be passed to the `sprintf` function.

## Value

This function does not return a value. It stops the execution by
throwing an error.

## Examples

``` r
stopf('This is an error message')
#> Error in stopf("This is an error message"): could not find function "stopf"
stopf('Invalid input: %s', input)
#> Error in stopf("Invalid input: %s", input): could not find function "stopf"
```
