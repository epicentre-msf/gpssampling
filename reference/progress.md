# Progress

This function allows you to track progress by printing the current
value. By default, the function starts at 1 and increments by 1 each
time it is called.

## Usage

``` r
progress(begin = FALSE, value = 1L, increment = 1L)
```

## Arguments

- begin:

  logical. If `TRUE`, the progress counter is set to the initial value
  specified by `value` and `increment`.

- value:

  numeric. The initial value of the progress counter.

- increment:

  numeric. The amount by which the progress counter should be
  incremented.

## Examples

``` r
progress(begin = TRUE, value = 1L, increment = 2L)
#> Error in progress(begin = TRUE, value = 1L, increment = 2L): could not find function "progress"
progress() # Prints '3'
#> Error in progress(): could not find function "progress"
progress() # Prints '5'
#> Error in progress(): could not find function "progress"
progress(begin = TRUE, value = 10L, increment = -1L)
#> Error in progress(begin = TRUE, value = 10L, increment = -1L): could not find function "progress"
progress() # Prints '9'
#> Error in progress(): could not find function "progress"
progress() # Prints '8'
#> Error in progress(): could not find function "progress"
```
