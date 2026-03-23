# Save Objects to a File

This function saves objects to a file using the
[`save()`](https://rdrr.io/r/base/save.html) function in R.

## Usage

``` r
saveit(..., file)
```

## Arguments

- ...:

  objects to be saved

- file:

  character string specifying the file name or path

## Examples

``` r
# save two variables to a file named 'my_data.RData'
saveit(var1, var2, file = 'my_data.RData')
#> Error in saveit(var1, var2, file = "my_data.RData"): could not find function "saveit"
```
