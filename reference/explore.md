# Open File Explorer at Specified Path

This function opens the File Explorer at the specified path.

## Usage

``` r
explore(path = fs::path_home())
```

## Arguments

- path:

  A character string specifying the path to open File Explorer at.
  Default is the current working directory.

## Value

This function is used for its side effects and does not return any
value.

## Examples

``` r
explore()
#> Error in explore(): could not find function "explore"
explore('C:/Users/')
#> Error in explore("C:/Users/"): could not find function "explore"
```
