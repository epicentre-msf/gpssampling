# Check if the app is hosted on Shiny Server

This function checks whether the Shiny Server environment variable
'SHINY_SERVER_VERSION' is set and returns TRUE only if it is true, and
the application is running in a shiny session.

## Usage

``` r
isHostedApp()
```

## Value

Returns TRUE if the app is hosted on Shiny Server and FALSE otherwise.

## Examples

``` r
isHostedApp()
#> Error in isHostedApp(): could not find function "isHostedApp"
# expected output: FALSE
```
