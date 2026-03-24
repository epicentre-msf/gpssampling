# Check if the current R session is running a Shiny server application

This function is used to determine whether the current R session is
running a Shiny server application. It internally calls the
[`isHostedApp`](https://epicentre-msf.github.io/gpssampling/reference/isHostedApp.md)
function to determine this.

## Usage

``` r
isShinyServerApp()
```

## Value

`TRUE` if the current R session is running a Shiny server application,
`FALSE` otherwise.

## Examples

``` r
isShinyServerApp()
#> Error in isShinyServerApp(): could not find function "isShinyServerApp"
```
