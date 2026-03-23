# Get table of listening TCP ports

Returns a tibble of listening TCP ports with host, port, and PID
columns. Works on Windows (netstat), macOS (lsof), and Linux (ss or
lsof).

## Usage

``` r
getTablePorts()
```

## Value

A tibble with columns: locale_host, locale_port, pid

## Examples

``` r
getTablePorts()
#> Error in getTablePorts(): could not find function "getTablePorts"
```
