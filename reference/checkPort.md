# Function to check if a port is open on localhost

Function to check if a port is open on localhost

## Usage

``` r
checkPort(port, timeout = 0.1)
```

## Arguments

- port:

  Port number to be checked

- timeout:

  Timeout value for the connection attempt (default is 0.1 seconds)

## Value

Logical value indicating if the port is open

## Examples

``` r
checkPort(80L)
#> Error in checkPort(80L): could not find function "checkPort"
checkPort(443L, timeout = 0.5)
#> Error in checkPort(443L, timeout = 0.5): could not find function "checkPort"
```
