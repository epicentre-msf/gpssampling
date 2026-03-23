# Find a free port to use

Uses the OS to find a free TCP port by binding to port 0 (which lets the
OS assign any available port), then closes the connection and returns
the assigned port. This approach works on all platforms (Windows, macOS,
Linux).

## Usage

``` r
findFreePort(start = 1024L, random = TRUE)
```

## Arguments

- start:

  The minimum port number (default 1024 to avoid privileged ports). When
  `random = FALSE`, sequential search starts here.

- random:

  Logical. If `TRUE` (recommended), lets the OS pick any free port. If
  `FALSE`, searches sequentially from `start`.

## Value

An integer: a free port number.

## Examples

``` r
findFreePort(6000L, TRUE)
#> Error in findFreePort(6000L, TRUE): could not find function "findFreePort"
findFreePort(6000L)
#> Error in findFreePort(6000L): could not find function "findFreePort"
```
