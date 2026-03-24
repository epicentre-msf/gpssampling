# Create a GPS sampler instance

Factory function for creating a
[GpsSampler](https://epicentre-msf.github.io/gpssampling/reference/GpsSampler.md)
object. This is the recommended way to instantiate the sampling
application.

## Usage

``` r
sampler(...)
```

## Arguments

- ...:

  Additional arguments passed to
  [GpsSampler](https://epicentre-msf.github.io/gpssampling/reference/GpsSampler.md)`$new()`.

## Value

A
[GpsSampler](https://epicentre-msf.github.io/gpssampling/reference/GpsSampler.md)
R6 object.

## Examples

``` r
if (FALSE) { # \dontrun{
samp <- sampler()
samp$launch()
} # }
```
