# Calculate Sample Zero-Inflated Distribution

This function calculates the population estimate and confidence interval
for a sample zero-inflated distribution.

## Usage

``` r
calculateSampleZeroInflatedDistribution(
  smp_pop,
  n = 10000L,
  error_confidence = 95L,
  progress = NULL
)
```

## Arguments

- smp_pop:

  numeric vector of the sample data

- n:

  integer value indicating the number of bootstrap samples to take
  (default is 10000)

- error_confidence:

  integer value indicating the error confidence level (default is 95)

- progress:

  a progress bar object created by
  [`progress_bar`](http://r-lib.github.io/progress/reference/progress_bar.md)
  (default is NULL)

## Value

a list with the following elements:

- pop:

  integer value of the estimated population

- pop_i:

  integer value of the population confidence interval

- lambda:

  numeric value of the estimated lambda parameter

- prob:

  numeric value of the estimated probability of zero inflation

## Examples

``` r
data <- c(1L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 5L, 5L, 5L, 5L, 6L, 6L, 7L, 7L, 7L, 7L, 7L, 7L, 8L, 8L, 8L, 8L, 9L, 9L, 9L, 9L, 10L, 10L)
calculateSampleZeroInflatedDistribution(data)
#> Error in calculateSampleZeroInflatedDistribution(data): could not find function "calculateSampleZeroInflatedDistribution"
```
