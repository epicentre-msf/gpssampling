# Get coded factors from codes and values.

This function takes two input vectors, `codes` and `values`, and returns
a factor variable based on the values and codes. The function can also
take an additional argument `short` which, when set to `TRUE`, returns
factors with short labels. By default, the function returns factors with
full labels.

## Usage

``` r
getCodeFactors(codes, values, short = FALSE)
```

## Arguments

- codes:

  A data frame or tibble containing code and label information.

- values:

  A vector of values to be converted into factors.

- short:

  Logical value indicating whether to use short labels (default is
  `FALSE`).

## Value

A factor variable created from the values and codes.

## Examples

``` r
codes <- data.frame(cde_code = c(1L, 2L, 3L), cde_label = c('Low', 'Medium', 'High'), cde_label_short = c('L', 'M', 'H'))
values <- c(1L, 3L)
getCodeFactors(codes, values)
#> Error in getCodeFactors(codes, values): could not find function "getCodeFactors"
getCodeFactors(codes, values, short = TRUE)
#> Error in getCodeFactors(codes, values, short = TRUE): could not find function "getCodeFactors"
```
