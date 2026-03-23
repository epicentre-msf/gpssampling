# Get code values

This function retrieves the code values for a given list of labels from
a codes data frame.

## Usage

``` r
getCodeValues(codes, labels, short = FALSE)
```

## Arguments

- codes:

  A data frame containing the codes and labels.

- labels:

  A vector of labels for which the code values should be retrieved.

- short:

  A logical value indicating whether the short labels should be used.
  Default is `FALSE`.

## Value

A vector of corresponding code values for the given labels.

## Examples

``` r
codes <- data.frame(
  cde_code = c(1L, 2L, 3L), cde_label = c('Label 1', 'Label 2', 'Label 3'),
  cde_label_short = c('L1', 'L2', 'L3')
)
labels <- c('Label 1', 'Label 2', NA, 'Label 3')
getCodeValues(codes, labels) # returns: 1, 2, NA, 3
#> Error in getCodeValues(codes, labels): could not find function "getCodeValues"
getCodeValues(codes, labels, short = TRUE) # returns: L1, L2, NA, L3
#> Error in getCodeValues(codes, labels, short = TRUE): could not find function "getCodeValues"
```
