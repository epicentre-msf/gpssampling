# Get code labels

This function returns the labels corresponding to a set of codes from a
given data frame. If a code does not have a corresponding label, it will
be replaced with a dot.

## Usage

``` r
getCodeLabels(codes, values, short = FALSE)
```

## Arguments

- codes:

  A data frame containing the code labels.

- values:

  A vector of codes for which labels are sought.

- short:

  If TRUE, the short labels will be returned instead of the full labels.
  Default is FALSE.

## Value

A vector of labels corresponding to the given codes.

## Examples

``` r
codes <- data.frame(
  cde_code = c(1L, 2L, 3L),
  cde_label = c('Label 1', 'Label 2', 'Label 3'),
  cde_label_short = c('L1', 'L2', 'L3')
)
values <- c(1L, NA, 2L)
getCodeLabels(codes, values) # returns: 'Label 1'  '.'  'Label 2'
#> Error in getCodeLabels(codes, values): could not find function "getCodeLabels"
getCodeLabels(codes, values, short = TRUE) # returns: 'L1'  '.'  'L2'
#> Error in getCodeLabels(codes, values, short = TRUE): could not find function "getCodeLabels"
```
