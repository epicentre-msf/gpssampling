# Get Labels

This function extracts labels from a data frame based on a code and
variable name.

## Usage

``` r
getLabels(df_values, code, var_name = "label")
```

## Arguments

- df_values:

  A data frame containing the values and labels.

- code:

  The code used to identify the label.

- var_name:

  The name of the variable which contains the labels. Default is
  'label'.

## Value

A vector of labels corresponding to the given code.

## Examples

``` r
df <- data.frame(code = c('A', 'B', 'C'), label = c('Label A', 'Label B', 'Label C'))
getLabels(df, 'B')
#> Error in getLabels(df, "B"): could not find function "getLabels"
```
