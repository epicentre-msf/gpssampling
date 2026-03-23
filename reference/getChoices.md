# Get choices for labelling

This function is used to generate choices for labelling purposes. It
takes in a data frame of values and outputs a list of choices with
corresponding labels.

## Usage

``` r
getChoices(df_values, var_name = "label", var_code = NULL, overall = NULL)
```

## Arguments

- df_values:

  A data frame of values.

- var_name:

  The variable name for the labels. Default is 'label'.

- var_code:

  The variable code for the choices. If not provided, row names are
  used.

- overall:

  The overall label for all choices. If provided, it is added to the
  list of choices with a code of 'T'.

## Value

A list of choices with corresponding labels.

## See also

`label` for labelling.

## Examples

``` r
df <- data.frame(var1 = c('A', 'B', 'C'), var2 = c(1L, 2L, 3L))
choices <- getChoices(df, 'var1', 'var2', overall = 'Overall')
#> Error in getChoices(df, "var1", "var2", overall = "Overall"): could not find function "getChoices"
print(choices)
#> Error: object 'choices' not found
```
