# Get Grouped Choices

This function takes in a dataframe containing values, names, and groups,
and returns choices grouped by their respective groups. It also allows
for an overall group to be specified and included in the output.

## Usage

``` r
getGroupedChoices(
  df_values,
  var_name = "label",
  var_code = NULL,
  var_group = NULL,
  overall = NULL
)
```

## Arguments

- df_values:

  A dataframe containing values, names, and groups.

- var_name:

  The name of the column containing choice names.

- var_code:

  The name of the column containing choice values. If NULL, row names
  are used.

- var_group:

  The name of the column containing choice groups.

- overall:

  The name of the overall group. If specified, an overall group will be
  included in the output.

## Value

A named list of choices, grouped by their respective groups.

## Examples

``` r
df <- data.frame(
  var_code = c(1L, 2L, 3L, 4L),
  var_name = c('Choice A', 'Choice B', 'Choice C', 'Choice D'),
  var_group = c('Group 1', 'Group 1', 'Group 2', 'Group 2')
)
getGroupedChoices(df)
#> Error in getGroupedChoices(df): could not find function "getGroupedChoices"
```
