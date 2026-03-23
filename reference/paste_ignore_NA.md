# Paste Strings with Ignoring NA Values

This function concatenates strings together, while ignoring any NA
values.

## Usage

``` r
paste_ignore_NA(..., sep = " ", sort = FALSE, unique = FALSE)
```

## Arguments

- ...:

  A series of character vectors that need to be concatenated.

- sep:

  The separator to be used between each element in the resulting string
  (default is a space).

- sort:

  Logical indicating whether to sort the elements in the resulting
  string (default is FALSE).

- unique:

  Logical indicating whether to remove duplicates from the resulting
  string (default is FALSE).

## Value

A character vector with the concatenated strings, where any NA values
are ignored.

## Examples

``` r
paste_ignore_NA('a', 'b', NA, 'c')
#> Error in paste_ignore_NA("a", "b", NA, "c"): could not find function "paste_ignore_NA"
# Result: 'a b c'
paste_ignore_NA('a', 'b', NA, 'c', sep = '-')
#> Error in paste_ignore_NA("a", "b", NA, "c", sep = "-"): could not find function "paste_ignore_NA"
# Result: 'a-b-c'
paste_ignore_NA('a', 'b', NA, 'c', sort = TRUE)
#> Error in paste_ignore_NA("a", "b", NA, "c", sort = TRUE): could not find function "paste_ignore_NA"
# Result: 'a b c'
paste_ignore_NA('a', 'b', NA, 'c', unique = TRUE)
#> Error in paste_ignore_NA("a", "b", NA, "c", unique = TRUE): could not find function "paste_ignore_NA"
# Result: 'a b c'
```
