# Wrapper function for string replacement using `[stringr::str_replace_all()]`

Wrapper function for string replacement using
`[stringr::str_replace_all()]`

## Usage

``` r
strReplaceAll(string, pattern, replacement)
```

## Arguments

- string:

  Input vector. Either a character vector, or something coercible to
  one.

- pattern:

  Pattern to look for.

  The default interpretation is a regular expression, as described in
  [stringi::about_search_regex](https://rdrr.io/pkg/stringi/man/about_search_regex.html).
  Control options with
  [`regex()`](https://stringr.tidyverse.org/reference/modifiers.html).

  For `str_replace_all()` this can also be a named vector
  (`c(pattern1 = replacement1)`), in order to perform multiple
  replacements in each element of `string`.

  Match a fixed string (i.e. by comparing only bytes), using
  [`fixed()`](https://stringr.tidyverse.org/reference/modifiers.html).
  This is fast, but approximate. Generally, for matching human text,
  you'll want
  [`coll()`](https://stringr.tidyverse.org/reference/modifiers.html)
  which respects character matching rules for the specified locale.

  You can not match boundaries, including `""`, with this function.

- replacement:

  The replacement value, usually a single string, but it can be the a
  vector the same length as `string` or `pattern`. References of the
  form `\1`, `\2`, etc will be replaced with the contents of the
  respective matched group (created by `()`).

  Alternatively, supply a function (or formula): it will be passed a
  single character vector and should return a character vector of the
  same length.

  To replace the complete string with `NA`, use
  `replacement = NA_character_`.

## Examples

``` r
strReplaceAll('Hello, world!', '[aeiou]', 'X')
#> Error in strReplaceAll("Hello, world!", "[aeiou]", "X"): could not find function "strReplaceAll"
```
