# Calculate a score for uniqueness of elements in a character vector

This function calculates a score for the uniqueness of elements in a
character vector `x`. The score is a value between 0 and 1, where 0
indicates no unique elements and 1 indicates all elements are unique.
The uniqueness is determined by counting unique elements in `x` and the
unique elements after applying the Soundex algorithm. If `x` is not a
character vector, the function returns 0.

## Usage

``` r
score_unique(x)
```

## Arguments

- x:

  A character vector

## Value

A numeric score for the uniqueness of elements in `x`

## References

Levenshtein, V. (1966). Binary codes capable of correcting deletions,
insertions, and reversals. Soviet Physics Doklady, 10(8), 707-710.

## See also

[`stringdist::phonetic`](https://rdrr.io/pkg/stringdist/man/phonetic.html)

## Examples

``` r
score_unique(c('cat', 'dog', 'cat', 'pig'))
#> Error in score_unique(c("cat", "dog", "cat", "pig")): could not find function "score_unique"
score_unique(c('John', 'Doe', 'Jane'))
#> Error in score_unique(c("John", "Doe", "Jane")): could not find function "score_unique"
score_unique(1:10)
#> Error in score_unique(1:10): could not find function "score_unique"
```
