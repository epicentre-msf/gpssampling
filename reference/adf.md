# bash alias for [`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html)

bash alias for
[`as.data.frame`](https://rdrr.io/r/base/as.data.frame.html)

## Usage

``` r
adf(x, row.names = NULL, optional = FALSE, ...)
```

## Arguments

- x:

  any R object.

- row.names:

  `NULL` or a character vector giving the row names for the data frame.
  Missing values are not allowed.

- optional:

  logical. If `TRUE`, setting row names and converting column names (to
  syntactic names: see
  [`make.names`](https://rdrr.io/r/base/make.names.html)) is optional.
  Note that all of R's base package
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) methods
  use `optional` only for column names treatment, basically with the
  meaning of
  [`data.frame`](https://rdrr.io/r/base/data.frame.html)`(*, check.names = !optional)`.
  See also the `make.names` argument of the `matrix` method.

- ...:

  additional arguments to be passed to or from methods.
