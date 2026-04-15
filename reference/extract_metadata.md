# Extract metadata from batched sampling results

Combines primary and (optionally) secondary batched results into a
single data frame with per-point metadata. Useful for creating field
assignment sheets.

## Usage

``` r
extract_metadata(primary, secondary = NULL)
```

## Arguments

- primary:

  Output of
  [`split_batches()`](https://epicentre-msf.github.io/gpssampling/reference/split_batches.md)
  for the primary set.

- secondary:

  Output of
  [`split_batches()`](https://epicentre-msf.github.io/gpssampling/reference/split_batches.md)
  for the secondary set. Default `NULL` (primary only).

## Value

A `data.frame` with columns: `community`, `point_id`, `named_point_id`
(when available), `assigned_batch`, `set`. Carries two attributes:
`buffer_size` (named numeric vector of per-community buffer radii) and
`n_teams` (named integer vector of per-community batch counts).

## Examples

``` r
if (FALSE) { # \dontrun{
meta <- extract_metadata(primary_batches, secondary_batches)
attr(meta, "buffer_size")
attr(meta, "n_teams")
} # }
```
