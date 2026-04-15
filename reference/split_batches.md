# Split sample points into batches

Distributes primary or secondary sample points into numbered batches
using round-robin assignment. Useful for dividing field work across
multiple teams.

## Usage

``` r
split_batches(samples_list, n_batches, set = c("primary", "secondary"))
```

## Arguments

- samples_list:

  Named list of communities, output of
  [`sample_communities()`](https://epicentre-msf.github.io/gpssampling/reference/sample_communities.md).

- n_batches:

  Integer (applied to all communities) or named integer vector (per
  community). E.g., `5L` or `c(community_one = 5, community_two = 3)`.

- set:

  Which point set to split: `"primary"` (default) or `"secondary"`.

## Value

A named list of communities. Each element is a list with:

- `batches`:

  `sf` POINT with `assigned_batch` column.

- `min_distance`:

  Buffer radius in meters.

- `n_buildings`:

  Total building count in the community.

- `seed`:

  Per-community RNG seed.

- `n_batches`:

  Number of batches.

- `buildings`:

  `sf` of all candidate buildings.

## Examples

``` r
if (FALSE) { # \dontrun{
batched <- split_batches(samples, n_batches = 5L, set = "primary")
} # }
```
