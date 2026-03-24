# Sample buildings across communities

Top-level function that orchestrates reproducible spatial sampling
across all communities. Points are selected randomly with minimum-
distance constraints, then reordered by proximity to roads for efficient
field work.

## Usage

``` r
sample_communities(
  buildings_list,
  n_required,
  min_distance = 50,
  seed,
  road_types = c("primary", "secondary", "tertiary", "residential", "trunk",
    "unclassified")
)
```

## Arguments

- buildings_list:

  Named list of `sf` POINT objects (output of
  [`crop_buildings()`](https://epicentre-msf.github.io/gpssampling/reference/crop_buildings.md)).

- n_required:

  Named integer vector of required sample sizes per community. Names
  must match `buildings_list`. A single unnamed integer applies the same
  size to all communities.

- min_distance:

  Numeric, minimum distance in meters between any two selected points.
  Default `50`.

- seed:

  Integer RNG seed for reproducibility (**required**, no default). A
  per-community seed is derived from `seed` and the community name, so
  adding or removing a community does not change the selection in other
  communities. Results are reproducible across machines given the same
  seed, input data, and R version (\>= 3.6.0). R 3.6.0 changed the
  default sampling algorithm (`sample.kind = "Rejection"`), so results
  from R \< 3.6 and R \>= 3.6 will differ even with the same seed.

- road_types:

  Character vector of OSM `highway=*` values used for the post-selection
  proximity ordering.

## Value

A named list of lists. Each community element contains: `$buildings`
(all candidates), `$primary` (selected points with `selection_order`),
`$secondary` (remaining points), `$min_distance`, and `$seed`.

## Examples

``` r
if (FALSE) { # \dontrun{
samples <- sample_communities(
  buildings_list,
  n_required = c(community_one = 30, community_two = 80),
  min_distance = 50,
  seed = 12345L
)
} # }
```
