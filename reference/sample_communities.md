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
  joint = FALSE,
  print_table = TRUE,
  road_types = c("primary", "secondary", "tertiary", "residential", "trunk",
    "unclassified"),
  road_dir = NULL
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

- joint:

  Logical. If `TRUE`, primary and secondary points are drawn together in
  a single pass, enforcing the minimum distance across both sets. This
  reduces clustering in the secondary points. The first `n_required`
  drawn become primary; the rest become secondary. Default `FALSE`
  (independent draws).

- print_table:

  Logical. If `TRUE` (default), prints a
  [`flextable::flextable()`](https://davidgohel.github.io/flextable/reference/flextable.html)
  summary at the end of sampling with per-community statistics:
  buildings available, points drawn, distance metrics, constraint
  violations, and coverage.

- road_types:

  Character vector of OSM `highway=*` values used for the post-selection
  proximity ordering.

- road_dir:

  Optional directory for cached road files. If provided, roads are read
  from / saved to `road_dir/{community_name}.gpkg`. Use
  [`fetch_community_roads()`](https://epicentre-msf.github.io/gpssampling/reference/fetch_community_roads.md)
  to pre-download roads. Default `NULL` (no caching).

## Value

A named list. Each community element contains: `$buildings` (all
candidates), `$primary` (selected points with `selection_order` and
`point_id`), `$secondary` (replacement points with `selection_order` and
`point_id`, at most `n_required` per community), `$min_distance`, and
`$seed`. Both primary and secondary are ordered by road proximity
(nearest-neighbour chain). The `point_id` column is globally unique
across all communities and sets: primary IDs are numbered
1..N_total_primary, secondary IDs continue from N_total_primary + 1.
When `print_table = TRUE`, the result carries two attributes:
`attr(, "summary_table")` (a
[`flextable::flextable()`](https://davidgohel.github.io/flextable/reference/flextable.html)
object ready for rendering in reports) and `attr(, "summary_df")` (the
underlying data frame). Access via `attr(result, "summary_table")`.

## Examples

``` r
if (FALSE) { # \dontrun{
samples <- sample_communities(
  buildings_list,
  n_required = c(community_one = 30, community_two = 80),
  min_distance = 50,
  seed = 12345L
)

# Joint sampling (less clustered secondary points)
samples <- sample_communities(
  buildings_list,
  n_required = c(community_one = 30, community_two = 80),
  min_distance = 50,
  seed = 12345L,
  joint = TRUE
)
} # }
```
