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
  default_distance = 50,
  seed,
  joint = FALSE,
  point_id_digits = NULL,
  print_table = TRUE,
  road_types = c("primary", "secondary", "tertiary", "residential", "trunk",
    "unclassified"),
  road_dir = NULL,
  starting_point = 1L,
  buffer_overlap = TRUE
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

  Minimum distance in meters between any two selected points. Either a
  single numeric value (applied to all communities) or a **named numeric
  vector** with per-community distances. Names must match entries in
  `buildings_list`. Any community not found in the named vector uses
  `default_distance`. Default `50`.

- default_distance:

  Numeric fallback distance in meters for communities not listed in a
  named `min_distance` vector. Ignored when `min_distance` is a scalar.
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

- point_id_digits:

  Integer or `NULL`. When set, creates a `named_point_id` column with
  zero-padded IDs of the given width (e.g., `point_id_digits = 3`
  produces `"001"`, `"002"`, ...). Used as display name in GPX/KML
  exports. Default `NULL` (no padding, exports use numeric `point_id`).

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

- starting_point:

  Integer. Starting number for `point_id` assignment. Useful when
  combining results from multiple survey rounds where IDs must not
  restart at 1. Default `1L`.

- buffer_overlap:

  Logical. Controls whether the circular buffers of radius
  `min_distance` around any two selected points are allowed to overlap.
  If `TRUE` (default), the standard `min_distance` exclusion radius is
  used. If `FALSE`, the exclusion radius is doubled to
  `2 * min_distance`, preventing any two buffers from touching or
  overlapping. Set to `FALSE` when field workers are instructed to
  sample any valid building inside the buffer if the original point is
  invalid, and you need to guarantee that no replacement building could
  fall within another point's buffer.

## Value

A named list. Each community element contains: `$buildings` (all
candidates), `$primary` (selected points with `selection_order` and
`point_id`), `$secondary` (replacement points with `selection_order` and
`point_id`, at most `n_required` per community), `$min_distance` (the
resolved per-community distance in meters), and `$seed`. When
`point_id_digits` is set, primary and secondary also carry a
`named_point_id` column with zero-padded string IDs. Both primary and
secondary are ordered by road proximity (nearest-neighbour chain). The
`point_id` column is globally unique across all communities and sets:
primary IDs are numbered 1..N_total_primary, secondary IDs continue from
N_total_primary + 1. When `print_table = TRUE`, the result carries two
attributes: `attr(, "summary_table")` (a
[`flextable::flextable()`](https://davidgohel.github.io/flextable/reference/flextable.html)
object ready for rendering in reports) and `attr(, "summary_df")` (the
underlying data frame). Access via `attr(result, "summary_table")`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Uniform distance for all communities
samples <- sample_communities(
  buildings_list,
  n_required = c(community_one = 30, community_two = 80),
  min_distance = 50,
  seed = 12345L
)

# Per-community distances (dense vs. sparse areas)
samples <- sample_communities(
  buildings_list,
  n_required = c(community_one = 30, community_two = 80),
  min_distance = c(community_one = 30, community_two = 80),
  seed = 12345L
)

# Per-community with a default fallback for unlisted communities
samples <- sample_communities(
  buildings_list,
  n_required = c(community_one = 30, community_two = 80),
  min_distance = c(community_one = 30),
  default_distance = 60,
  seed = 12345L
)

# Retrieve per-community distance from result
samples$community_one$min_distance # 30
samples$community_two$min_distance # 60 (default_distance)

# Joint sampling (less clustered secondary points)
samples <- sample_communities(
  buildings_list,
  n_required = c(community_one = 30, community_two = 80),
  min_distance = 50,
  seed = 12345L,
  joint = TRUE
)

# Shift point IDs to start at 300 (e.g., continuing from a prior round)
samples <- sample_communities(
  buildings_list,
  n_required = c(community_one = 30, community_two = 80),
  min_distance = 50,
  seed = 12345L,
  starting_point = 300L
)
samples$community_one$primary$point_id # starts at 300

# No buffer overlap: any two buffers of radius 50m never touch
# (effective exclusion radius becomes 100m = 2 * 50)
samples <- sample_communities(
  buildings_list,
  n_required = c(community_one = 30, community_two = 80),
  min_distance = 50,
  seed = 12345L,
  buffer_overlap = FALSE
)
} # }
```
