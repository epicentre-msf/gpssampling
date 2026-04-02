# Programmatic Sampling Pipeline

## Overview

The `gpssampling` package includes a **programmatic sampling pipeline**
that runs entirely in R scripts – no Shiny app required. This is useful
for:

- Reproducible, scripted sampling workflows
- Batch processing multiple survey areas
- Integration with existing analysis pipelines
- Generating publication-quality static maps

The pipeline has three stages:

1.  **Sampling** – fetch buildings, filter, crop to communities, and
    sample with minimum-distance constraints.
2.  **GPS Management** – split into batches, create buffers, export to
    GPX / GeoPackage / SQLite tile overlays, zip, and email.
3.  **Static Mapping** – generate per-community and overview maps with
    basemap tiles, batch coloring, and scale bars.

## Stage 1: Sampling

### Fetch and filter buildings

``` r
library(gpssampling)
library(sf)

# Load your study area boundary and community polygons
state_boundary <- st_read("boundary.gpkg")
communities <- st_read("communities.gpkg")

# Download OSM building footprints
osm_buildings <- fetch_osm_buildings(state_boundary)

# Filter out non-residential buildings (hospitals, schools, etc.)
buildings <- filter_buildings(osm_buildings)
```

If you have your own building footprint dataset (e.g., from Microsoft
Building Footprints or Google Open Buildings), use OSM only for
labeling:

``` r
user_buildings <- st_read("my_building_footprints.gpkg")
osm <- fetch_osm_buildings(state_boundary)

buildings <- filter_buildings(
  user_buildings,
  osm_buildings_sf = osm,
  remove_tags = c("hospital", "school", "church")
)
```

### Crop to communities

[`crop_buildings()`](https://epicentre-msf.github.io/gpssampling/reference/crop_buildings.md)
intersects buildings with community polygons, converts to centroids, and
returns a named list (one entry per community):

``` r
buildings_list <- crop_buildings(
  buildings,
  communities,
  community_id_col = "name"
)

# Check counts per community
vapply(buildings_list, nrow, integer(1L))
#> community_one community_two community_three community_four
#>           847          1203             956            612
```

### Visualize cropped buildings

[`map_cropped_buildings()`](https://epicentre-msf.github.io/gpssampling/reference/map_cropped_buildings.md)
produces one large map per community showing the community boundary and
actual building footprint shapes over an OSM basemap. Pass the original
building polygons – the function clips them to each community
internally:

``` r
crop_maps <- map_cropped_buildings(
  buildings,
  communities,
  community_id_col = "name",
  out_dir = "output/maps"
)
# Or save individually:
# ggplot2::ggsave("community_one.png", crop_maps[["community_one"]],
#   width = 12, height = 12, dpi = 300)
```

### Sample with minimum-distance constraints

[`sample_communities()`](https://epicentre-msf.github.io/gpssampling/reference/sample_communities.md)
selects points using a two-phase approach:

1.  **Random selection** – all points (including the first) are chosen
    at random, subject to the `min_distance` constraint so that no two
    selected points are closer than `min_distance` meters.
2.  **Proximity ordering** – after selection, the points are reordered
    for efficient field work: the selected point closest to an OSM road
    becomes \#1, then the nearest unvisited selected point becomes \#2,
    and so on (greedy nearest-neighbour chain). This `selection_order`
    determines batch assignment and the sequence field workers follow on
    the ground.

``` r
samples <- sample_communities(
  buildings_list,
  n_required = c(
    community_one = 30, community_two = 80,
    community_three = 85, community_four = 60
  ),
  min_distance = 50,
  seed = 250292L
)
```

The result is a named list of lists. Each community contains `$primary`
(selected points with `selection_order`), `$secondary` (remaining
points), `$buildings` (all candidates), and metadata (`$min_distance`,
`$seed`).

#### Joint sampling (less clustered secondary points)

By default, primary and secondary points are drawn independently. The
secondary draw restarts on the reduced pool, which can cluster when the
pool is sparse. Pass `joint = TRUE` to draw both sets in a single pass:

``` r
samples <- sample_communities(
  buildings_list,
  n_required = c(community_one = 80, community_two = 60),
  min_distance = 50,
  seed = 250292L,
  joint = TRUE
)
```

With `joint = TRUE`, the algorithm draws `2 * n_required` points in one
call, enforcing the distance constraint across all of them. The first
`n_required` drawn become primary; the rest become secondary. If the
pool is too small for the full secondary set, all remaining points fill
in.

**Reproducibility:** Same `seed` + same inputs = same output on any
platform. The algorithm uses
[`withr::with_seed()`](https://withr.r-lib.org/reference/with_seed.html)
(not [`set.seed()`](https://rdrr.io/r/base/Random.html)), deterministic
point sorting, and alphabetical community processing order.

## Stage 2: GPS Point Management

### Split into batches

Distribute points across field teams using round-robin assignment:

``` r
batched <- split_batches(samples, n_batches = 5L, set = "primary")
```

Each community’s points get an `assigned_batch` column (1 to 5).

If different communities need different team counts, pass a named
vector:

``` r
batched <- split_batches(
  samples,
  n_batches = c(community_one = 5, community_two = 3, community_three = 4),
  set = "primary"
)
```

### Create buffers

Generate circular buffer polygons around sampled points:

``` r
buffers <- create_buffers(samples, radius = 50, set = "primary")
```

### Export to disk

[`export_points()`](https://epicentre-msf.github.io/gpssampling/reference/export_points.md)
writes points, buffer polygons, and OsmAnd-compatible SQLite tile
overlays to a structured directory:

``` r
manifest <- export_points(
  batched,
  out_dir = "output",
  formats = c("gpkg", "gpx"),
  include_buffers = TRUE,
  buffer_radius = 50,
  set = "primary"
)
```

This creates:

    output/
      primary/
        community_one/
          community_one_primary_all.gpkg
          community_one_primary_batch_1.gpkg
          community_one_primary_batch_1.gpx
          community_one_buffers_all.gpkg
          community_one_buffers_all.gpx
          community_one_buffers_all.sqlitedb      <- OsmAnd tile overlay
          community_one_buffers_batch_1.gpkg
          community_one_buffers_batch_1.gpx
          community_one_buffers_batch_1.sqlitedb
          ...
        community_two/
          ...

**OsmAnd integration:** Copy `.sqlitedb` files to
`Android/data/net.osmand/files/tiles/` on the tablet and enable via
Configure map \> Overlay/Underlay.

### Zip and email

``` r
# Zip GPX + SQLite files for field distribution
zips <- zip_points("output", prefix = "project-", sets = "primary")

# Send via Outlook (requires emayili package + SMTP credentials)
email_points(
  zips,
  to = "fieldteam@example.org",
  subject = "Survey - GPS Sampling Points"
)
```

## Stage 3: Static Mapping

Generate publication-quality static maps using `ggplot2` + basemap
tiles:

``` r
# Generate all maps (overview + per-community for primary and secondary)
maps <- map_all_communities(
  samples,
  communities,
  community_id_col = "name",
  out_dir = "output/maps",
  buffer_radius = 50
)
```

This creates:

- `overview.png` – all communities, uniform point color, community
  labels
- `{name}_primary.png` – batch-colored primary points per community
- `{name}_secondary.png` – batch-colored secondary points per community

You can also generate individual maps:

``` r
# Single community map
p <- map_community(
  "community_one",
  communities[communities$name == "community_one", ],
  samples$community_one$primary,
  buffers_sf = create_buffers(samples$community_one$primary, radius = 50),
  batch_colors = TRUE
)

# Customize before saving
p + ggplot2::theme(legend.position = "bottom")
ggplot2::ggsave("community_one.png", p, width = 10, height = 12, dpi = 300)
```

### Interactive leaflet map

[`leaflet_communities()`](https://epicentre-msf.github.io/gpssampling/reference/leaflet_communities.md)
creates an interactive map with layer toggles, community navigation,
fullscreen, and multiple base maps:

``` r
pri <- split_batches(samples, n_batches = 5L, set = "primary")
sec <- split_batches(samples, n_batches = 5L, set = "secondary")
roads <- fetch_community_roads(
  communities, community_id_col = "name",
  road_dir = "output/roads"
)

m <- leaflet_communities(
  pri,
  communities,
  community_id_col = "name",
  secondary_batches = sec,
  buildings_list = buildings_cropped,
  roads_list = roads,
  out_file = "output/maps/sampling_map.html"
)
```

Features:

- **Z-ordered layers**: communities (bottom) \> roads \> buildings \>
  buffers \> points (top)
- **Grouped toggles**: “Primary Points”, “Secondary Points”,
  “Buildings”, “Roads”
- **Distinct shapes**: circles for primary, triangles for secondary
- **Community navigation**: panel with quick-zoom buttons per community
- **Fullscreen**: via `leaflet.extras` (if installed)
- **Buildings**: hidden by default (toggle via layer control)

## Complete Workflow

``` r
library(gpssampling)
library(sf)

# 1. Load data
communities <- st_read("communities.gpkg")
state_boundary <- st_read("boundary.gpkg")

# 2. Fetch, filter, crop buildings
buildings <- fetch_osm_buildings(state_boundary)
buildings_filtered <- filter_buildings(buildings)
buildings_list <- crop_buildings(
  buildings_filtered, communities, community_id_col = "name"
)

# 3. Sample (seed is required — choose any integer for reproducibility)
samples <- sample_communities(
  buildings_list,
  n_required = c(
    community_one = 30, community_two = 80,
    community_three = 85, community_four = 60
  ),
  min_distance = 50,
  seed = 250292L,
  joint = TRUE
)

# 4. Batch and export
batched <- split_batches(samples, n_batches = 5L, set = "primary")
export_points(batched, out_dir = "output", set = "primary")
export_points(
  split_batches(samples, n_batches = 5L, set = "secondary"),
  out_dir = "output", set = "secondary"
)

# 5. Zip for field teams
zips <- zip_points("output", prefix = "project-")

# 6. Generate static maps
map_all_communities(
  samples, communities,
  community_id_col = "name",
  out_dir = "output/maps"
)

# 7. Generate interactive map
roads <- fetch_community_roads(
  communities, community_id_col = "name",
  road_dir = "output/roads"
)
leaflet_communities(
  batched, communities,
  secondary_batches = split_batches(samples, n_batches = 5L, set = "secondary"),
  buildings_list = buildings_cropped,
  roads_list = roads,
  out_file = "output/maps/sampling_map.html"
)
```

## Function Reference

### Sampling

| Function                                                                                                    | Purpose                                                        |
|-------------------------------------------------------------------------------------------------------------|----------------------------------------------------------------|
| [`fetch_osm_buildings()`](https://epicentre-msf.github.io/gpssampling/reference/fetch_osm_buildings.md)     | Download OSM building footprints for an area                   |
| [`filter_buildings()`](https://epicentre-msf.github.io/gpssampling/reference/filter_buildings.md)           | Remove non-residential buildings by OSM tags                   |
| [`crop_buildings()`](https://epicentre-msf.github.io/gpssampling/reference/crop_buildings.md)               | Clip buildings to community polygons, convert to centroids     |
| [`find_start_points()`](https://epicentre-msf.github.io/gpssampling/reference/find_start_points.md)         | Find closest-to-road building per community (utility)          |
| [`sample_communities()`](https://epicentre-msf.github.io/gpssampling/reference/sample_communities.md)       | Random sampling with proximity-based ordering (`joint` option) |
| [`fetch_community_roads()`](https://epicentre-msf.github.io/gpssampling/reference/fetch_community_roads.md) | Pre-download OSM roads per community (with caching)            |

### GPS Management

| Function                                                                                                | Purpose                                                     |
|---------------------------------------------------------------------------------------------------------|-------------------------------------------------------------|
| [`split_batches()`](https://epicentre-msf.github.io/gpssampling/reference/split_batches.md)             | Assign round-robin batch numbers (uniform or per-community) |
| [`create_buffers()`](https://epicentre-msf.github.io/gpssampling/reference/create_buffers.md)           | Generate circular buffer polygons                           |
| [`create_buffer_tiles()`](https://epicentre-msf.github.io/gpssampling/reference/create_buffer_tiles.md) | Create OsmAnd-compatible SQLite tile overlays               |
| [`export_points()`](https://epicentre-msf.github.io/gpssampling/reference/export_points.md)             | Export points, buffers, and tiles to disk                   |
| [`zip_points()`](https://epicentre-msf.github.io/gpssampling/reference/zip_points.md)                   | Bundle GPX + SQLite files into zip archives                 |
| [`email_points()`](https://epicentre-msf.github.io/gpssampling/reference/email_points.md)               | Send zip files via SMTP email                               |

### Mapping

| Function                                                                                                    | Purpose                                       |
|-------------------------------------------------------------------------------------------------------------|-----------------------------------------------|
| [`map_cropped_buildings()`](https://epicentre-msf.github.io/gpssampling/reference/map_cropped_buildings.md) | Building footprint maps per community         |
| [`map_community()`](https://epicentre-msf.github.io/gpssampling/reference/map_community.md)                 | Per-community static map with batch coloring  |
| [`map_overview()`](https://epicentre-msf.github.io/gpssampling/reference/map_overview.md)                   | Zoomed-out static overview of all communities |
| [`map_all_communities()`](https://epicentre-msf.github.io/gpssampling/reference/map_all_communities.md)     | Generate and optionally save all static maps  |
| [`leaflet_communities()`](https://epicentre-msf.github.io/gpssampling/reference/leaflet_communities.md)     | Interactive leaflet map with layer controls   |
