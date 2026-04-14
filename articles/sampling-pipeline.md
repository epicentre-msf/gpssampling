# GPS Sampling Pipeline: A Complete Guide

## Introduction

The `gpssampling` package provides a **programmatic sampling pipeline**
for selecting buildings within community polygons for field epidemiology
surveys. Everything runs from R scripts – no Shiny app required.

This vignette is a **complete, step-by-step guide** designed for someone
who has never used the package before. It covers every function in the
pipeline, explains what each one does, shows its key parameters, and
walks through a realistic workflow from start to finish.

### What the pipeline does

At a high level, the pipeline takes:

- A **study area boundary** (polygon shapefile, GeoPackage, etc.)
- **Community polygons** (one per survey community)

And produces:

- **Sampled GPS points** with minimum-distance constraints
- **Batch assignments** for dividing work across field teams
- **Export files** (GPX, GeoPackage, KML) ready for GPS devices and
  OsmAnd
- **Static maps** (PNG) for reports and briefings
- **Interactive maps** (HTML) for digital review

### What you need before starting

**R packages:**

``` r
# Install gpssampling (from GitHub)
remotes::install_github("epicentre-msf/gpssampling")

# Suggested packages for mapping (installed automatically if missing)
install.packages(c("ggplot2", "ggspatial", "tidyterra"))
```

**Data files:**

You need two spatial files:

1.  **Study area boundary** – a polygon covering your entire area of
    interest. This is used to download building footprints from
    OpenStreetMap.
2.  **Community polygons** – one polygon per community you want to
    sample. Each polygon must have a column with a unique community
    name.

Both can be GeoPackage (`.gpkg`), Shapefile (`.shp`), GeoJSON, or any
format readable by
[`sf::st_read()`](https://r-spatial.github.io/sf/reference/st_read.html).
Any coordinate reference system (CRS) works – the package reprojects
internally as needed.

**Internet connection:**

Required for downloading building footprints and road networks from
OpenStreetMap. Once downloaded, everything else works offline.

## Step 1: Load Your Data

``` r
library(gpssampling)
library(sf)

# Load your study area boundary
state_boundary <- st_read("boundary.gpkg")

# Load community polygons
# The file must have a column that uniquely identifies each community.
# In this example, the column is called "name".
communities <- st_read("communities.gpkg")

# Check what the community data looks like
print(communities)
#> Simple feature collection with 4 features and 1 field
#> Geometry type: POLYGON
#> CRS:          EPSG:4326
#>            name                       geometry
#> 1 community_one POLYGON ((...)...)
#> 2 community_two POLYGON ((...)...)
#> 3 community_three POLYGON ((...)...)
#> 4 community_four POLYGON ((...)...)
```

**Important:** the `community_id_col` parameter (used in many functions
below) must match the name of the column that identifies your
communities. Throughout this vignette we use
`community_id_col = "name"`. If your column is called something else
(e.g., `"site"`, `"village"`, `"locality"`), adjust accordingly.

## Step 2: Fetch Building Footprints

[`fetch_osm_buildings()`](https://epicentre-msf.github.io/gpssampling/reference/fetch_osm_buildings.md)
downloads building footprints from
[OpenStreetMap](https://www.openstreetmap.org/) for a given area. It
uses tile-based Overpass API queries so that even large areas can be
processed without hitting API limits.

``` r
osm_buildings <- fetch_osm_buildings(state_boundary)
#> Fetching OSM buildings across 12 tiles at zoom 13...
#>   Tile 1/12...
#>   Tile 2/12...
#>   ...
#> Merging 4,832 raw buildings from 12 tiles...
#> Clipping buildings to area boundary...
#> Done: 3,618 unique buildings fetched.
```

**What it returns:**

An `sf` POLYGON data frame with columns:

- `osm_id` – the OpenStreetMap feature identifier
- `building` – the OSM building type tag (e.g., `"yes"`,
  `"residential"`, `"hospital"`, `"school"`)
- `geometry` – the building footprint polygon

**Key parameters:**

| Parameter | Default      | Description                                                                                                                               |
|-----------|--------------|-------------------------------------------------------------------------------------------------------------------------------------------|
| `area_sf` | *(required)* | Polygon defining the area of interest                                                                                                     |
| `zoom`    | `13L`        | Tile zoom level. Higher = smaller tiles, more queries. Lower = fewer queries but larger data per query. Default works well for most areas |

**Tips:**

- For very large study areas (spanning many tiles), expect the download
  to take several minutes. The function prints progress per tile.
- If a tile download fails (e.g., network issue), it logs a warning and
  continues with the remaining tiles. You will not lose progress.
- Buildings are automatically deduplicated (tiles can overlap at edges).

## Step 3: Filter Non-Residential Buildings

Not all buildings in OSM are residential.
[`filter_buildings()`](https://epicentre-msf.github.io/gpssampling/reference/filter_buildings.md)
removes non-residential structures like hospitals, schools, and
warehouses so you only sample from buildings where people live.

The function supports **three workflows** depending on your data:

### Path A: OSM-only data (most common)

When your buildings come from
[`fetch_osm_buildings()`](https://epicentre-msf.github.io/gpssampling/reference/fetch_osm_buildings.md),
they have a `building` column with OSM tags. Filter directly on that
column:

``` r
buildings <- filter_buildings(
  osm_buildings,
  building_col = "building"
)
#> Filtering 3,618 buildings by building column...
#> Removed 47 non-residential buildings, 3,571 remaining.
```

**Important:** when filtering OSM data, you must set
`building_col = "building"` because the default is `"type"` (designed
for user-provided datasets).

### Path B: Your own footprints + OSM labeling

If you have building footprints from another source (e.g., Microsoft
Building Footprints, Google Open Buildings) that do not have type tags,
you can use OSM to label them. The function spatially matches your
buildings against OSM buildings and copies the tags over:

``` r
user_buildings <- st_read("my_building_footprints.gpkg")
osm <- fetch_osm_buildings(state_boundary)

buildings <- filter_buildings(
  user_buildings,
  osm_buildings_sf = osm
)
#> Matching 5,200 buildings against 3,618 OSM footprints...
#> Computing spatial intersections...
#> Matched 4,100 buildings, 1,100 unmatched.
#> Removed 82 non-residential buildings, 5,118 remaining.
```

By default, unmatched buildings (those with no OSM counterpart) are
**kept**, assuming they are residential. Set `keep_untagged = FALSE` to
drop them.

### Path C: Your own footprints, no OSM

If you have your own data with a type column, filter directly:

``` r
buildings <- filter_buildings(
  user_buildings,
  building_col = "type"
)
```

If your data has no type information and you do not provide OSM data,
the function returns all buildings as-is with a warning.

**Key parameters:**

| Parameter          | Default                                                                                                        | Description                                                               |
|--------------------|----------------------------------------------------------------------------------------------------------------|---------------------------------------------------------------------------|
| `buildings_sf`     | *(required)*                                                                                                   | Building footprints (`sf` POLYGON)                                        |
| `osm_buildings_sf` | `NULL`                                                                                                         | Optional OSM buildings for labeling                                       |
| `remove_tags`      | `c("hospital", "school", "church", "mosque", "industrial", "commercial", "warehouse", "government", "public")` | Building types to exclude                                                 |
| `keep_untagged`    | `TRUE`                                                                                                         | Keep buildings with no OSM match?                                         |
| `building_col`     | `"type"`                                                                                                       | Name of the column with building type tags. Use `"building"` for OSM data |

**Customizing the filter:**

Add or remove tags to suit your study. For example, to also exclude
religious buildings and markets:

``` r
buildings <- filter_buildings(
  osm_buildings,
  building_col = "building",
  remove_tags = c(
    "hospital",
    "school",
    "church",
    "mosque",
    "industrial",
    "commercial",
    "warehouse",
    "government",
    "public",
    "temple",
    "synagogue",
    "market",
    "retail"
  )
)
```

## Step 4: Crop Buildings to Communities

[`crop_buildings()`](https://epicentre-msf.github.io/gpssampling/reference/crop_buildings.md)
intersects your building footprints with community polygons, converts
each building to its centroid point, and returns a **named list** – one
entry per community.

``` r
buildings_list <- crop_buildings(
  buildings,
  communities,
  community_id_col = "name"
)
#> Cropping 3,571 buildings to 4 communities...
#> Finding buildings per community (spatial index)...
#>   community_four: computing centroids for 612 buildings...
#>   community_one: computing centroids for 847 buildings...
#>   community_three: computing centroids for 956 buildings...
#>   community_two: computing centroids for 1,203 buildings...
#> Done: community_four: 598, community_one: 834, community_three: 940, community_two: 1,185.
```

**What it returns:**

A named list of `sf` POINT data frames. Each element has:

- `id` – sequential integer (1, 2, 3, …) within that community
- `community` – the community name
- `osm_id` – preserved from the input (if present)
- `geometry` – the centroid point

You can check how many buildings are in each community:

``` r
vapply(buildings_list, nrow, integer(1L))
#> community_four  community_one community_three  community_two
#>            598            834             940           1185
```

**Key parameters:**

| Parameter          | Default      | Description                        |
|--------------------|--------------|------------------------------------|
| `buildings_sf`     | *(required)* | Building footprints (`sf` POLYGON) |
| `communities_sf`   | *(required)* | Community polygons (`sf`)          |
| `community_id_col` | `"name"`     | Column name for community ID       |

**What happens internally:**

1.  Communities with the same name are dissolved (e.g., a multipolygon
    stored as separate features with the same name is merged into one).
2.  A spatial index (`st_intersects`) finds buildings that overlap each
    community.
3.  Building polygons are converted to centroids.
4.  Only centroids that fall **within** the community boundary are kept.
5.  Points are sorted deterministically by longitude then latitude,
    ensuring reproducible `id` numbering.

## Step 5: Visualize Building Footprints (Optional but Recommended)

Before sampling, it is helpful to visually verify that your building
data looks correct.
[`map_cropped_buildings()`](https://epicentre-msf.github.io/gpssampling/reference/map_cropped_buildings.md)
renders a map per community showing the actual building footprint shapes
over an OSM basemap.

``` r
crop_maps <- map_cropped_buildings(
  buildings,
  communities,
  community_id_col = "name",
  out_dir = "output/maps/buildings"
)
#> Mapping 3,571 building footprints across 4 communities (filtering mode)...
#>   community_four: finding overlapping buildings...
#>   community_four: rendering map (612 buildings)...
#>   ...
#> Generated 4 building maps.
#> Saving maps to output/maps/buildings...
```

This saves one PNG per community (e.g., `community_one_buildings.png`).

You can also display a single map interactively in RStudio:

``` r
crop_maps[["community_one"]]
```

**Key parameters:**

| Parameter          | Default           | Description                                                                                                                          |
|--------------------|-------------------|--------------------------------------------------------------------------------------------------------------------------------------|
| `buildings_sf`     | *(required)*      | Building footprints (the polygons, not centroids)                                                                                    |
| `communities_sf`   | *(required)*      | Community polygons                                                                                                                   |
| `community_id_col` | `"name"`          | Column name for community ID                                                                                                         |
| `clip`             | `FALSE`           | `TRUE` = geometrically clip buildings to boundary (slow but precise). `FALSE` = show full footprints of overlapping buildings (fast) |
| `basemap`          | `"OpenStreetMap"` | Tile provider (e.g., `"OpenStreetMap.HOT"`, `"CartoDB.Positron"`)                                                                    |
| `building_fill`    | `"#8B6914"`       | Building fill color                                                                                                                  |
| `out_dir`          | `NULL`            | Directory for saving PNGs. `NULL` = return plots only                                                                                |
| `width`, `height`  | `12`              | Plot dimensions in inches                                                                                                            |
| `dpi`              | `300`             | Plot resolution                                                                                                                      |

## Step 6: Sample Buildings

[`sample_communities()`](https://epicentre-msf.github.io/gpssampling/reference/sample_communities.md)
is the core function. It selects a specified number of buildings per
community, enforcing a minimum distance between any two selected points
so that field teams cover different areas rather than clustering.

### How the algorithm works

Sampling uses a **two-phase approach**:

1.  **Random selection** – all points (including the first) are chosen
    at random. At each step, the algorithm picks a random candidate from
    buildings that are at least `min_distance` meters away from every
    already-selected point. If no candidates meet the distance
    constraint, the remaining points are drawn randomly with a warning.

2.  **Proximity ordering** – after selection, points are reordered for
    efficient field work using a **nearest-neighbour chain**: the
    selected point closest to an OSM road becomes \#1, then the nearest
    unvisited point becomes \#2, and so on. This `selection_order`
    determines the order field workers follow.

After primary selection, **secondary (replacement) points** are drawn
from the remaining pool using the same algorithm.

### Basic usage

``` r
samples <- sample_communities(
  buildings_list,
  n_required = c(
    community_one = 30,
    community_two = 80,
    community_three = 85,
    community_four = 60
  ),
  min_distance = 50,
  seed = 250292L
)
```

If all communities need the same number of points, pass a single
integer:

``` r
samples <- sample_communities(
  buildings_list,
  n_required = 50L,
  min_distance = 50,
  seed = 250292L
)
```

### Understanding the output

The result is a **named list of lists**. Each community contains:

``` r
str(samples$community_one, max.level = 1)
#> List of 5
#>  $ buildings   : sf [834 x 4]   -- all candidate buildings
#>  $ primary     : sf [30 x 6]    -- selected primary points
#>  $ secondary   : sf [30 x 6]    -- replacement points
#>  $ min_distance: num 50         -- distance constraint used
#>  $ seed        : int 827364     -- per-community seed
```

The `$primary` and `$secondary` data frames have these columns:

| Column            | Description                                                                                                             |
|-------------------|-------------------------------------------------------------------------------------------------------------------------|
| `id`              | Original building ID from [`crop_buildings()`](https://epicentre-msf.github.io/gpssampling/reference/crop_buildings.md) |
| `community`       | Community name                                                                                                          |
| `osm_id`          | OSM feature ID (if present)                                                                                             |
| `selection_order` | Walking order (1 = start near road, then nearest-neighbour chain)                                                       |
| `point_id`        | Globally unique ID across all communities and sets                                                                      |
| `geometry`        | Point coordinates                                                                                                       |

**Global point IDs:** Primary points are numbered 1 through N across all
communities (alphabetically). Secondary point IDs continue from N+1.
This ensures every point across the entire project has a unique
identifier, which is critical for field tracking.

### The summary table

By default (`print_table = TRUE`),
[`sample_communities()`](https://epicentre-msf.github.io/gpssampling/reference/sample_communities.md)
prints a `flextable` summary and attaches it to the result:

``` r
# Access the summary table (a flextable object)
attr(samples, "summary_table")

# Access the underlying data frame
attr(samples, "summary_df")
```

The summary shows per-community statistics:

| Column                 | Meaning                                                                 |
|------------------------|-------------------------------------------------------------------------|
| Buildings              | Total candidate buildings                                               |
| Primary                | Number of primary points drawn                                          |
| Secondary              | Number of secondary (replacement) points drawn                          |
| Total Pts              | Primary + Secondary                                                     |
| Coverage %             | Total points as % of available buildings                                |
| Min Dist Requested (m) | The `min_distance` you specified                                        |
| Min Dist Primary (m)   | Actual minimum pairwise distance among primary points                   |
| Min Dist Secondary (m) | Actual minimum pairwise distance among secondary points                 |
| Min Dist All (m)       | Actual minimum pairwise distance among all points (primary + secondary) |
| Pairs \< Threshold     | Number of point pairs closer than `min_distance` (ideally 0)            |
| Mode                   | `"independent"` or `"joint"`                                            |
| Seed                   | Per-community seed used                                                 |

A **TOTAL** row summarizes across all communities. Violations (pairs
below threshold) are highlighted in red.

### Joint sampling (less clustered secondary points)

By default, primary and secondary points are drawn **independently**:
first the primary set is drawn, then the secondary set is drawn from the
remaining pool. This can lead to clustering in the secondary set when
the remaining pool is sparse.

Set `joint = TRUE` to draw both sets in a **single pass**:

``` r
samples <- sample_communities(
  buildings_list,
  n_required = c(
    community_one = 30,
    community_two = 80,
    community_three = 85,
    community_four = 60
  ),
  min_distance = 50,
  seed = 250292L,
  joint = TRUE
)
```

With `joint = TRUE`, the algorithm draws up to `2 * n_required` points
in one call, enforcing the minimum distance across **all** of them. The
first `n_required` drawn become primary; the rest become secondary. This
produces better spatial spread across both sets.

### Reproducibility

Results are **fully reproducible** given the same:

- `seed` value
- Input data (same buildings, same communities)
- R version \>= 3.6.0

The algorithm uses
[`withr::with_seed()`](https://withr.r-lib.org/reference/with_seed.html)
(not [`set.seed()`](https://rdrr.io/r/base/Random.html)), so it does not
affect your global RNG state. Each community gets a deterministic
sub-seed derived from the master `seed` and the community name, so
**adding or removing a community does not change the selection in other
communities**.

### Key parameters

| Parameter        | Default        | Description                                                                                                                 |
|------------------|----------------|-----------------------------------------------------------------------------------------------------------------------------|
| `buildings_list` | *(required)*   | Named list of `sf` POINT from [`crop_buildings()`](https://epicentre-msf.github.io/gpssampling/reference/crop_buildings.md) |
| `n_required`     | *(required)*   | Named integer vector (or single integer for all)                                                                            |
| `min_distance`   | `50`           | Minimum distance in meters between any two selected points                                                                  |
| `seed`           | *(required)*   | Integer seed for reproducibility                                                                                            |
| `joint`          | `FALSE`        | Draw primary + secondary in one pass?                                                                                       |
| `print_table`    | `TRUE`         | Print a flextable summary?                                                                                                  |
| `road_types`     | 6 common types | OSM road types for proximity ordering                                                                                       |
| `road_dir`       | `NULL`         | Directory for caching road files (see below)                                                                                |

## Step 7: Pre-Download Roads (Optional)

[`sample_communities()`](https://epicentre-msf.github.io/gpssampling/reference/sample_communities.md)
downloads roads from OSM for each community to do the proximity
ordering. If you want to pre-download roads (e.g., while you have
internet) and cache them for later use, use
[`fetch_community_roads()`](https://epicentre-msf.github.io/gpssampling/reference/fetch_community_roads.md):

``` r
roads <- fetch_community_roads(
  communities,
  community_id_col = "name",
  road_dir = "output/roads"
)
#> Fetching roads for 4 communities into output/roads...
#>   community_one (1/4)...
#>     Downloading roads from OSM (timeout: 120s)...
#>     Cached roads to community_one.gpkg.
#>   ...
#> Done: 4/4 communities with roads.
```

Roads are saved as `.gpkg` files, one per community. On subsequent
calls, cached files are reused without re-downloading.

Pass the same `road_dir` to
[`sample_communities()`](https://epicentre-msf.github.io/gpssampling/reference/sample_communities.md)
to use the cache:

``` r
samples <- sample_communities(
  buildings_list,
  n_required = c(community_one = 30, community_two = 80),
  min_distance = 50,
  seed = 250292L,
  road_dir = "output/roads"
)
```

**Key parameters:**

| Parameter          | Default        | Description                        |
|--------------------|----------------|------------------------------------|
| `communities_sf`   | *(required)*   | Community polygons                 |
| `community_id_col` | `"name"`       | Column name for community ID       |
| `road_dir`         | *(required)*   | Directory for cached `.gpkg` files |
| `road_types`       | 6 common types | OSM highway values to query        |
| `timeout`          | `120`          | Overpass API timeout in seconds    |

## Step 8: Split into Batches

[`split_batches()`](https://epicentre-msf.github.io/gpssampling/reference/split_batches.md)
distributes points across field teams using **round-robin assignment**.
Points are assigned in order of their `selection_order`, so consecutive
points (which are geographically close thanks to the nearest- neighbour
chain) go to different batches.

``` r
primary_batches <- split_batches(samples, n_batches = 5L, set = "primary")
secondary_batches <- split_batches(samples, n_batches = 5L, set = "secondary")
```

Each community’s points get an `assigned_batch` column (1, 2, 3, 4, or
5).

If different communities need different team counts:

``` r
primary_batches <- split_batches(
  samples,
  n_batches = c(
    community_one = 3,
    community_two = 5,
    community_three = 5,
    community_four = 4
  ),
  set = "primary"
)
```

**Key parameters:**

| Parameter      | Default      | Description                                                                                                     |
|----------------|--------------|-----------------------------------------------------------------------------------------------------------------|
| `samples_list` | *(required)* | Output of [`sample_communities()`](https://epicentre-msf.github.io/gpssampling/reference/sample_communities.md) |
| `n_batches`    | *(required)* | Integer (all communities) or named integer vector                                                               |
| `set`          | `"primary"`  | Which point set: `"primary"` or `"secondary"`                                                                   |

## Step 9: Create Buffers

[`create_buffers()`](https://epicentre-msf.github.io/gpssampling/reference/create_buffers.md)
generates circular buffer polygons around sampled points. Buffers are
computed in a local UTM projection (auto-detected) for accurate metric
distances, then reprojected back to WGS84.

``` r
# From the full samples list
buffers <- create_buffers(samples, radius = 50, set = "primary")

# Or from a single sf POINT
buffers_one <- create_buffers(samples$community_one$primary, radius = 50)
```

**Key parameters:**

| Parameter | Default      | Description                                                                                                                                                                                                                             |
|-----------|--------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `x`       | *(required)* | An `sf` POINT, or a named list from [`sample_communities()`](https://epicentre-msf.github.io/gpssampling/reference/sample_communities.md) / [`split_batches()`](https://epicentre-msf.github.io/gpssampling/reference/split_batches.md) |
| `radius`  | `50`         | Buffer radius in meters                                                                                                                                                                                                                 |
| `set`     | `"primary"`  | Which point set (when `x` is a list)                                                                                                                                                                                                    |

## Step 10: Export Points to Disk

[`export_points()`](https://epicentre-msf.github.io/gpssampling/reference/export_points.md)
writes points, buffer polygons, and OsmAnd-compatible SQLite tile
overlays to a structured directory.

``` r
# Export primary points
manifest_pri <- export_points(
  primary_batches,
  out_dir = "output",
  formats = c("gpkg", "gpx"),
  include_buffers = TRUE,
  buffer_radius = 50,
  set = "primary"
)

# Export secondary points
manifest_sec <- export_points(
  secondary_batches,
  out_dir = "output",
  formats = c("gpkg", "gpx"),
  include_buffers = TRUE,
  buffer_radius = 50,
  set = "secondary"
)
```

### Output directory structure

    output/
      primary/
        community_one/
          community_one_primary_all.gpkg           <- all points (GeoPackage)
          community_one_primary_all.gpx            <- all points (GPX waypoints)
          community_one_primary_batch_1.gpkg       <- batch 1 points
          community_one_primary_batch_1.gpx
          community_one_primary_batch_2.gpkg
          community_one_primary_batch_2.gpx
          ...
          community_one_buffers_all.gpkg           <- all buffer polygons
          community_one_buffers_all.gpx            <- buffer boundaries (GPX tracks)
          community_one_buffers_all.sqlitedb       <- OsmAnd tile overlay (all)
          community_one_buffers_batch_1.gpkg
          community_one_buffers_batch_1.gpx
          community_one_buffers_batch_1.sqlitedb   <- OsmAnd tile overlay (batch 1)
          ...
        community_two/
          ...
      secondary/
        community_one/
          ...

### The manifest

[`export_points()`](https://epicentre-msf.github.io/gpssampling/reference/export_points.md)
returns a tibble listing every file it created:

``` r
print(manifest_pri)
#> # A tibble: 84 x 6
#>    community     set     batch type    format  path
#>    <chr>         <chr>   <chr> <chr>   <chr>   <chr>
#>  1 community_one primary all   points  gpkg    output/primary/community_one/...
#>  2 community_one primary all   points  gpx     output/primary/community_one/...
#>  3 community_one primary 1     points  gpkg    output/primary/community_one/...
#>  ...
```

### OsmAnd integration

The `.sqlitedb` files are tile overlays that show buffer zones on OsmAnd
(Android GPS navigation app used by field teams). To use them:

1.  Copy the `.sqlitedb` file to your Android device at:
    `Android/data/net.osmand/files/tiles/`
2.  In OsmAnd, go to **Configure map** \> **Overlay** (or **Underlay**)
3.  Select the `.sqlitedb` file

The overlay shows semi-transparent green circles around each sampled
point, helping field workers identify which buildings to visit.

### Supported export formats

| Format | Extension | Description                                                    |
|--------|-----------|----------------------------------------------------------------|
| `gpkg` | `.gpkg`   | GeoPackage – the modern standard, preserves all attributes     |
| `gpx`  | `.gpx`    | GPS Exchange – widely supported by GPS devices and apps        |
| `shp`  | `.shp`    | ESRI Shapefile – legacy format, 10-character column name limit |
| `kml`  | `.kml`    | Google Earth format                                            |

**Key parameters:**

| Parameter         | Default            | Description                                                                                                                                                                                                    |
|-------------------|--------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `samples_list`    | *(required)*       | Output of [`split_batches()`](https://epicentre-msf.github.io/gpssampling/reference/split_batches.md) or [`sample_communities()`](https://epicentre-msf.github.io/gpssampling/reference/sample_communities.md) |
| `out_dir`         | *(required)*       | Root output directory                                                                                                                                                                                          |
| `formats`         | `c("gpkg", "gpx")` | Export formats                                                                                                                                                                                                 |
| `include_buffers` | `TRUE`             | Generate buffer polygons and SQLite tiles?                                                                                                                                                                     |
| `buffer_radius`   | `50`               | Buffer radius in meters                                                                                                                                                                                        |
| `set`             | `"primary"`        | Which point set                                                                                                                                                                                                |

## Step 11: Create a Google Earth Project (Optional)

[`create_earth_project()`](https://epicentre-msf.github.io/gpssampling/reference/create_earth_project.md)
generates a KML file organized into folders for viewing in Google Earth.
This is useful for reviewing the sampling plan on a desktop before going
to the field.

``` r
create_earth_project(
  samples,
  out_file = "output/sampling_project.kml",
  buffer_radius = 50,
  title = "Survey Sampling Plan"
)
#> Building KML for 4 communities...
#> Created Google Earth project: output/sampling_project.kml
```

The KML file contains four top-level folders:

- **Primary Points** – with subfolders per community, each point labeled
  by `point_id`
- **Secondary Points** – same structure
- **Primary Buffers** – buffer circles around primary points
- **Secondary Buffers** – buffer circles around secondary points

Each folder can be toggled on/off in Google Earth. Points and buffers
use distinct colors (configurable).

**Key parameters:**

| Parameter                | Default              | Description                                                                                                     |
|--------------------------|----------------------|-----------------------------------------------------------------------------------------------------------------|
| `samples_list`           | *(required)*         | Output of [`sample_communities()`](https://epicentre-msf.github.io/gpssampling/reference/sample_communities.md) |
| `out_file`               | *(required)*         | Path for the `.kml` file                                                                                        |
| `buffer_radius`          | `50`                 | Buffer radius in meters                                                                                         |
| `primary_color`          | `"#FF4500"`          | Point color for primary (orange-red)                                                                            |
| `secondary_color`        | `"#1E90FF"`          | Point color for secondary (blue)                                                                                |
| `primary_buffer_color`   | `"#FF450044"`        | Buffer fill for primary                                                                                         |
| `secondary_buffer_color` | `"#1E90FF44"`        | Buffer fill for secondary                                                                                       |
| `title`                  | `"Sampling Project"` | Document title in Google Earth                                                                                  |

## Step 12: Zip for Field Distribution

[`zip_points()`](https://epicentre-msf.github.io/gpssampling/reference/zip_points.md)
bundles GPX files and SQLite tile overlays into zip archives, ready for
copying to field devices:

``` r
zips <- zip_points(
  export_dir = "output",
  prefix = "survey-",
  sets = c("primary", "secondary")
)
#> Created output/survey-primary-points.zip
#> Created output/survey-secondary-points.zip
```

**Key parameters:**

| Parameter    | Default                     | Description                                                                                                                              |
|--------------|-----------------------------|------------------------------------------------------------------------------------------------------------------------------------------|
| `export_dir` | *(required)*                | Root export directory (same as `out_dir` in [`export_points()`](https://epicentre-msf.github.io/gpssampling/reference/export_points.md)) |
| `out_dir`    | `export_dir`                | Where to write the zip files                                                                                                             |
| `sets`       | `c("primary", "secondary")` | Which sets to zip                                                                                                                        |
| `prefix`     | `""`                        | Optional prefix for zip filenames                                                                                                        |

## Step 13: Email to Field Teams (Optional)

[`email_points()`](https://epicentre-msf.github.io/gpssampling/reference/email_points.md)
sends zip archives as email attachments. It uses the `emayili` package
(must be installed separately) and requires SMTP credentials.

``` r
email_points(
  zip_files = zips,
  to = "fieldteam@example.org",
  subject = "Survey - GPS Sampling Points"
)
#> Email sent to fieldteam@example.org
```

**SMTP configuration:**

Set these environment variables (e.g., in your `.Renviron` file):

    EMAIL_FROM=your.email@example.org
    EMAIL_USER=your.email@example.org
    EMAIL_PASSWORD=your-app-password

Or pass them directly:

``` r
email_points(
  zip_files = zips,
  to = c("team1@example.org", "team2@example.org"),
  from = "coordinator@example.org",
  subject = "Survey GPS Points - Week 12",
  host = "smtp.office365.com",
  port = 587L,
  username = "coordinator@example.org",
  password = "app-password-here"
)
```

## Step 14: Static Maps

The package produces publication-quality static maps using `ggplot2`,
`ggspatial` (scale bars and north arrows), and `tidyterra` (basemap tile
rendering). All three are in `Suggests` and installed automatically when
first needed.

### Generate all maps at once

[`map_all_communities()`](https://epicentre-msf.github.io/gpssampling/reference/map_all_communities.md)
is the main function for batch map generation. It creates:

- An **overview map** showing all communities with uniform point color
- **Per-community primary maps** with batch coloring
- **Per-community secondary maps** with batch coloring (if secondary
  points are provided)

``` r
maps <- map_all_communities(
  primary_batches,
  communities,
  community_id_col = "name",
  secondary_batches = secondary_batches,
  out_dir = "output/maps",
  buffer_radius = 50
)
#> Generating overview map...
#> Generating map for community_one (primary)...
#> Generating map for community_one (secondary)...
#> ...
#> Saved output/maps/overview.png
#> Saved output/maps/community_one_primary.png
#> Saved output/maps/community_one_secondary.png
#> ...
```

**Key parameters:**

| Parameter           | Default      | Description                                                                                                                            |
|---------------------|--------------|----------------------------------------------------------------------------------------------------------------------------------------|
| `primary_batches`   | *(required)* | Named list of `sf` from [`split_batches()`](https://epicentre-msf.github.io/gpssampling/reference/split_batches.md)                    |
| `communities_sf`    | *(required)* | Community polygons                                                                                                                     |
| `community_id_col`  | `"name"`     | Column name for community ID                                                                                                           |
| `secondary_batches` | `NULL`       | Named list of `sf` from [`split_batches()`](https://epicentre-msf.github.io/gpssampling/reference/split_batches.md) for secondary maps |
| `color_batches`     | `TRUE`       | Color points by `assigned_batch`?                                                                                                      |
| `out_dir`           | `NULL`       | Directory for saving PNGs. `NULL` = return plots only                                                                                  |
| `buffer_radius`     | `50`         | Buffer radius in meters                                                                                                                |
| `primary_shape`     | `16`         | Marker shape for primary maps (filled circle)                                                                                          |
| `secondary_shape`   | `17`         | Marker shape for secondary maps (filled triangle)                                                                                      |
| `width`             | `10`         | Plot width in inches                                                                                                                   |
| `height`            | `12`         | Plot height in inches                                                                                                                  |
| `dpi`               | `300`        | Plot resolution                                                                                                                        |

### Generate a single community map

[`map_community()`](https://epicentre-msf.github.io/gpssampling/reference/map_community.md)
gives full control over a single map:

``` r
p <- map_community(
  community_name = "community_one",
  community_sf = communities[communities$name == "community_one", ],
  points_sf = primary_batches[["community_one"]],
  buffers_sf = create_buffers(
    primary_batches[["community_one"]],
    radius = 50
  ),
  color_batches = TRUE,
  show_labels = TRUE,
  label_size = 1.8,
  basemap = "OpenStreetMap.HOT"
)

# Display in RStudio
p

# Customize before saving
p <- p + ggplot2::theme(legend.position = "bottom")
ggplot2::ggsave(
  "community_one_primary.png",
  p,
  width = 10,
  height = 12,
  dpi = 300
)
```

**What the map shows:**

- OSM basemap tiles (downloaded automatically)
- Community boundary polygon (semi-transparent)
- Buffer circles around each point (light green by default)
- Sampled points (colored by batch if `color_batches = TRUE`)
- Point ID labels (if `show_labels = TRUE` and `point_id` column exists)
- Scale bar (bottom-left)
- North arrow (top-right)
- Auto-generated subtitle with point count, ID range, and minimum
  pairwise distance

**Key parameters:**

| Parameter        | Default               | Description                                                                        |
|------------------|-----------------------|------------------------------------------------------------------------------------|
| `community_name` | *(required)*          | Name (used in title)                                                               |
| `community_sf`   | *(required)*          | Community polygon (`sf`)                                                           |
| `points_sf`      | *(required)*          | Sampled points (`sf` POINT)                                                        |
| `buffers_sf`     | `NULL`                | Buffer polygons (`sf` POLYGON)                                                     |
| `color_batches`  | `TRUE`                | Color by `assigned_batch`?                                                         |
| `show_labels`    | `TRUE`                | Show `point_id` labels?                                                            |
| `label_size`     | `1.8`                 | Text size for labels                                                               |
| `point_shape`    | `16`                  | Marker shape (see [`?points`](https://rdrr.io/r/graphics/points.html) for options) |
| `basemap`        | `"OpenStreetMap.HOT"` | Tile provider name                                                                 |
| `point_color`    | `"#e97a52"`           | Uniform color (when no batch coloring)                                             |
| `buffer_color`   | `"#90EE9066"`         | Buffer fill color (with alpha)                                                     |
| `title`          | `community_name`      | Map title                                                                          |
| `subtitle`       | auto                  | Auto-generated or custom subtitle                                                  |

### Generate an overview map

[`map_overview()`](https://epicentre-msf.github.io/gpssampling/reference/map_overview.md)
creates a zoomed-out map of all communities:

``` r
p_overview <- map_overview(
  primary_batches,
  communities,
  community_id_col = "name",
  buffer_radius = 50,
  title = "Sampling Overview"
)

ggplot2::ggsave(
  "overview.png",
  p_overview,
  width = 10,
  height = 12,
  dpi = 300
)
```

The overview shows all communities with labels at centroids, uniform
point color (no batch coloring), and buffer zones. It is designed to
give a bird’s-eye view of the sampling plan.

## Step 15: Interactive Leaflet Map

[`leaflet_communities()`](https://epicentre-msf.github.io/gpssampling/reference/leaflet_communities.md)
creates a fully interactive map with layer toggles, a community
navigation panel, and multiple base map options. This is ideal for
digital review, presentations, and sharing with stakeholders.

``` r
m <- leaflet_communities(
  primary_batches,
  communities,
  community_id_col = "name",
  secondary_batches = secondary_batches,
  buildings_list = buildings_list,
  roads_list = roads,
  buffer_radius = 50,
  out_file = "output/maps/sampling_map.html"
)

# Display in RStudio viewer
m
```

### Features

- **Z-ordered layers:** communities (bottom) \> roads \> buildings \>
  buffers \> points (top). Layers never obscure each other.
- **Layer toggles:** checkbox controls for Communities, Roads,
  Buildings, Primary Points, Primary Buffers, Secondary Points,
  Secondary Buffers.
- **Distinct markers:** circles for primary points, triangles for
  secondary.
- **Batch coloring:** points colored by `assigned_batch` (using the Set1
  color palette).
- **Community navigation:** a panel with quick-zoom buttons for each
  community.
- **Multiple base maps:** OpenStreetMap, Light (CartoDB), Satellite
  (ESRI), OSM Humanitarian.
- **Fullscreen:** button to expand the map (requires `leaflet.extras`).
- **Point popups:** click any point to see its ID, batch, and community.
- **Buildings toggle:** building footprints are included but hidden by
  default (they can be heavy). Toggle via the layer control.
- **Self-contained HTML:** pass `out_file` to save as a standalone HTML
  file that can be opened in any browser.

**Key parameters:**

| Parameter           | Default      | Description                                                                                                                 |
|---------------------|--------------|-----------------------------------------------------------------------------------------------------------------------------|
| `primary_batches`   | *(required)* | Named list from [`split_batches()`](https://epicentre-msf.github.io/gpssampling/reference/split_batches.md)                 |
| `communities_sf`    | *(required)* | Community polygons                                                                                                          |
| `community_id_col`  | `"name"`     | Column name for community ID                                                                                                |
| `secondary_batches` | `NULL`       | Named list from [`split_batches()`](https://epicentre-msf.github.io/gpssampling/reference/split_batches.md)                 |
| `buildings_list`    | `NULL`       | Named list from [`crop_buildings()`](https://epicentre-msf.github.io/gpssampling/reference/crop_buildings.md)               |
| `roads_list`        | `NULL`       | Named list from [`fetch_community_roads()`](https://epicentre-msf.github.io/gpssampling/reference/fetch_community_roads.md) |
| `color_batches`     | `TRUE`       | Color by batch?                                                                                                             |
| `buffer_radius`     | `50`         | Buffer radius in meters                                                                                                     |
| `out_file`          | `NULL`       | Path for self-contained HTML output                                                                                         |

## Step 16: Utility – Find Closest-to-Road Buildings

[`find_start_points()`](https://epicentre-msf.github.io/gpssampling/reference/find_start_points.md)
is a standalone utility that identifies the building closest to an OSM
road in each community. The main sampling pipeline uses this logic
internally (via `order_selected_points()`), but you can call it
independently for other purposes:

``` r
starts <- find_start_points(buildings_list)
#> Finding road-nearest start points for 4 communities...
#>   community_one: querying roads for 834 points...
#>   community_one: computing distances to 156 road segments...
#>   community_one: start point id 42.
#>   ...

print(starts)
#> community_four  community_one community_three  community_two
#>            315             42             178            503
```

Returns a named integer vector of building `id` values.

## Complete Workflow Script

Here is the full pipeline in a single script, ready to copy and adapt:

``` r
library(gpssampling)
library(sf)

# ── 1. Load data ──────────────────────────────────────────────
communities <- st_read("communities.gpkg")
state_boundary <- st_read("boundary.gpkg")

# ── 2. Fetch and filter buildings ─────────────────────────────
osm_buildings <- fetch_osm_buildings(state_boundary)

buildings <- filter_buildings(
  osm_buildings,
  building_col = "building"
)

# ── 3. Crop to communities ───────────────────────────────────
buildings_list <- crop_buildings(
  buildings,
  communities,
  community_id_col = "name"
)

# Check counts
vapply(buildings_list, nrow, integer(1L))

# ── 4. (Optional) Visualize building footprints ──────────────
map_cropped_buildings(
  osm_buildings,
  communities,
  community_id_col = "name",
  out_dir = "output/maps/buildings"
)

# ── 5. Pre-download roads (optional, for offline use) ────────
roads <- fetch_community_roads(
  communities,
  community_id_col = "name",
  road_dir = "output/roads"
)

# ── 6. Sample ────────────────────────────────────────────────
samples <- sample_communities(
  buildings_list,
  n_required = c(
    community_one = 30,
    community_two = 80,
    community_three = 85,
    community_four = 60
  ),
  min_distance = 50,
  seed = 250292L,
  joint = TRUE,
  road_dir = "output/roads"
)

# ── 7. Split into batches ────────────────────────────────────
primary_batches <- split_batches(samples, n_batches = 5L, set = "primary")
secondary_batches <- split_batches(samples, n_batches = 5L, set = "secondary")

# ── 8. Export to disk ────────────────────────────────────────
export_points(
  primary_batches,
  out_dir = "output",
  formats = c("gpkg", "gpx"),
  include_buffers = TRUE,
  buffer_radius = 50,
  set = "primary"
)

export_points(
  secondary_batches,
  out_dir = "output",
  formats = c("gpkg", "gpx"),
  include_buffers = TRUE,
  buffer_radius = 50,
  set = "secondary"
)

# ── 9. Google Earth project ──────────────────────────────────
create_earth_project(
  samples,
  out_file = "output/sampling_project.kml",
  buffer_radius = 50
)

# ── 10. Zip for field teams ──────────────────────────────────
zips <- zip_points("output", prefix = "survey-")

# ── 11. Static maps ─────────────────────────────────────────
map_all_communities(
  primary_batches,
  communities,
  community_id_col = "name",
  secondary_batches = secondary_batches,
  out_dir = "output/maps",
  buffer_radius = 50
)

# ── 12. Interactive map ──────────────────────────────────────
leaflet_communities(
  primary_batches,
  communities,
  community_id_col = "name",
  secondary_batches = secondary_batches,
  buildings_list = buildings_list,
  roads_list = roads,
  buffer_radius = 50,
  out_file = "output/maps/sampling_map.html"
)
```

## Function Reference

### Sampling

| Function                                                                                                    | Purpose                                                             |
|-------------------------------------------------------------------------------------------------------------|---------------------------------------------------------------------|
| [`fetch_osm_buildings()`](https://epicentre-msf.github.io/gpssampling/reference/fetch_osm_buildings.md)     | Download OSM building footprints for an area                        |
| [`filter_buildings()`](https://epicentre-msf.github.io/gpssampling/reference/filter_buildings.md)           | Remove non-residential buildings by OSM tags                        |
| [`crop_buildings()`](https://epicentre-msf.github.io/gpssampling/reference/crop_buildings.md)               | Intersect buildings with community polygons, return centroids       |
| [`sample_communities()`](https://epicentre-msf.github.io/gpssampling/reference/sample_communities.md)       | Random sampling with min-distance constraint and proximity ordering |
| [`fetch_community_roads()`](https://epicentre-msf.github.io/gpssampling/reference/fetch_community_roads.md) | Pre-download and cache OSM roads per community                      |
| [`find_start_points()`](https://epicentre-msf.github.io/gpssampling/reference/find_start_points.md)         | Find closest-to-road building per community (utility)               |

### GPS Management

| Function                                                                                                  | Purpose                                            |
|-----------------------------------------------------------------------------------------------------------|----------------------------------------------------|
| [`split_batches()`](https://epicentre-msf.github.io/gpssampling/reference/split_batches.md)               | Assign round-robin batch numbers                   |
| [`create_buffers()`](https://epicentre-msf.github.io/gpssampling/reference/create_buffers.md)             | Generate circular buffer polygons                  |
| [`create_buffer_tiles()`](https://epicentre-msf.github.io/gpssampling/reference/create_buffer_tiles.md)   | Create OsmAnd-compatible SQLite tile overlays      |
| [`export_points()`](https://epicentre-msf.github.io/gpssampling/reference/export_points.md)               | Export points, buffers, and tiles to disk          |
| [`create_earth_project()`](https://epicentre-msf.github.io/gpssampling/reference/create_earth_project.md) | Generate a Google Earth KML with organized folders |
| [`zip_points()`](https://epicentre-msf.github.io/gpssampling/reference/zip_points.md)                     | Bundle GPX + SQLite files into zip archives        |
| [`email_points()`](https://epicentre-msf.github.io/gpssampling/reference/email_points.md)                 | Send zip files via SMTP email                      |

### Mapping

| Function                                                                                                    | Purpose                                                    |
|-------------------------------------------------------------------------------------------------------------|------------------------------------------------------------|
| [`map_cropped_buildings()`](https://epicentre-msf.github.io/gpssampling/reference/map_cropped_buildings.md) | Building footprint maps per community                      |
| [`map_community()`](https://epicentre-msf.github.io/gpssampling/reference/map_community.md)                 | Per-community static map with batch coloring and labels    |
| [`map_overview()`](https://epicentre-msf.github.io/gpssampling/reference/map_overview.md)                   | Zoomed-out static overview of all communities              |
| [`map_all_communities()`](https://epicentre-msf.github.io/gpssampling/reference/map_all_communities.md)     | Generate and optionally save all static maps               |
| [`leaflet_communities()`](https://epicentre-msf.github.io/gpssampling/reference/leaflet_communities.md)     | Interactive leaflet map with layer controls and navigation |

## Troubleshooting

### OSM download fails or times out

The Overpass API has rate limits. If downloads fail:

- Wait a few minutes and try again.
- Reduce the area size or increase the `zoom` level in
  [`fetch_osm_buildings()`](https://epicentre-msf.github.io/gpssampling/reference/fetch_osm_buildings.md)
  (more tiles but smaller queries).
- For roads, increase the `timeout` parameter in
  [`fetch_community_roads()`](https://epicentre-msf.github.io/gpssampling/reference/fetch_community_roads.md).

### “Requested N points but only M available”

The number of buildings in a community is less than `n_required`.
Solutions:

- Reduce `n_required` for that community.
- Check your community polygon – it may be too small or misaligned.
- Check that
  [`filter_buildings()`](https://epicentre-msf.github.io/gpssampling/reference/filter_buildings.md)
  did not remove too many buildings.

### “No candidates beyond Xm”

The minimum distance constraint is too strict for the number of points
requested. The algorithm relaxes the constraint and draws the remaining
points randomly with a warning. You can:

- Reduce `min_distance`.
- Reduce `n_required`.
- This is expected when `n_required` is a large fraction of the
  available buildings.

### Basemap tiles fail to download

If
[`maptiles::get_tiles()`](https://rdrr.io/pkg/maptiles/man/get_tiles.html)
fails (network issue or provider down), maps are rendered without a
basemap. The data layers (points, buffers, boundaries) still appear
correctly. Try a different `basemap` provider:

``` r
map_all_communities(
  primary_batches,
  communities,
  basemap = "CartoDB.Positron"
)
```

### OsmAnd does not show the overlay

- Verify the `.sqlitedb` file is in
  `Android/data/net.osmand/files/tiles/`.
- In OsmAnd, go to Configure map \> Overlay (or Underlay) and select the
  file.
- Make sure the zoom level you are viewing is within the tile range
  (default zoom 8-14).
