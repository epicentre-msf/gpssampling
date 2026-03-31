# gpssampling

A Shiny application for geospatial sampling of building structures within
defined polygon areas. Supports quadrat, T-square, and random sampling methods
with OpenStreetMap and Google satellite imagery integration for field
epidemiology surveys.

> **Disclaimer:** This package is a fork of the original
> [GeoSampler](https://apps.msf.net/geosampler/site/index.html) package
> developed by Epicentre (MSF). It has been reverse-engineered and modified
> with the help of a Claude AI agent for internal use only. This fork is
> **not** affiliated with, endorsed by, or supported by the original authors.

## Installation

### System prerequisites

**R >= 4.3.1** plus system libraries for spatial packages (`sf`, `terra`,
`gdal`, `geos`, `proj`).

Ubuntu / Debian:

```bash
sudo apt-get install -y \
  libcurl4-openssl-dev libssl-dev libxml2-dev \
  libgdal-dev libgeos-dev libproj-dev libudunits2-dev \
  libsqlite3-dev libfontconfig1-dev libfreetype6-dev \
  libharfbuzz-dev libfribidi-dev libpng-dev libtiff-dev libjpeg-dev
```

macOS (Homebrew):

```bash
brew install gdal geos proj udunits
```

### Install from GitHub

```r
install.packages("remotes")
remotes::install_github("epicentre-msf/gpssampling")
```

### Install from a local clone

```bash
git clone https://github.com/epicentre-msf/gpssampling.git
R CMD INSTALL gpssampling
```

Or from within R:

```r
devtools::install("path/to/gpssampling")
```

## Usage

```r
library(gpssampling)
samp <- sampler()
samp$launch()
```

## Programmatic Sampling Pipeline

In addition to the Shiny app, the package provides a scripted sampling
pipeline for reproducible workflows:

```r
library(gpssampling)
library(sf)

# Fetch buildings, filter, crop to communities
buildings_list <- fetch_osm_buildings(st_read("boundary.gpkg")) |>
  filter_buildings() |>
  crop_buildings(st_read("communities.gpkg"), community_id_col = "name")

# Sample with minimum-distance constraints
# joint = TRUE draws primary + secondary together (less clustered)
samples <- sample_communities(
  buildings_list,
  n_required = c(community_1 = 30, community_2 = 80),
  min_distance = 50,
  seed = 250292L,
  joint = TRUE
)

# Batch, export (GPX + GeoPackage + OsmAnd tiles), and map
# Use a single integer for uniform batches, or a named vector per community:
batched <- split_batches(samples, n_batches = 5L, set = "primary")
# batched <- split_batches(samples, n_batches = c(community_1 = 5, community_2 = 3), set = "primary")
export_points(batched, out_dir = "output", set = "primary")
map_all_communities(samples, communities, out_dir = "output/maps")

# Interactive leaflet map with buildings, roads, and navigation
leaflet_communities(
  batched, communities,
  buildings_sf = buildings,
  roads_list = fetch_community_roads(communities, road_dir = "output/roads"),
  out_file = "output/maps/map.html"
)
```

See `vignette("sampling-pipeline")` for the full guide.

## Configuration

### Map tile API keys (optional)

The app works out of the box with free map providers (OpenStreetMap, ESRI
World Imagery, Maxar). For additional basemap options, configure API keys in
your `~/.Renviron` file:

| Provider | Env variable | Free tier | Get a key |
|---|---|---|---|
| Google Maps | `MAPS_API_KEY_GOOGLE` | 28 000 map loads/month | [Google Cloud Console](https://console.cloud.google.com/apis/credentials) |
| Mapbox | `MAPS_API_KEY_MAPBOX` | 200 000 tile requests/month | [Mapbox tokens](https://account.mapbox.com/access-tokens/) |
| Bing Maps | `MAPS_API_KEY_BING` | 125 000 sessions/year | [Bing Maps Portal](https://www.bingmapsportal.com/Application) |
| Maxar | `MAPS_API_KEY_MAXAR` | Built-in default key | Override with your own |

Only basemap options with valid API keys appear in the layer switcher. Building
detection works without any API keys.

### Logging

Logging is powered by `log4r`. Configure via environment variables
(in `~/.Renviron` or at runtime):

| Variable | Default | Description |
|---|---|---|
| `GPSSAMPLING_LOG_LEVEL` | `DEBUG` | Log threshold: `DEBUG`, `INFO`, `WARN`, `ERROR`, `FATAL` |
| `GPSSAMPLING_LOG_FORMAT` | `text` | Set to `json` for structured JSON log output |
| `GPSSAMPLING_LOG_DIR` | *(none)* | Directory for persistent log files (with rotation) |

Example:

```bash
# In ~/.Renviron
GPSSAMPLING_LOG_LEVEL=INFO
GPSSAMPLING_LOG_DIR=~/gpssampling-logs
```

### Mergin integration (optional)

The package includes utilities for syncing projects with a
[Mergin](https://merginmaps.com/) server (Windows only, requires `mergin.exe`).
Configure via environment variables:

| Variable | Default | Description |
|---|---|---|
| `MERGIN_URL` | `https://mergin.msf.net/` | Mergin server URL |
| `MERGIN_USER` | *(required)* | Mergin account username |
| `MERGIN_PASSWORD` | *(required)* | Mergin account password |

### Method tracing

For debugging, enable method-level tracing to log every R6 method call with
timing:

```r
options(gpssampling.trace = TRUE)
samp <- sampler()
samp$launch()
```

Or enable per-instance:

```r
samp <- sampler(.trace = TRUE)
```

## Development

Development happens on the `dev` branch:

```bash
git clone https://github.com/epicentre-msf/gpssampling.git
cd gpssampling
git checkout dev
```

**Branch workflow:**

- `dev` — active development, CI runs R-CMD-check on push
- `main` — stable releases, protected branch, auto-release on push

## Warnings

1. **No warranty.** This package is provided "as is". AI-assisted modifications
   have not been reviewed by the original developers.
2. **Not for production.** Intended for internal exploration and learning. Do
   not use for critical field operations without independent validation.
3. **Sampling accuracy.** Results have not been independently verified.
   Cross-check against the official GeoSampler application.
4. **Data integrity.** Data is stored locally via SQLite. No guarantees on
   persistence across versions.
5. **Security.** SQL injection vulnerabilities have been patched, but a full
   audit has not been performed. Do not expose to untrusted networks.
6. **Dependencies.** Several private GitHub dependencies have been replaced with
   internal bridge implementations that may not cover all edge cases.
7. **No support.** Refer to the
   [official GeoSampler documentation](https://apps.msf.net/geosampler/site/index.html)
   for help.
