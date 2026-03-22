#' @title Application Configuration Constants
#' @description Centralizes magic numbers, default values, and configuration
#'   constants used throughout the gpssampling package.
#' @noRd

# Zoom Levels
# ............................................................................

#' Default zoom level for tile-based operations (identification, cell mapping).
#' @noRd
ZOOM_TILE <- 18L

#' Zoom level for OSM building data fetching.
#' @noRd
ZOOM_OSM <- 13L

#' Default zoom for sample point display.
#' @noRd
ZOOM_SAMPLE <- 20L

# Grid Configuration
# ............................................................................

#' Grid cell subdivision count per tile axis (3x3 = 9 cells per tile).
#' @noRd
GRID_SIZE <- 3L

# Default Sample Parameters
# ............................................................................

#' Default sample count for new polygons.
#' @noRd
DEFAULT_SAMPLE_COUNT <- 35L

#' Default sample size for power calculations.
#' @noRd
DEFAULT_SAMPLE_SIZE <- 200L

#' Default confidence level (percentage) for sample calculations.
#' @noRd
DEFAULT_CONFIDENCE <- 95L

# Default Settings Values
# ............................................................................

#' Default color for CSI circle markers.
#' @noRd
DEFAULT_CSI_CIRCLE_COLOR <- '#80FF00'

#' Default radius for SLI circle markers.
#' @noRd
DEFAULT_SLI_CIRCLE_RADIUS <- 2L

#' Default opacity for SLI circle markers.
#' @noRd
DEFAULT_SLI_CIRCLE_OPACITY <- 25L

#' Default radius for sample point markers.
#' @noRd
DEFAULT_SLI_SAMPLE_RADIUS <- 25L

# Tile Palette
# ............................................................................

#' Status codes for tile display palette.
#' @noRd
TILE_PALETTE_DOMAIN <- -2:3

#' Tile status color palette (red/black/white/yellow/green/red).
#' @noRd
TILE_PALETTE_COLORS <- c(
  scales::alpha('red', 0.5),
  scales::alpha('black', 0L),
  scales::alpha('white', 0.5),
  scales::alpha('yellow', 0.5),
  scales::alpha('green', 0.5),
  scales::alpha('red', 0.5)
)

# Polygon Display
# ............................................................................

#' Stroke weight for focused polygons.
#' @noRd
POLYGON_WEIGHT_FOCUSED <- 4L

#' Stroke weight for unfocused polygons.
#' @noRd
POLYGON_WEIGHT_DEFAULT <- 2L

# OSM Fetching
# ............................................................................

#' Maximum number of tiles to iterate when fetching OSM buildings.
#' @noRd
OSM_MAX_TILES <- 70L

# Basemap API Keys
# ............................................................................

#' Default Maxar/DigitalGlobe connection ID (OSM community shared key).
#' Override via env var MAPS_API_KEY_MAXAR.
#' @noRd
DEFAULT_MAXAR_CONNECT_ID <- "552c824a-5d4b-4bea-969f-06c8b50b80bc"

#' Default basemap when no API keys are configured.
#' @noRd
DEFAULT_BASEMAP <- "sat.esri"
