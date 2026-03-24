# Tests for Phase 3 fixes: PolygonManager, DataPersistence, config module,
# error handling, commented code cleanup, French comment translation

# --- 1. PolygonManager class exists and initializes --------------------------

test_that('PolygonManager R6 class is defined', {
  expect_true(R6::is.R6Class(PolygonManager))
  expect_equal(PolygonManager$classname, 'PolygonManager')
})

test_that('PolygonManager has all expected methods', {
  expected_methods <- c(
    'initialize',
    'updateCells',
    'drawNewFeature',
    'displayGuidePolygon',
    'displayGuidePoint',
    'invertSelection',
    'cutPolygons',
    'makeValidPolygons',
    'displayPolygons',
    'addPolygon',
    'deletePolygon',
    'editPolygon',
    'togglePolygonSelected',
    'togglePolygonFocused',
    'setStylePolygonFocused'
  )

  defined_methods <- names(PolygonManager$public_methods)

  for (method in expected_methods) {
    expect_true(
      method %in% defined_methods,
      info = sprintf('PolygonManager should have method: %s', method)
    )
  }
})

test_that('PolygonManager stores data reference', {
  expect_true('data' %in% names(PolygonManager$public_fields))
})

# --- 2. DataPersistence class exists and initializes -------------------------

test_that('DataPersistence R6 class is defined', {
  expect_true(R6::is.R6Class(DataPersistence))
  expect_equal(DataPersistence$classname, 'DataPersistence')
})

test_that('DataPersistence has all expected methods', {
  expected_methods <- c(
    'initialize',
    'load',
    'save',
    'clear',
    'rollback',
    'readFromCache',
    'saveToCache',
    'addProject',
    'projectClone',
    'projectEdit',
    'projectDelete',
    'projectSelect'
  )

  defined_methods <- names(DataPersistence$public_methods)

  for (method in expected_methods) {
    expect_true(
      method %in% defined_methods,
      info = sprintf('DataPersistence should have method: %s', method)
    )
  }
})

test_that('DataPersistence stores data reference', {
  expect_true('data' %in% names(DataPersistence$public_fields))
})

# --- 3. UserData forwarding stubs for PolygonManager -------------------------

test_that('UserData has polygon_mgr active binding', {
  active_fields <- names(UserData$active)
  expect_true('polygon_mgr' %in% active_fields)
})

test_that('UserData has polygon forwarding methods', {
  expected_methods <- c(
    'updateCells',
    'drawNewFeature',
    'displayGuidePolygon',
    'displayGuidePoint',
    'invertSelection',
    'cutPolygons',
    'makeValidPolygons',
    'displayPolygons',
    'addPolygon',
    'deletePolygon',
    'editPolygon',
    'togglePolygonSelected',
    'togglePolygonFocused',
    'setStylePolygonFocused'
  )

  defined_methods <- names(UserData$public_methods)

  for (method in expected_methods) {
    expect_true(
      method %in% defined_methods,
      info = sprintf('UserData should still have forwarding stub: %s', method)
    )
  }
})

# --- 4. UserData forwarding stubs for DataPersistence ------------------------

test_that('UserData has persistence active binding', {
  active_fields <- names(UserData$active)
  expect_true('persistence' %in% active_fields)
})

test_that('UserData has persistence forwarding methods', {
  expected_methods <- c(
    'load',
    'save',
    'clear',
    'rollback',
    'readFromCache',
    'saveToCache',
    'addProject',
    'projectClone',
    'projectEdit',
    'projectDelete',
    'projectSelect'
  )

  defined_methods <- names(UserData$public_methods)

  for (method in expected_methods) {
    expect_true(
      method %in% defined_methods,
      info = sprintf('UserData should still have forwarding stub: %s', method)
    )
  }
})

test_that('UserData has project_active_raw active binding', {
  active_fields <- names(UserData$active)
  expect_true('project_active_raw' %in% active_fields)
})

test_that('UserData has triggerProjectName method', {
  expect_true('triggerProjectName' %in% names(UserData$public_methods))
})

# --- 5. Config module constants exist ----------------------------------------

test_that('Zoom level constants are defined', {
  expect_equal(ZOOM_TILE, 18L)
  expect_equal(ZOOM_OSM, 13L)
  expect_equal(ZOOM_SAMPLE, 20L)
})

test_that('Grid size constant is defined', {
  expect_equal(GRID_SIZE, 3L)
})

test_that('Default sample parameter constants are defined', {
  expect_equal(DEFAULT_SAMPLE_COUNT, 35L)
  expect_equal(DEFAULT_SAMPLE_SIZE, 200L)
  expect_equal(DEFAULT_CONFIDENCE, 95L)
})

test_that('Default settings constants are defined', {
  expect_equal(DEFAULT_CSI_CIRCLE_COLOR, '#80FF00')
  expect_equal(DEFAULT_SLI_CIRCLE_RADIUS, 2L)
  expect_equal(DEFAULT_SLI_CIRCLE_OPACITY, 25L)
  expect_equal(DEFAULT_SLI_SAMPLE_RADIUS, 25L)
})

test_that('Tile palette constants are defined', {
  expect_equal(TILE_PALETTE_DOMAIN, -2:3)
  expect_length(TILE_PALETTE_COLORS, 6L)
})

test_that('Polygon display constants are defined', {
  expect_equal(POLYGON_WEIGHT_FOCUSED, 4L)
  expect_equal(POLYGON_WEIGHT_DEFAULT, 2L)
})

test_that('OSM fetching constant is defined', {
  expect_equal(OSM_MAX_TILES, 70L)
})

# --- 6. Error handling coverage ----------------------------------------------

test_that('All DB operations in class.app.store.R are wrapped with safe_db', {
  source_code <- read_pkg_file('R/class.app.store.R')

  raw_db_calls <- grep(
    'db\\$db(GetQuery|Execute|WriteTable)',
    source_code,
    value = TRUE
  )

  for (line in raw_db_calls) {
    trimmed <- trimws(line)
    is_wrapper_def <- grepl('pool::', trimmed)
    is_wrapped <- grepl('safe_db', trimmed)
    is_inside_safe_db <- any(grepl('safe_db', source_code[
      max(1L, which(source_code == line) - 3L):which(source_code == line)
    ]))

    expect_true(
      is_wrapper_def || is_wrapped || is_inside_safe_db,
      info = sprintf('Unprotected DB call found: %s', trimmed)
    )
  }
})

test_that('All DB operations in class.tile.manager.R are wrapped with safe_db', {
  source_code <- read_pkg_file('R/class.tile.manager.R')

  db_lines <- grep('data\\$db\\$db', source_code)

  for (idx in db_lines) {
    context <- source_code[max(1L, idx - 5L):idx]
    has_safe_db <- any(grepl('safe_db', context))

    expect_true(
      has_safe_db,
      info = sprintf(
        'Unprotected DB call at line %d: %s',
        idx,
        trimws(source_code[idx])
      )
    )
  }
})

test_that('All DB operations in class.polygon.manager.R are wrapped with safe_db', {
  source_code <- read_pkg_file('R/class.polygon.manager.R')

  db_lines <- grep('data\\$db\\$db', source_code)

  for (idx in db_lines) {
    context <- source_code[max(1L, idx - 5L):idx]
    has_safe_db <- any(grepl('safe_db', context))

    expect_true(
      has_safe_db,
      info = sprintf(
        'Unprotected DB call at line %d: %s',
        idx,
        trimws(source_code[idx])
      )
    )
  }
})

test_that('All DB operations in class.data.persistence.R are wrapped with safe_db', {
  source_code <- read_pkg_file('R/class.data.persistence.R')

  db_lines <- grep('data\\$db\\$db', source_code)

  for (idx in db_lines) {
    context <- source_code[max(1L, idx - 5L):idx]
    has_safe_db <- any(grepl('safe_db', context))

    expect_true(
      has_safe_db,
      info = sprintf(
        'Unprotected DB call at line %d: %s',
        idx,
        trimws(source_code[idx])
      )
    )
  }
})

# --- 7. Commented-out code removal -------------------------------------------

test_that('No terra::plot debug calls remain in class.app.store.R', {
  source_code <- read_pkg_file('R/class.app.store.R')
  terra_plots <- grep('terra::plot', source_code, value = TRUE)
  expect_length(terra_plots, 0L)
})

test_that('No commented-out shiny::observe blocks remain in class.app.store.R', {
  source_code <- read_pkg_file('R/class.app.store.R')
  commented_observe <- grep(
    '^\\s*#\\s*shiny::observe',
    source_code,
    value = TRUE
  )
  expect_length(commented_observe, 0L)
})

test_that('No commented-out hintjs or tab module calls remain', {
  source_code <- read_pkg_file('R/class.app.store.R')
  dead_code <- grep(
    '^\\s*#\\s*(hintjs|tabSplash|tabSteps|onBookmarked)',
    source_code,
    value = TRUE
  )
  expect_length(dead_code, 0L)
})

# --- 8. French comment removal ----------------------------------------------

test_that('No French FIXME/TODO comments remain in class.app.store.R', {
  source_code <- read_pkg_file('R/class.app.store.R')
  french <- grep(
    "l'image est decalee|Gestion du mode de tirage",
    source_code,
    value = TRUE
  )
  expect_length(french, 0L)
})

test_that('No French placeholder keywords remain in utils.R', {
  source_code <- read_pkg_file('R/utils.R')
  french_keywords <- grep('truc machin|truc$', source_code, value = TRUE)
  expect_length(french_keywords, 0L)
})

# --- 9. DESCRIPTION placeholder removed -------------------------------------

test_that('DESCRIPTION has a real package description', {
  desc <- utils::packageDescription('gpssampling')
  expect_false(
    grepl('What the package does', desc$Description),
    info = 'DESCRIPTION should not have placeholder text'
  )
})

# --- 10. Roxygen placeholder removal ----------------------------------------

test_that('No FUNCTION_TITLE or FUNCTION_DESCRIPTION placeholders remain', {
  r_files <- read_pkg_file('R/utils.R')
  # Check all R source files via the package namespace
  r_dir <- system.file('R', package = 'gpssampling')
  if (nzchar(r_dir) && dir.exists(r_dir)) {
    all_files <- list.files(r_dir, pattern = '\\.R$', full.names = TRUE)
  } else {
    r_dir2 <- tryCatch(here::here('R'), error = function(e) '../../R')
    all_files <- list.files(r_dir2, pattern = '\\.R$', full.names = TRUE)
  }

  for (f in all_files) {
    content <- readLines(f)
    placeholders <- grep(
      'FUNCTION_TITLE|FUNCTION_DESCRIPTION|PARAM_DESCRIPTION',
      content,
      value = TRUE
    )
    expect_true(
      length(placeholders) == 0L,
      info = sprintf('Placeholder found in %s', basename(f))
    )
  }
})

# --- 11. Line count verification ---------------------------------------------

test_that('class.app.store.R is under 1000 lines', {
  source_code <- read_pkg_file('R/class.app.store.R')
  expect_lt(length(source_code), 1000L)
})

test_that('class.polygon.manager.R exists and has content', {
  source_code <- read_pkg_file('R/class.polygon.manager.R')
  expect_gt(length(source_code), 100L)
})

test_that('class.data.persistence.R exists and has content', {
  source_code <- read_pkg_file('R/class.data.persistence.R')
  expect_gt(length(source_code), 100L)
})

test_that('config.R exists and has content', {
  source_code <- read_pkg_file('R/config.R')
  expect_gt(length(source_code), 30L)
})

# --- 12. Package loads cleanly -----------------------------------------------

test_that('Package loads without errors', {
  expect_true(requireNamespace('gpssampling', quietly = TRUE))
})
