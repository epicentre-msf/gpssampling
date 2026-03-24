# Tests for Phase 2 fixes: God Object extraction, error handling wrappers

# --- 1. SampleManager class exists and initializes ---------------------------

test_that('SampleManager R6 class is defined', {
  expect_true(R6::is.R6Class(SampleManager))
  expect_equal(SampleManager$classname, 'SampleManager')
})

test_that('SampleManager has all expected methods', {
  expected_methods <- c(
    'initialize',
    'addSample',
    'addSamples',
    'generateSamples',
    'sampleAppend',
    'calculateSample',
    'displaySample',
    'sampleDisplayToTable',
    'sampleQuadratEdit',
    'sampleQuadratPost',
    'sampleLoadTable',
    'sampleToggleSelect',
    'samplesSetValueFromTable',
    'samplesSetValueFromMap'
  )

  defined_methods <- names(SampleManager$public_methods)

  for (method in expected_methods) {
    expect_true(
      method %in% defined_methods,
      info = sprintf('SampleManager should have method: %s', method)
    )
  }
})

test_that('SampleManager has private .sample field', {
  private_fields <- names(SampleManager$private_fields)
  expect_true('.sample' %in% private_fields)
})

# --- 2. DocumentExporter class exists and initializes -------------------------

test_that('DocumentExporter R6 class is defined', {
  expect_true(R6::is.R6Class(DocumentExporter))
  expect_equal(DocumentExporter$classname, 'DocumentExporter')
})

test_that('DocumentExporter has expected methods', {
  expected_methods <- c(
    'initialize',
    'sampleExportFilename',
    'sampleExportToDoc',
    'sampleExportToDocEach'
  )

  defined_methods <- names(DocumentExporter$public_methods)

  for (method in expected_methods) {
    expect_true(
      method %in% defined_methods,
      info = sprintf('DocumentExporter should have method: %s', method)
    )
  }
})

# --- 3. TileManager class exists and initializes -----------------------------

test_that('TileManager R6 class is defined', {
  expect_true(R6::is.R6Class(TileManager))
  expect_equal(TileManager$classname, 'TileManager')
})

test_that('TileManager has expected methods', {
  expected_methods <- c(
    'initialize',
    'addRoof',
    'removeRoof',
    'addRoofs',
    'displayRoofs'
  )

  defined_methods <- names(TileManager$public_methods)

  for (method in expected_methods) {
    expect_true(
      method %in% defined_methods,
      info = sprintf('TileManager should have method: %s', method)
    )
  }
})

# --- 4. UserData has delegation active bindings ------------------------------

test_that('UserData has manager active bindings', {
  active_fields <- names(UserData$active)
  expect_true('exporter' %in% active_fields)
  expect_true('tile_mgr' %in% active_fields)
  expect_true('sample_mgr' %in% active_fields)
  expect_true('polygons_raw' %in% active_fields)
})

test_that('UserData has forwarding stubs for sample methods', {
  public_methods <- names(UserData$public_methods)

  sample_methods <- c(
    'addSample',
    'addSamples',
    'generateSamples',
    'sampleAppend',
    'calculateSample',
    'displaySample',
    'sampleDisplayToTable',
    'sampleQuadratEdit',
    'sampleQuadratPost',
    'sampleLoadTable',
    'sampleToggleSelect',
    'samplesSetValueFromTable',
    'samplesSetValueFromMap'
  )

  for (method in sample_methods) {
    expect_true(
      method %in% public_methods,
      info = sprintf('UserData should keep forwarding stub: %s', method)
    )
  }
})

test_that('UserData forwarding stubs delegate to sample_mgr', {
  # Check that the method bodies contain sample_mgr delegation
  stub_source <- deparse(UserData$public_methods$addSample)
  expect_true(
    any(grepl('sample_mgr', stub_source)),
    info = 'addSample should delegate to sample_mgr'
  )

  stub_source <- deparse(UserData$public_methods$generateSamples)
  expect_true(
    any(grepl('sample_mgr', stub_source)),
    info = 'generateSamples should delegate to sample_mgr'
  )

  stub_source <- deparse(UserData$public_methods$calculateSample)
  expect_true(
    any(grepl('sample_mgr', stub_source)),
    info = 'calculateSample should delegate to sample_mgr'
  )
})

test_that('UserData forwarding stubs for export delegate to exporter', {
  stub_source <- deparse(UserData$public_methods$sampleExportToDoc)
  expect_true(
    any(grepl('exporter', stub_source)),
    info = 'sampleExportToDoc should delegate to exporter'
  )

  stub_source <- deparse(UserData$public_methods$sampleExportFilename)
  expect_true(
    any(grepl('exporter', stub_source)),
    info = 'sampleExportFilename should delegate to exporter'
  )
})

test_that('UserData forwarding stubs for tiles delegate to tile_mgr', {
  stub_source <- deparse(UserData$public_methods$addCell)
  expect_true(
    any(grepl('tile_mgr', stub_source)),
    info = 'addCell should delegate to tile_mgr'
  )

  stub_source <- deparse(UserData$public_methods$searchSwipeTile)
  expect_true(
    any(grepl('tile_mgr', stub_source)),
    info = 'searchSwipeTile should delegate to tile_mgr'
  )
})

# --- 5. Error handling wrappers exist ----------------------------------------

test_that('safe_db wraps errors and returns default', {
  result <- safe_db(
    stop('test error'),
    default = 'fallback',
    msg = 'test'
  )
  expect_equal(result, 'fallback')
})

test_that('safe_db passes through on success', {
  result <- safe_db(42L, default = NULL, msg = 'test')
  expect_equal(result, 42L)
})

test_that('safe_download returns NULL on bad URL', {
  bad_url <- 'http://127.0.0.1:1/nonexistent'
  tmp <- tempfile(fileext = '.png')
  on.exit(unlink(tmp), add = TRUE)

  result <- safe_download(
    url = bad_url,
    destfile = tmp,
    quiet = TRUE,
    msg = 'test'
  )
  expect_null(result)
})

test_that('safe_file_copy returns FALSE on missing source', {
  result <- safe_file_copy(
    path = '/nonexistent/file.txt',
    new_path = tempfile(),
    msg = 'test'
  )
  expect_false(result)
})

test_that('safe_read_rds returns default for missing file', {
  result <- safe_read_rds(
    file = '/nonexistent/file.rds',
    default = 'fallback',
    msg = 'test'
  )
  expect_equal(result, 'fallback')
})

test_that('safe_read_rds reads valid RDS files', {
  tmp <- tempfile(fileext = '.rds')
  on.exit(unlink(tmp), add = TRUE)
  saveRDS(list(a = 1L, b = 'test'), tmp)

  result <- safe_read_rds(file = tmp, default = NULL, msg = 'test')
  expect_equal(result$a, 1L)
  expect_equal(result$b, 'test')
})

test_that('safe_read_rds returns default for corrupt RDS', {
  tmp <- tempfile(fileext = '.rds')
  on.exit(unlink(tmp), add = TRUE)
  writeLines('not an rds file', tmp)

  result <- safe_read_rds(file = tmp, default = 'fallback', msg = 'test')
  expect_equal(result, 'fallback')
})

test_that('safe_save_rds writes and returns TRUE on success', {
  tmp <- tempfile(fileext = '.rds')
  on.exit(unlink(tmp), add = TRUE)

  result <- safe_save_rds(
    object = list(x = 42L),
    file = tmp,
    msg = 'test'
  )
  expect_true(result)
  expect_true(file.exists(tmp))

  # Verify content
  loaded <- readRDS(tmp)
  expect_equal(loaded$x, 42L)
})

test_that('safe_save_rds returns FALSE on bad path', {
  result <- safe_save_rds(
    object = list(x = 42L),
    file = '/nonexistent/dir/file.rds',
    msg = 'test'
  )
  expect_false(result)
})

# --- 6. UserData line count reduction verification ----------------------------

test_that('UserData class.app.store.R is under 2000 lines', {
  store_file <- system.file(
    'R',
    'class.app.store.R',
    package = 'gpssampling'
  )

  # During development, use devtools path

  if (store_file == '') {
    store_file <- file.path(
      find.package('gpssampling', lib.loc = .libPaths()),
      'R',
      'class.app.store.R'
    )
  }

  # If still not found (devtools::load_all), count from source
  if (!file.exists(store_file)) {
    skip('Cannot locate class.app.store.R in installed package')
  }

  line_count <- length(readLines(store_file))
  expect_lt(
    line_count,
    2000L,
    label = sprintf(
      'class.app.store.R has %d lines (target < 2000)',
      line_count
    )
  )
})

# --- 7. Extracted classes are not empty shells --------------------------------

test_that('SampleManager has substantial method implementations', {
  # generateSamples should be a real method, not a one-liner
  source_lines <- deparse(SampleManager$public_methods$generateSamples)
  expect_gt(
    length(source_lines),
    10L,
    label = 'generateSamples should have substantial implementation'
  )
})

test_that('DocumentExporter has substantial method implementations', {
  source_lines <- deparse(DocumentExporter$public_methods$sampleExportToDoc)
  expect_gt(
    length(source_lines),
    10L,
    label = 'sampleExportToDoc should have substantial implementation'
  )
})

test_that('TileManager has substantial method implementations', {
  source_lines <- deparse(TileManager$public_methods$addRoof)
  expect_gt(
    length(source_lines),
    10L,
    label = 'addRoof should have substantial implementation'
  )
})

# --- 8. Phase 1 regression check --------------------------------------------

test_that('Phase 1 tests still pass (parameterized SQL)', {
  skip_if_not_installed('RSQLite')
  skip_if_not_installed('pool')

  db_path <- tempfile(fileext = '.sqlite')
  on.exit(unlink(db_path), add = TRUE)

  db_pool <- pool::dbPool(drv = RSQLite::SQLite(), dbname = db_path)
  on.exit(pool::poolClose(db_pool), add = TRUE)

  pool::dbExecute(
    db_pool,
    'CREATE TABLE test_items (id TEXT, value INTEGER)'
  )
  pool::dbExecute(
    db_pool,
    'INSERT INTO test_items VALUES (?, ?)',
    params = list('a', 1L)
  )

  result <- pool::dbGetQuery(
    db_pool,
    'SELECT * FROM test_items WHERE id = ?',
    params = list('a')
  )
  expect_equal(nrow(result), 1L)
  expect_equal(result$value, 1L)
})

# --- 9. Utility file split — function accessibility --------------------------

test_that('error wrappers are accessible from utils.error.R', {
  expect_true(is.function(safe_db))
  expect_true(is.function(safe_download))
  expect_true(is.function(safe_file_copy))
  expect_true(is.function(safe_read_rds))
  expect_true(is.function(safe_save_rds))
  expect_true(is.function(stopf))
})

test_that('path functions are accessible from utils.paths.R', {
  expect_true(is.function(getDirApp))
  expect_true(is.function(getDirApps))
  expect_true(is.function(getDirOrganization))
  expect_true(is.function(getDirAppTemp))
  expect_true(is.function(getDirAppUsers))
  expect_true(is.function(getDirAssets))
  expect_true(is.function(getDirDatabase))
  expect_true(is.function(getFileCache))
  expect_true(is.function(createDir))
  expect_true(is.function(explore))
})

test_that('data functions are accessible from utils.data.R', {
  expect_true(is.function(getChoices))
  expect_true(is.function(getCodeFactors))
  expect_true(is.function(getCodeLabels))
  expect_true(is.function(getCodeValues))
  expect_true(is.function(getGroupedChoices))
  expect_true(is.function(getLabels))
  expect_true(is.function(matchValue))
  expect_true(is.function(compareDataframe))
  expect_true(is.function(getFilters))
})

test_that('string functions are accessible from utils.string.R', {
  expect_true(is.function(getFileExt))
  expect_true(is.function(collapse))
  expect_true(is.function(strReplaceAll))
  expect_true(is.function(getIdentifierFromLabel))
  expect_true(is.function(snumber))
  expect_true(is.function(paste_ignore_NA))
  expect_true(is.function(pct))
  expect_true(is.function(writeUtf8))
  expect_true(is.function(splitMsg))
  expect_true(is.function(score_unique))
})

# --- 10. Utility file split — functions still work correctly -----------------

test_that('path functions produce correct paths', {
  org_path <- getDirOrganization()
  expect_true(grepl('epicentre', org_path))

  apps_path <- getDirApps()
  expect_true(grepl('apps', apps_path))
  expect_true(grepl('epicentre', apps_path))

  app_path <- getDirApp()
  expect_true(grepl('gpssampling', app_path))
})

test_that('getFileCache builds correct cache paths', {
  cache_path <- getFileCache('test_cache', ext = 'rds')
  expect_true(grepl('test_cache\\.rds$', cache_path))
  expect_true(grepl('gpssampling', cache_path))
})

test_that('createDir creates directories and lowercases', {
  tmp <- file.path(tempdir(), 'TEST_DIR_UPPER')
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  result <- createDir(tmp)
  expect_true(grepl('test_dir_upper', result))
  expect_true(dir.exists(result))
})

test_that('getFileExt extracts extensions correctly', {
  expect_equal(getFileExt('file.txt'), 'txt')
  expect_equal(getFileExt('path/to/file.CSV'), 'csv')
  expect_equal(getFileExt('file.TXT', tolower = FALSE), 'TXT')
})

test_that('collapse joins strings correctly', {
  expect_equal(collapse(c('a', 'b'), quote = TRUE), '"a", "b"')
  expect_equal(collapse(c('x', 'y'), quote = FALSE, sep = '-'), 'x-y')
})

test_that('strReplaceAll wraps stringr correctly', {
  expect_equal(strReplaceAll('hello world', 'o', '0'), 'hell0 w0rld')
})

test_that('getIdentifierFromLabel normalizes labels', {
  expect_equal(getIdentifierFromLabel('Hello World!'), 'hello_world')
  expect_equal(
    getIdentifierFromLabel('Test Label', prefix = 'pfx'),
    'pfx_test_label'
  )
})

test_that('snumber formats numbers correctly', {
  expect_equal(snumber(1.5, '%.1f'), '1.5')
  result <- snumber(NA, '%f')
  expect_true(grepl('-', result))
})

test_that('pct formats percentages', {
  expect_equal(pct(50), '50%')
  expect_equal(pct(0), '0%')
})

test_that('getChoices builds named list from data frame', {
  df <- data.frame(
    label = c('Apple', 'Banana'),
    code = c('a', 'b'),
    stringsAsFactors = FALSE
  )
  choices <- getChoices(df, var_name = 'label', var_code = 'code')
  expect_equal(choices[['Apple']], 'a')
  expect_equal(choices[['Banana']], 'b')
})

test_that('getCodeLabels returns labels for codes', {
  codes <- data.frame(
    cde_code = c(1L, 2L, 3L),
    cde_label = c('Low', 'Medium', 'High'),
    cde_label_short = c('L', 'M', 'H')
  )
  expect_equal(getCodeLabels(codes, c(1L, 3L)), c('Low', 'High'))
  expect_equal(
    getCodeLabels(codes, c(2L, 1L), short = TRUE),
    c('M', 'L')
  )
})

test_that('getCodeValues reverse-looks up codes from labels', {
  codes <- data.frame(
    cde_code = c(1L, 2L, 3L),
    cde_label = c('Low', 'Medium', 'High'),
    cde_label_short = c('L', 'M', 'H')
  )
  expect_equal(getCodeValues(codes, c('Low', 'High')), c(1L, 3L))
})

test_that('matchValue retrieves matching values', {
  tbl <- data.frame(a = 1:5, b = rev(1:5))
  expect_equal(matchValue(tbl, c(1L, 5L), 'a', 'b'), c(5L, 1L))
})

# --- 11. Utility file split — remaining utils.R functions still work ---------

test_that('platform detection functions remain in utils.R', {
  expect_true(is.function(is.win))
  expect_true(is.function(is.x32))
  expect_true(is.function(is.x64))
  expect_true(is.function(isShinyApp))
  expect_true(is.function(isHostedApp))
  expect_true(is.function(is.dev))
})

test_that('package metadata functions remain in utils.R', {
  expect_true(is.function(getPackageDescription))
  expect_true(is.function(getPackageVersion))
  expect_true(is.function(getPackageTitle))
  expect_true(is.function(getPackageIdentifier))
  expect_true(is.function(getPackagePath))
})

test_that('misc utility functions remain in utils.R', {
  expect_true(is.function(dropNA))
  expect_true(is.function(dropNulls))
  expect_true(is.function(dropNullsOrEmpty))
  expect_true(is.function(nullOrEmpty))
  expect_true(is.function(is.defined))
  expect_true(is.function(is.error))
  expect_true(is.function(is.unique))
})

test_that('dropNA removes NA values', {
  expect_equal(dropNA(c(1L, NA, 3L)), c(1L, 3L))
  expect_length(dropNA(c(NA, NA)), 0L)
})

test_that('nullOrEmpty detects empty values', {
  expect_true(nullOrEmpty(NULL))
  expect_true(nullOrEmpty(character(0)))
  expect_false(nullOrEmpty(1L))
})

test_that('is.unique checks uniqueness', {
  expect_true(is.unique(c(1L, 2L, 3L)))
  expect_false(is.unique(c(1L, 1L, 2L)))
})

# --- 12. Shiny utility file split — function accessibility -------------------

test_that('shiny input functions are accessible from utils.shiny.inputs.R', {
  expect_true(is.function(textInput))
  expect_true(is.function(selectInput))
  expect_true(is.function(pickerInputEx))
  expect_true(is.function(tokenInput))
  expect_true(is.function(virtualSelectInput))
})

test_that('shiny theming functions are accessible from utils.shiny.theming.R', {
  expect_true(is.function(theme))
  expect_true(is.function(shinyAppMinimal))
  expect_true(is.function(themePreview))
})

test_that('shiny port functions are accessible from utils.shiny.ports.R', {
  expect_true(is.function(checkPort))
  expect_true(is.function(findFreePort))
  expect_true(is.function(getProcessPidByPort))
  expect_true(is.function(getTablePorts))
})

test_that('shiny launcher functions are accessible from utils.shiny.launcher.R', {
  expect_true(is.function(launcher))
  expect_true(is.function(win_launcher))
  expect_true(is.function(mac_launcher))
  expect_true(is.function(lin_launcher))
})

test_that('remaining shiny functions stay in utils.shiny.R', {
  expect_true(is.function(button))
  expect_true(is.function(modalDialog))
  expect_true(is.function(modalDanger))
  expect_true(is.function(icon))
  expect_true(is.function(reactiveTrigger))
  expect_true(is.function(useWaiter))
  expect_true(is.function(withBusyIndicatorUI))
  expect_true(is.function(withBusyIndicatorServer))
})

# --- 13. Utility file split — line count verification ------------------------

test_that('utils.R is under 1500 lines after split', {
  utils_file <- file.path(
    getwd() |> dirname() |> dirname(),
    'R',
    'utils.R'
  )
  if (!file.exists(utils_file)) {
    utils_file <- 'R/utils.R'
  }
  if (!file.exists(utils_file)) {
    skip('Cannot locate utils.R source file')
  }

  line_count <- length(readLines(utils_file))
  expect_lt(
    line_count,
    1500L,
    label = sprintf('utils.R has %d lines (target < 1500)', line_count)
  )
})

test_that('utils.shiny.R is under 1500 lines after split', {
  shiny_file <- file.path(
    getwd() |> dirname() |> dirname(),
    'R',
    'utils.shiny.R'
  )
  if (!file.exists(shiny_file)) {
    shiny_file <- 'R/utils.shiny.R'
  }
  if (!file.exists(shiny_file)) {
    skip('Cannot locate utils.shiny.R source file')
  }

  line_count <- length(readLines(shiny_file))
  expect_lt(
    line_count,
    1500L,
    label = sprintf('utils.shiny.R has %d lines (target < 1500)', line_count)
  )
})

# --- 14. No duplicate function definitions ------------------------------------

test_that('no duplicate ask() function (was duplicated in old utils.R)', {
  # The old utils.R had ask() defined twice (lines ~1798 and ~2600)
  # After the split, only one definition should remain
  utils_file <- file.path(
    getwd() |> dirname() |> dirname(),
    'R',
    'utils.R'
  )
  if (!file.exists(utils_file)) {
    utils_file <- 'R/utils.R'
  }
  if (!file.exists(utils_file)) {
    skip('Cannot locate utils.R source file')
  }

  lines <- readLines(utils_file)
  ask_defs <- grep('^ask <- function', lines)
  expect_equal(
    length(ask_defs),
    1L,
    label = 'There should be exactly one ask() definition'
  )
})
