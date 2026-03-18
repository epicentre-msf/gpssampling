# Tests for Phase 1 fixes: SQL injection, zzz.R cleanup, Suggests migration,
# multipolygon cut/edit

# --- 1. Parameterized SQL queries -------------------------------------------

test_that('pool supports parameterized queries (INSERT/SELECT/UPDATE/DELETE)', {
  skip_if_not_installed('RSQLite')
  skip_if_not_installed('pool')

  db_path <- tempfile(fileext = '.sqlite')
  on.exit(unlink(db_path), add = TRUE)

  db_pool <- pool::dbPool(drv = RSQLite::SQLite(), dbname = db_path)
  on.exit(pool::poolClose(db_pool), add = TRUE)

  # CREATE

  pool::dbExecute(
    db_pool,
    'CREATE TABLE test_items (id TEXT, value INTEGER, label TEXT)'
  )

  # INSERT with params
  pool::dbExecute(
    db_pool,
    'INSERT INTO test_items VALUES (?, ?, ?)',
    params = list('item_1', 42L, 'hello')
  )

  # SELECT with params
  result <- pool::dbGetQuery(
    db_pool,
    'SELECT * FROM test_items WHERE id = ?',
    params = list('item_1')
  )
  expect_equal(nrow(result), 1L)
  expect_equal(result$value, 42L)
  expect_equal(result$label, 'hello')

  # UPDATE with params
  pool::dbExecute(
    db_pool,
    'UPDATE test_items SET value = ? WHERE id = ?',
    params = list(99L, 'item_1')
  )
  result <- pool::dbGetQuery(
    db_pool,
    'SELECT value FROM test_items WHERE id = ?',
    params = list('item_1')
  )
  expect_equal(result$value, 99L)

  # DELETE with params
  pool::dbExecute(
    db_pool,
    'DELETE FROM test_items WHERE id = ?',
    params = list('item_1')
  )
  result <- pool::dbGetQuery(db_pool, 'SELECT * FROM test_items')
  expect_equal(nrow(result), 0L)
})

test_that('Parameterized queries prevent SQL injection', {
  skip_if_not_installed('RSQLite')
  skip_if_not_installed('pool')

  db_path <- tempfile(fileext = '.sqlite')
  on.exit(unlink(db_path), add = TRUE)

  db_pool <- pool::dbPool(drv = RSQLite::SQLite(), dbname = db_path)
  on.exit(pool::poolClose(db_pool), add = TRUE)

  pool::dbExecute(db_pool, 'CREATE TABLE secrets (id TEXT, data TEXT)')
  pool::dbExecute(
    db_pool,
    'INSERT INTO secrets VALUES (?, ?)',
    params = list('safe', 'my_secret')
  )

  # Attempt SQL injection via parameter — treated as literal string
  malicious_id <- '" OR 1=1 --'
  result <- pool::dbGetQuery(
    db_pool,
    'SELECT * FROM secrets WHERE id = ?',
    params = list(malicious_id)
  )
  expect_equal(nrow(result), 0L)

  # Original row is still there
  result <- pool::dbGetQuery(db_pool, 'SELECT * FROM secrets')
  expect_equal(nrow(result), 1L)
})

test_that('IN clause with dynamic placeholders works', {
  skip_if_not_installed('RSQLite')
  skip_if_not_installed('pool')

  db_path <- tempfile(fileext = '.sqlite')
  on.exit(unlink(db_path), add = TRUE)

  db_pool <- pool::dbPool(drv = RSQLite::SQLite(), dbname = db_path)
  on.exit(pool::poolClose(db_pool), add = TRUE)

  pool::dbExecute(db_pool, 'CREATE TABLE tokens (token TEXT, status INTEGER)')
  for (tok in c('aaa', 'bbb', 'ccc', 'ddd')) {
    pool::dbExecute(
      db_pool,
      'INSERT INTO tokens VALUES (?, ?)',
      params = list(tok, 1L)
    )
  }

  # Dynamic IN clause — the pattern used in class.app.store.R
  keep_tokens <- c('aaa', 'ccc')
  placeholders <- paste(rep('?', length(keep_tokens)), collapse = ', ')
  sql <- paste0('SELECT * FROM tokens WHERE token IN (', placeholders, ')')
  result <- pool::dbGetQuery(db_pool, sql, params = as.list(keep_tokens))
  expect_equal(nrow(result), 2L)
  expect_true(all(result$token %in% keep_tokens))
})

# --- 2. zzz.R cleanup -------------------------------------------------------

test_that('Package .onLoad does not call set.seed', {
  onload_body <- body(epi.geosampler:::.onLoad)
  onload_text <- paste(deparse(onload_body), collapse = '\n')
  expect_false(grepl('set\\.seed', onload_text))
})

test_that('Package .onLoad does not set stringsAsFactors', {
  onload_body <- body(epi.geosampler:::.onLoad)
  onload_text <- paste(deparse(onload_body), collapse = '\n')
  expect_false(grepl('stringsAsFactors', onload_text))
})

# --- 3. Suggests migration ---------------------------------------------------

test_that('Debug packages are in Suggests, not Imports', {
  desc_path <- system.file('DESCRIPTION', package = 'epi.geosampler')
  skip_if(desc_path == '', message = 'Package not installed')

  desc <- read.dcf(desc_path)
  imports <- desc[1L, 'Imports']
  suggests <- desc[1L, 'Suggests']

  debug_pkgs <- c('shinytest2', 'profvis', 'tictoc', 'diffdf', 'devtools')

  for (pkg in debug_pkgs) {
    expect_false(
      grepl(paste0('\\b', pkg, '\\b'), imports),
      info = sprintf('%s should not be in Imports', pkg)
    )
    expect_true(
      grepl(paste0('\\b', pkg, '\\b'), suggests),
      info = sprintf('%s should be in Suggests', pkg)
    )
  }
})

# --- 4. Multipolygon geometry handling ---------------------------------------

test_that('MULTIPOLYGON can be cast to POLYGON for splitting', {
  p1 <- sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0))))
  p2 <- sf::st_polygon(list(rbind(c(2, 2), c(3, 2), c(3, 3), c(2, 3), c(2, 2))))
  mp <- sf::st_multipolygon(list(p1, p2))
  mp_sf <- sf::st_sf(id = 'ZN_1', geometry = sf::st_sfc(mp, crs = 4326L))

  cast <- sf::st_cast(mp_sf, 'POLYGON')
  expect_equal(nrow(cast), 2L)
  expect_true(all(sf::st_geometry_type(cast) == 'POLYGON'))
})

test_that('lwgeom::st_split works on individual POLYGONs', {
  skip_if_not_installed('lwgeom')

  p1 <- sf::st_polygon(list(rbind(c(0, 0), c(2, 0), c(2, 2), c(0, 2), c(0, 0))))
  p_sf <- sf::st_sf(id = 'ZN_1', geometry = sf::st_sfc(p1, crs = 4326L))

  line <- sf::st_linestring(rbind(c(1, -1), c(1, 3)))
  line_sf <- sf::st_sfc(line, crs = 4326L)

  result <- lwgeom::st_split(p_sf, line_sf)
  extracted <- sf::st_collection_extract(result, 'POLYGON')
  expect_equal(nrow(extracted), 2L)
})

test_that('POLYGON can be cast to MULTIPOLYGON for editPolygon compatibility', {
  p_new <- sf::st_polygon(list(rbind(
    c(0, 0),
    c(1.5, 0),
    c(1.5, 1.5),
    c(0, 1.5),
    c(0, 0)
  )))
  new_geom <- sf::st_sfc(p_new, crs = 4326L)

  new_geom_cast <- sf::st_cast(new_geom, 'MULTIPOLYGON')
  expect_equal(
    as.character(sf::st_geometry_type(new_geom_cast, by_geometry = TRUE)),
    'MULTIPOLYGON'
  )

  # Verify it can replace a MULTIPOLYGON geometry
  mp <- sf::st_multipolygon(list(
    sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0))))
  ))
  existing <- sf::st_sf(id = 'ZN_1', geometry = sf::st_sfc(mp, crs = 4326L))
  updated <- sf::st_set_geometry(existing, new_geom_cast)
  expect_equal(
    as.character(sf::st_geometry_type(updated, by_geometry = TRUE)),
    'MULTIPOLYGON'
  )
})
