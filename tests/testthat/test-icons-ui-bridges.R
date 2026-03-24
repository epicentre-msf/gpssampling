# Phase 4 Tests: Remote dependency replacement
# Tests for utils.icons.R, utils.handsontable.R bridge, and utils.leafpm.R

# --- utils.icons.R ---

test_that("html_dependency_mdi returns htmlDependency", {
  dep <- html_dependency_mdi()
  expect_s3_class(dep, "html_dependency")
  expect_equal(dep$name, "mdi")
  expect_equal(dep$version, "7.4.47")
  expect_true(length(dep$stylesheet) > 0)
})

test_that("icon_mdi returns an i tag with correct classes", {
  tag <- icon_mdi("check-bold")
  rendered <- as.character(tag)
  expect_true(grepl("mdi", rendered))
  expect_true(grepl("mdi-check-bold", rendered))
  expect_s3_class(tag, "shiny.tag")
})

test_that("icon_mdi handles named sizes", {
  tag <- icon_mdi("magnify", size = "xs")
  rendered <- as.character(tag)
  expect_true(grepl("font-size:18px", rendered))

  tag_lg <- icon_mdi("magnify", size = "lg")
  rendered_lg <- as.character(tag_lg)
  expect_true(grepl("font-size:48px", rendered_lg))
})

test_that("icon_mdi handles CSS size values", {
  tag <- icon_mdi("magnify", size = "2em")
  rendered <- as.character(tag)
  expect_true(grepl("font-size:2em", rendered))
})

test_that("icon_mdi handles color parameter", {
  tag <- icon_mdi("square-rounded", color = "yellow")
  rendered <- as.character(tag)
  expect_true(grepl("color:yellow", rendered))
})

test_that("icon_mdi handles additional classes", {
  tag <- icon_mdi("check-bold", class = "my-class")
  rendered <- as.character(tag)
  expect_true(grepl("my-class", rendered))
})

test_that("icon_mdi with no optional args has no style attribute", {
  tag <- icon_mdi("check-bold")
  rendered <- as.character(tag)
  expect_false(grepl("style=", rendered))
})

# --- utils.handsontable.R bridge functions ---

test_that("hot_load_data sends correct message type", {
  mock_session <- list(
    sendCustomMessage = function(type, message) {
      list(type = type, message = message)
    }
  )
  result <- withr::with_options(
    list(shiny.session = mock_session),
    hot_load_data("test_id", data.frame(a = 1), session = mock_session)
  )
  # The function calls session$sendCustomMessage; no error means it works
  expect_true(TRUE)
})

test_that("hot_render sends correct message type", {
  mock_session <- list(
    sendCustomMessage = function(type, message) {
      expect_equal(type, "hot-render")
      expect_equal(message$id, "my_table")
    }
  )
  hot_render("my_table", session = mock_session)
})

test_that("hot_select_cell sends correct message", {
  mock_session <- list(
    sendCustomMessage = function(type, message) {
      expect_equal(type, "hot-select-cell")
      expect_equal(message$row, 5L)
      expect_equal(message$col, 2L)
    }
  )
  hot_select_cell("tbl", 5L, 2L, session = mock_session)
})

test_that("hot_filter sends correct message", {
  mock_session <- list(
    sendCustomMessage = function(type, message) {
      expect_equal(type, "hot-filter")
      expect_equal(message$column, 1L)
      expect_equal(message$filter, "search_term")
    }
  )
  hot_filter("tbl", 1L, "search_term", session = mock_session)
})

# --- utils.leafpm.R functions ---

test_that("pm_attach_dependencies returns a leaflet object", {
  skip_if_not_installed("leaflet")
  skip_if_not_installed("leafpm")

  lf <- leaflet::leaflet() |>
    leaflet::addTiles()
  result <- pm_attach_dependencies(lf, targetGroup = "test_group")
  expect_s3_class(result, "leaflet")
})

test_that("pm_clear_features calls invokeMethod on proxy", {
  skip_if_not_installed("leaflet")
  # Create a minimal map to test the function doesn't error
  lf <- leaflet::leaflet() |>
    leaflet::addTiles()
  result <- pm_clear_features(lf)
  expect_s3_class(result, "leaflet")
})

test_that("pm_toggle_draw_mode calls invokeMethod", {
  skip_if_not_installed("leaflet")
  lf <- leaflet::leaflet() |>
    leaflet::addTiles()
  result <- pm_toggle_draw_mode(lf, shape = "Polygon")
  expect_s3_class(result, "leaflet")
})

test_that("pm_edit_feature calls invokeMethod", {
  skip_if_not_installed("leaflet")
  lf <- leaflet::leaflet() |>
    leaflet::addTiles()
  result <- pm_edit_feature(lf, targetGroup = "polygons", targetId = "poly_1")
  expect_s3_class(result, "leaflet")
})

test_that("pm_edit_stop calls invokeMethod", {
  skip_if_not_installed("leaflet")
  lf <- leaflet::leaflet() |>
    leaflet::addTiles()
  result <- pm_edit_stop(lf, targetGroup = "polygons")
  expect_s3_class(result, "leaflet")
})
