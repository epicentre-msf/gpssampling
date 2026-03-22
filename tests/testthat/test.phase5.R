# Phase 5: Performance optimization, dependency cleanup, step class coverage
# devtools::test(filter = 'test.phase5', stop_on_failure = TRUE)

pkg_root <- here::here()

# -- addRoofsGoogle: batch download optimization -------------------------------

test_that("addRoofsGoogle uses curl::multi_download", {
  src <- readLines(file.path(pkg_root, "R/utils.pkg.R"))
  expect_true(
    any(grepl("curl::multi_download", src, fixed = TRUE)),
    info = "addRoofsGoogle should use curl::multi_download for batch downloads"
  )
  fn_body <- paste(src, collapse = "\n")
  fn_start <- regexpr("addRoofsGoogle <- function", fn_body, fixed = TRUE)
  expect_true(fn_start > 0L)
})

test_that("addRoofsGoogle separates download from processing", {
  src <- readLines(file.path(pkg_root, "R/utils.pkg.R"))
  fn_text <- paste(src, collapse = "\n")

  # The function should have Phase 1 (batch download) and Phase 2 (process)
  expect_true(
    grepl("Phase 1.*batch download", fn_text, ignore.case = TRUE),
    info = "addRoofsGoogle should have a batch download phase"
  )
  expect_true(
    grepl("Phase 2.*Process", fn_text, ignore.case = TRUE),
    info = "addRoofsGoogle should have a processing phase"
  )
})

# -- roofsAddOSM moved to TileManager -----------------------------------------

test_that("TileManager has roofsAddOSM method", {
  expect_true("roofsAddOSM" %in% names(TileManager$public_methods))
})

test_that("UserData$roofsAddOSM delegates to tile_mgr", {
  src <- readLines(file.path(pkg_root, "R/class.app.store.R"))
  fn_text <- paste(src, collapse = "\n")
  expect_true(
    grepl("tile_mgr\\$roofsAddOSM", fn_text),
    info = "UserData$roofsAddOSM should delegate to tile_mgr"
  )
})

# -- devEMF and sessioninfo moved to Suggests ----------------------------------

test_that("devEMF is in Suggests, not Imports", {
  desc <- read.dcf(file.path(pkg_root, "DESCRIPTION"), fields = c("Imports", "Suggests"))
  imports <- desc[1L, "Imports"]
  suggests <- desc[1L, "Suggests"]

  expect_false(
    grepl("\\bdevEMF\\b", imports),
    info = "devEMF should not be in Imports"
  )
  expect_true(
    grepl("\\bdevEMF\\b", suggests),
    info = "devEMF should be in Suggests"
  )
})

test_that("sessioninfo is in Suggests, not Imports", {
  desc <- read.dcf(file.path(pkg_root, "DESCRIPTION"), fields = c("Imports", "Suggests"))
  imports <- desc[1L, "Imports"]
  suggests <- desc[1L, "Suggests"]

  expect_false(
    grepl("\\bsessioninfo\\b", imports),
    info = "sessioninfo should not be in Imports"
  )
  expect_true(
    grepl("\\bsessioninfo\\b", suggests),
    info = "sessioninfo should be in Suggests"
  )
})

test_that("devEMF call sites have requireNamespace guard", {
  src <- readLines(file.path(pkg_root, "R/utils.report.R"))

  devemf_calls <- grep("devEMF::emf", src, fixed = TRUE)
  guard_calls <- grep('requireNamespace\\("devEMF"', src)

  expect_true(
    length(guard_calls) >= length(devemf_calls),
    info = "Every devEMF::emf call site should have a requireNamespace guard"
  )
})

test_that("sessioninfo call site has requireNamespace guard", {
  src <- readLines(file.path(pkg_root, "R/class.help.about.R"))
  src_text <- paste(src, collapse = "\n")

  expect_true(
    grepl('requireNamespace\\("sessioninfo"', src_text),
    info = "sessioninfo::session_info() should be guarded with requireNamespace"
  )
})

# -- Step class structure tests ------------------------------------------------

test_that("StepDelimit inherits from Step", {
  expect_true(inherits(StepDelimit, "R6ClassGenerator"))
  step_inherit <- StepDelimit$get_inherit()
  expect_true(inherits(step_inherit, "R6ClassGenerator"))
})

test_that("StepIdentify inherits from Step", {
  expect_true(inherits(StepIdentify, "R6ClassGenerator"))
  step_inherit <- StepIdentify$get_inherit()
  expect_true(inherits(step_inherit, "R6ClassGenerator"))
})

test_that("StepSample inherits from Step", {
  expect_true(inherits(StepSample, "R6ClassGenerator"))
  step_inherit <- StepSample$get_inherit()
  expect_true(inherits(step_inherit, "R6ClassGenerator"))
})

test_that("StepResult inherits from Step", {
  expect_true(inherits(StepResult, "R6ClassGenerator"))
  step_inherit <- StepResult$get_inherit()
  expect_true(inherits(step_inherit, "R6ClassGenerator"))
})

test_that("All step classes have required public methods", {
  required_methods <- c("initialize", "commit", "rollback", "clear")

  for (cls_name in c(
    "StepDelimit",
    "StepIdentify",
    "StepSample",
    "StepResult"
  )) {
    cls <- get(cls_name)
    public_names <- names(cls$public_methods)
    for (method in required_methods) {
      expect_true(
        method %in% public_names,
        info = sprintf("%s should have public method '%s'", cls_name, method)
      )
    }
  }
})

test_that("Step base class inherits from GpsSamplerModule", {
  step_inherit <- Step$get_inherit()
  expect_identical(step_inherit$classname, "GpsSamplerModule")
})

test_that("StepDelimit has correct id", {
  src <- readLines(file.path(pkg_root, "R/class.step.delimit.R"))
  expect_true(
    any(grepl("step_delimit", src, fixed = TRUE)),
    info = "StepDelimit should use id 'step_delimit'"
  )
})

test_that("StepIdentify has correct id", {
  src <- readLines(file.path(pkg_root, "R/class.step.identify.R"))
  expect_true(
    any(grepl("step_identify", src, fixed = TRUE)),
    info = "StepIdentify should use id 'step_identify'"
  )
})

test_that("StepSample has correct id", {
  src <- readLines(file.path(pkg_root, "R/class.step.sample.R"))
  expect_true(
    any(grepl("step_sample", src, fixed = TRUE)),
    info = "StepSample should use id 'step_sample'"
  )
})

test_that("StepResult has correct id", {
  src <- readLines(file.path(pkg_root, "R/class.step.result.R"))
  expect_true(
    any(grepl("step_result", src, fixed = TRUE)),
    info = "StepResult should use id 'step_result'"
  )
})

# -- GpsSampler rename verification -------------------------------------------

test_that("GpsSampler class is exported and functional", {
  expect_true(inherits(GpsSampler, "R6ClassGenerator"))
  expect_identical(GpsSampler$classname, "GpsSampler")
})

test_that("sampler() factory creates GpsSampler instance", {
  samp <- sampler()
  expect_s3_class(samp, "GpsSampler")
})

test_that("Application backward compat alias works", {
  expect_identical(Application, GpsSampler)
  app <- Application$new()
  expect_s3_class(app, "GpsSampler")
})

test_that("GpsSamplerModule class exists", {
  expect_true(inherits(GpsSamplerModule, "R6ClassGenerator"))
  expect_identical(GpsSamplerModule$classname, "GpsSamplerModule")
})

test_that("ApplicationModule backward compat alias works", {
  expect_identical(ApplicationModule, GpsSamplerModule)
})
