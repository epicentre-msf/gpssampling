.conflicts.OK <- function() {
  NULL
}

.onAttach <- function(libname, pkgname) {
  if (!interactive()) {
    return()
  }

  packageStartupMessage(sprintf(
    "%s (v%s)",
    getPackageName(),
    getPackageVersion()
  ))
}

.onLoad <- function(libname, pkgname) {
  op <- list(
    gpssampling.trace = FALSE,
    gpssampling.default_user = "default"
  )

  toset <- !(names(op) %in% names(options()))

  if (any(toset)) options(op[toset])

  # suppress scientific notation
  options(scipen = 999L)
  options(digits = 12L)
  options(digits.secs = 4L)

  # Re-initialize logger with env var settings (if changed after package load)
  .logger <<- init_logger()
}

.onUnload <- function(libpath) {
}

# styler: block

# R CMD check complains about . in magrittr pipelines
if (getRversion() >= "2.15.1") {
  # bindings for global variables
  utils::globalVariables(c(
    ".globals",
    "pt",
    "x",
    "xmin",
    "xmax",
    "y",
    "ymin",
    "ymax"
  ))
}
if (getRversion() >= "3.1.0") {
  utils::suppressForeignCheck("localvariable")
}
