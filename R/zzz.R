.conflicts.OK <- function() {
  NULL
}

.onAttach <- function(libname, pkgname) {
  if (!interactive()) {
    return()
  }

  # if (.Platform$OS.type == 'windows') {
  #   if (interactive()) {
  #     packageStartupMessage('Registering Windows fonts with R')
  #   }
  #   windowsFonts <- grDevices::windowsFonts
  # }

  packageStartupMessage(sprintf('%s (v%s)', getPackageName(), getPackageVersion()))
}

.onLoad <- function(libname, pkgname) {
  op <- list(
    epi.log = TRUE,
    epi.user.default.name = 'default'
  )

  toset <- !(names(op) %in% names(options()))

  if (any(toset)) options(op[toset])

  # suppress scientific notation
  options(scipen = 999L)
  options(stringsAsFactors = FALSE)
  options(digits = 12L)
  options(digits.secs = 4L)

  set.seed(1025L)

  # rlang::env_unlock(env = asNamespace('terra'))
  # rlang::env_binding_unlock(env = asNamespace('terra'))
  # assign('new_rast', new_rast, envir = asNamespace('terra'))
  # rlang::env_binding_lock(env = asNamespace('terra'))
  # rlang::env_lock(asNamespace('terra'))
}

.onUnload <- function(libpath) {
}

# styler: block

# R CMD check complains about . in magrittr pipelines
if (getRversion() >= '2.15.1') {
  # bindings for global variables
  utils::globalVariables(c(
    '.globals', 'pt',
    'x', 'xmin', 'xmax',
    'y', 'ymin', 'ymax'
  ))
}
if (getRversion() >= '3.1.0') {
  utils::suppressForeignCheck('localvariable')
}
