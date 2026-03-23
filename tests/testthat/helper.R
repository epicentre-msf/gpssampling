
# Enable snapshot tests (not on CRAN)
if (!nzchar(Sys.getenv("NOT_CRAN"))) {
  Sys.setenv(NOT_CRAN = "true")
}

# Ensure terra can find proj.db via sf's bundled PROJ data
if (nzchar(system.file("proj", package = "sf"))) {
  proj_dir <- system.file("proj", package = "sf")
  if (file.exists(file.path(proj_dir, "proj.db")) &&
    !nzchar(Sys.getenv("PROJ_LIB"))) {
    Sys.setenv(PROJ_LIB = proj_dir)
  }
}

getDirTests <- function() {
  testthat::test_path()
}

# Read a package source file, works during both devtools::test() and R CMD check.
# For R/ files: read_pkg_file("R/utils.pkg.R")
# For DESCRIPTION: read_pkg_file("DESCRIPTION")
read_pkg_file <- function(file) {
  # 1. system.file (installed package)
  path <- system.file(file, package = "gpssampling")
  if (nzchar(path) && file.exists(path)) {
    return(readLines(path))
  }
  # 2. here::here (devtools::test context)
  path2 <- tryCatch(here::here(file), error = function(e) "")
  if (nzchar(path2) && file.exists(path2)) {
    return(readLines(path2))
  }
  # 3. Relative path fallback (devtools::test from tests/testthat/)
  rel <- file.path("../..", file)
  if (file.exists(rel)) {
    return(readLines(rel))
  }
  testthat::skip(sprintf("Cannot locate package file: %s", file))
}

save_png <- function(code, width = 400L, height = 400L) {
  path <- fs::file_temp(ext = "png")
  png(path, width = width, height = height)
  on.exit(dev.off())
  code
  path
}
