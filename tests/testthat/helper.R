
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
  here::here("tests", "testthat")
}

save_png <- function(code, width = 400L, height = 400L) {
  path <- fs::file_temp(ext = "png")
  png(path, width = width, height = height)
  on.exit(dev.off())
  code
  path
}
