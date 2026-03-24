# styler: block

# Disable parallel testthat on macOS: forked processes + dyn.load()
# of GDAL native libraries causes segfaults on macOS ARM64 CI runners.
if (Sys.info()[["sysname"]] == "Darwin") {
  options(testthat.parallel = FALSE) # nolint
}
