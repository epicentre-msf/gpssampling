# styler: block

# Pre-load sf GDAL drivers to avoid segfault when testthat parallel
# workers call dyn.load() in forked processes on macOS CI (ARM64).
suppressMessages(sf::sf_extSoftVersion())
