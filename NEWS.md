# gpssampling 0.2.0

## Breaking changes

* Renamed package from `epi.geosampler` to `gpssampling`.
* Renamed `Application` class to `GpsSampler`. Added `sampler()` factory
  function. `Application` and `ApplicationModule` kept as deprecated aliases.
* Removed 4 private GitHub Remotes (`leafem`, `epi.icons`, `leafpm`,
  `rhandsontable`). Replaced with internal JS bridge implementations.
* Removed Selenium test infrastructure. Switched to `shinytest2`.
* Renamed `epi.*` options to `gpssampling.*`:
  `epi.log` -> `gpssampling.trace`, `epi.user.default.name` ->
  `gpssampling.default_user`, `epi.h100` -> `gpssampling.h100`.

## Bug fixes

* Fixed HTTP 500 on app startup: `useGoogle()` no longer calls `stop()` when
  `MAPS_API_KEY_GOOGLE` is unset. Returns empty tag with a warning instead.
* Fixed `makeValidPolygons(NULL)` crash during `UserData` initialization
  (regression from `PolygonManager` extraction — init order bug).
* Fixed `units::set_units(area, km^2L)` parse error — changed to string form
  `"km^2"`.
* Fixed `makeValidPolygons()` on empty sf objects (zero-row `st_area()` +
  `round()` arithmetic failure). Added early return with typed columns.
* Fixed `self$load()` calling reactive bindings outside reactive context during
  `UserData$new()`. Wrapped in `shiny::isolate()`.
* Fixed `options(shiny.error = browser)` crashing non-interactive sessions.
  Guarded with `if (interactive())`.
* Fixed `addRoofsOpenBuilding()`: removed dead `readSpatialLayer` call, fixed
  tile grid, WKT parsing, CRS, and `st_make_valid`.
* Fixed NULL guard on `roofs_sf` in `addRoofsGoogle`/`addRoofsOpenBuilding`.
* Fixed `findFreePort()` — replaced Windows-only `netstat -aofn` with
  cross-platform socket binding test.
* Patched 18 SQL injection sites with parameterized queries.
* Fixed multipolygon cut/edit bug.
* Moved `R6::finalize()` methods from public to private (R6 >= 2.4.0).

## Structured logging

* Rewrote `utils.logs.R`: removed 3 dead decorator functions (`decorate()`,
  `decorateR6()`, `decorator()`, `logDecorator()`), ~120 lines of dead code.
* Fixed inverted option logic in `Base$initialize()` — the decorator pattern
  was silently disabled because `if (getOption('epi.log', FALSE))` skipped
  decoration when the option was TRUE.
* Replaced blanket method decoration with opt-in tracing via
  `gpssampling.trace` option or per-instance `.trace` parameter.
* New `decorate_methods()` function supports selective tracing via
  `trace_methods` parameter (trace only named methods instead of all).
* Replaced `tictoc::tic()/toc()` timing with `proc.time()` — removed tictoc
  dependency from the logging path.
* Added configurable log level via `GPSSAMPLING_LOG_LEVEL` env var
  (default: `DEBUG`).
* Added optional JSON structured log format via `GPSSAMPLING_LOG_FORMAT=json`.
* Added optional file logging with rotation via `GPSSAMPLING_LOG_DIR` env var.
* Replaced 5 `print()` calls in `class.step.identify.R` with `logDebug()`.
* Replaced `print()`/`cat()` in `utils.sf.R` (verbose layer output) with
  `logDebug()`.
* Replaced `print()` in `utils.pkg.R` (tile progress) with `logDebug()`.
* Upgraded `console.out()` to delegate to `logDebug()` instead of being a
  tictoc-only no-op.
* Upgraded session start/end log calls from `logDebug()` to `logInfo()`.
* Logger is re-initialized in `.onLoad()` to pick up env var changes.

## New features

* Basemap selector now filters options by available API keys. Only providers
  with configured keys appear in the dropdown.
* Default basemap changed from `sat.google` (requires key) to `sat.esri`
  (free).
* Maxar/DigitalGlobe connection ID moved from hardcoded URL to internal
  package constant (`DEFAULT_MAXAR_CONNECT_ID`), overridable via
  `MAPS_API_KEY_MAXAR` environment variable.
* Added 71 `importFrom(shiny, ...)` directives to NAMESPACE (were entirely
  missing). Excluded 7 functions the package intentionally overrides.
* `leafpm-bridge.js` now uses deferred registration to avoid
  `LeafletWidget is not defined` errors from script load ordering.

## Refactoring

* Extracted 5 classes from the `UserData` god object (3,468 -> 965 lines,
  72% reduction): `DocumentExporter`, `TileManager`, `SampleManager`,
  `PolygonManager`, `DataPersistence`.
* Split `utils.R` (2,619 lines) into 4 modules: `utils.error.R`,
  `utils.paths.R`, `utils.data.R`, `utils.string.R`.
* Split `utils.shiny.R` (3,693 lines) into 4 modules: `utils.shiny.inputs.R`,
  `utils.shiny.theming.R`, `utils.shiny.ports.R`, `utils.shiny.launcher.R`.
* Created `config.R` with 15 named constants replacing magic numbers.
* Moved `devEMF` and `sessioninfo` from Imports to Suggests with
  `requireNamespace()` guards.
* Optimized `addRoofsGoogle()` with `curl::multi_download()` batch downloads.
* Cleaned 14 dead code blocks, translated French comments, replaced roxygen
  and DESCRIPTION placeholders.
* Fixed all roxygen2 documentation warnings (zero warnings on
  `devtools::document()`).

## DevOps

* Added GitHub Actions workflow for R-CMD-check (Ubuntu, macOS, Windows)
  on push to `dev` and PRs.
* Added GitHub Actions release workflow on push to `main` (auto-documents,
  creates GitHub Release from DESCRIPTION version).
* Added branch protection on `main` (requires CI pass, no force push).
* Created `dev` branch for active development.
* Added `.Rbuildignore` for `.github/`, `.r-agent/`, `.local/`, etc.

## Tests

* 591 tests passing (up from ~390 at fork).
* Added shinytest2 test infrastructure with initial test suite.
* Rewrote `test-utils-shiny.R` and `test-logging.R` from scratch.
* Removed Selenium dependencies (`RSelenium`, `selenider`) from Suggests.

## Documentation

* Added installation instructions and API key setup guide to README.
* Added development workflow section to README.
* Comprehensive reports in `.local/`: architecture, bottlenecks, Shiny app
  fix, satellite API dependencies, structured logging plan.

# gpssampling 0.1.190

* Initial fork from GeoSampler by Epicentre (MSF).
* Added `NEWS.md` to track changes.
