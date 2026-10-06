# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

`rNDC.Shiny` (Apache-2.0, maintained for LTER-LIFE; the GitHub repository is `LTER-LIFE/rNDC-Shiny`) is an R package with a Shiny GUI to LTER-LIFE's **NatureDataCube**. It is built on the [`rNDC`](https://github.com/LTER-LIFE/rNDC) package, currently from its `texel26` branch (`Remotes:` in `DESCRIPTION`): prefer exported `rNDC::` functions over re-implementing requests here, and put generic helpers that other rNDC users would want in rNDC itself. Unqualified calls to imported packages must be listed as imports in `R/rNDC.Shiny-package.R` (`@import shiny`, specific `@importFrom` for the rest, to avoid masking); otherwise use `pkg::fn`. `DESCRIPTION` `Imports` lists the packages.

## Commands

Run R through the conda env: `conda run -n base_r Rscript -e '...'`. It has all dependencies, `devtools`, `roxygen2` and `webmockr`, and `rNDC` installed from `LTER-LIFE/rNDC@texel26`. Credentials for live runs are in `~/.Renviron`.

- Load during development: `devtools::load_all()`. Run the app: `NDC_TOKEN=... conda run -n base_r Rscript -e 'devtools::load_all(); ndc_gui(port = 3838, launch.browser = FALSE)'` (only `NDC_TOKEN` is required; dummy values are enough to check that the app starts and serves, but data requests then fail).
- Run all tests: `devtools::test()`; a single file: `devtools::test(filter = "retrieval")` (matches `tests/testthat/test-<filter>.R`). The tests are offline: HTTP is stubbed at transport level with `webmockr` (httr adapter; rstac and httr both go through httr). `tests/testthat/helper-fixtures.R` has `local_stac_api()` (stub STAC API at `https://example.org/api/`, with result pages and an optional error status), `adc_re()` + `geojson_features()` for AgroDataCube stubs, `request_uris()` / `last_request_body()` to inspect the requests, and `selected_polygon()`.
- Regenerate docs and `NAMESPACE`: `roxygen2::roxygenise()`. `NAMESPACE` and `man/` are **generated**; never edit them by hand. Imports are declared as tags in `R/rNDC.Shiny-package.R`, and every exported function needs `@export` plus a roxygen block.
- Full check: `R CMD build .` in a scratch directory, then `R CMD check --no-manual rNDC.Shiny_*.tar.gz`. It must stay free of errors, warnings and notes (CI uses `error-on: "warning"`).
- Live API tests (`tests/testthat/test-live.R`) are skipped unless `RNDC_LIVE_TESTS=true` (plus both tokens): `RNDC_LIVE_TESTS=true conda run -n base_r Rscript -e 'devtools::test(filter = "live")'`.
- Update rNDC to the latest `texel26`: `conda run -n base_r Rscript -e 'remotes::install_github("LTER-LIFE/rNDC", ref = "texel26", upgrade = "never")'`. Check `packageDescription("rNDC")$RemoteSha` when behaviour differs from what the rNDC source says.
- Container: `docker compose up --build` (or `podman compose`), reading `.env` (copy `.env.example`). The image installs rNDC from GitHub (`RNDC_REF`, default `texel26`), the other dependencies from `DESCRIPTION` (`remotes::install_deps()`, incl. `Remotes:`), then this package, and runs `rNDC.Shiny::ndc_gui(host = '0.0.0.0', port = 3838)`; the app is served at `http://localhost:3838/` (`SHINY_APP_BASE_URL` does not create a route, it only sets Shiny's `appBaseUrl` behind a reverse proxy). Docker is not available in the dev environment: the image is built by the `docker-build` workflow only.
- CI (`.github/workflows/`): `R-CMD-check.yaml` runs the offline check on push/PR; `live-checks.yaml` runs the live tests weekly (default branch) or on manual dispatch, using the `NDC_TOKEN`/`ADC_TOKEN` repository secrets; `docker-build.yaml` builds the image (no push) when the Docker files or the package change. Repo: https://github.com/LTER-LIFE/rNDC-Shiny.
- Credentials come from env vars: `NDC_TOKEN` (NatureDataCube STAC API, also the default `token` of `rNDC::ndc_get()`, land use and nitrogen functions; **required**) and `ADC_TOKEN` (AgroDataCube: Fields, AHN, Soil map, Weather; **optional**, those datasets are disabled without it). Never write tokens to the repo or to outputs.

## Architecture

Code lives in flat `R/*.R` files:

1. **Launchers** (`ndc_gui.R`, `ndc_app.R`, the only exports): `ndc_gui()` runs `shiny::runApp(ndc_app(), ...)` and returns what `stopApp()` is given (the "Return data to R" button, shown only when `interactive()`); `ndc_app()` registers the `ndc-www` resource path (logo, from `inst/app/www`) and returns `shinyApp(app_ui(), app_server)`. `ndc_setup()` (internal, called by `ndc_app()`) stops without `NDC_TOKEN`, warns without `ADC_TOKEN` and sets `shiny.appBaseUrl` from `SHINY_APP_BASE_URL`. `inst/app/app.R` is a one-line entry point (`rNDC.Shiny::ndc_app()`) for Shiny Server.
2. **UI and server** (`app_ui.R`, `app_server.R`): `app_ui()` returns the page (CSS/JS inline); `app_server()` holds all reactive logic. The `overview` reactiveVal is the "shopping cart" (one row per dataset x polygon x year/dates, with the polygon `sf` in a list column). `retrieve_and_save()` (inside the server) loops over the rows with `retrieve_row()` (see 3), collects the data, the `download_summary.csv` manifest and the messages, writes the manifest into a unique temp folder, zips it with `zip_export()` and removes the folder. It backs both "Download" (zip) and "Return data to R" (`stopApp(list(datasets, overview, messages, summary, ...))`); per-dataset messages ("Retrieved: ...", "Failed: ... - reason") are shown in the messages panel.
3. **Retrieval** (`retrieval.R`, no Shiny dependencies): `retrieve_row(row, index, workdir, ndc_token, adc_token)` handles one overview row. `retrieve_dataset()` dispatches to one function per dataset (`retrieve_agricultural_fields()`, `retrieve_weather()`, `retrieve_nitrogen()`, `retrieve_ndvi_rasters()`, ...), each returning an *outcome*: `outcome_data()` (the data, its name, file extension, messages and optional writer) or `outcome_none()` (message, file type and status for the summary). `retrieve_row()` writes the file (with a unique name, see Gotchas), builds the summary rows (`manifest_row()`, once per polygon the reference polygon via `export_reference_polygon()`) and turns any error into a "Failed: ..." row. To add a dataset: write a `retrieve_<name>()` and add it to `retrieve_dataset()`.
4. **Constants** (`constants.R`): dataset menu and tab layout, STAC collection IDs, SNL settings, `adc_datasets`, and the rNDC internals taken lazily with `delayedAssign()` (`landuse_default_year`, `nitrogen_layer_choices`; `rNDC:::`, because rNDC does not export them).
5. **STAC/ADC helpers** (`stac_helpers.R`): `ndc_get_all_sf()` (all result pages as `sf`, via `rNDC::ndc_get(mode = "fetch")`), `adc_get_all()` (all pages of AgroDataCube Fields/Soiltypes), `get_nitrogen_years()` (from the STAC items) and the process-level cache (`cache_get()`, `cache_set()`, TTL `ndc_cache_ttl`).
6. **Project layers** (`projects.R`): the `lter` collection is fetched once per process and classified by `name` into 4 projects (`classify_lter`, `get_lter_data`); `snl` (~264k parcels) is fetched per map viewport (`fetch_snl_bbox`, zoom-gated, capped by `snl_fetch_limit`). NDVI statistics come from `ndvi-lter` / `ndvi-snl` (`fetch_ndvi_stats_monthly`, chosen by `detect_ndvi_collection` from the polygon's source name, e.g. `Nestboxes_3`, `SNL parcel_12`, and filtered on the polygon's `ndc_id`).
7. **Polygons** (`polygons.R`): WKT columns, sequential source names, conversion of drawn/uploaded geometries (zip/gpkg/shp/geojson/kml); everything is kept in EPSG:4326 with a `wkt` column used by the retrieval code.

Dataset -> rNDC function:

| Dataset | Function |
|---|---|
| Agricultural fields, Soil map, AHN | `adc_url()` + `adc_get()` (AgroDataCube REST) |
| Weather | `get_closest_meteostation()`, `get_meteo_for_date()`, `get_meteo_for_long_period()` |
| NDVI (Geodata) | `download_avg_ndvi_month()`, `download_avg_ndvi_stack()` (GroenMonitor WCS) |
| NDVI (Statistics) | `ndc_get()` on `ndvi-lter` / `ndvi-snl` |
| Land Use | `get_landuse_raster()` (collection `lgn`) |
| Nitrogen | `get_nitrogen_raster()` (collections `ntot`, `nox`, `nh3`) |

The rNDC endpoint is `rNDC::ndc_endpoint()` (default: the test server `ndc-test.containers.wur.nl`; override with `options(rNDC.endpoint = ...)`).

Tests (`tests/testthat/`): `test-helpers.R` (pure helpers), `test-stac_helpers.R` and `test-projects.R` (stubbed HTTP), `test-retrieval-functions.R` (the functions of `retrieval.R`, directly), `test-retrieval.R` and `test-app.R` (server logic with `shiny::testServer(app_server, ...)`; rNDC functions that download files are replaced with `local_mocked_bindings(..., .package = "rNDC")`), `test-live.R` (opt-in).

## Gotchas

- The rNDC raster functions (`get_landuse_raster`, `get_nitrogen_raster`) **stop with an error** when nothing matches instead of returning `NULL`; the real message arrives via the generic error handler of `retrieve_and_save()`. Their results are lists: use `$stack`.
- `rNDC::ndc_get(mode = "sf")` converts only the first page of results (and warns if incomplete); use `ndc_get_all_sf()` when all items are needed. AgroDataCube Fields/Soiltypes are paged (default 50 features, `page_offset` is the page number): use `adc_get_all()`.
- NDVI statistics queries return all NDVI features that intersect the polygon (neighbouring SNL parcels too); the results are filtered on the polygon's `ndc_id`. Drawn and uploaded polygons have none (and no NDVI statistics).
- `rNDC::ndc_trange()` formats a date as midnight UTC; the app extends the end of an NDVI period to 23:59:59.
- `rNDC::get_closest_meteostation()` returns `closest_id = character(0)` (not `NULL`) when no station id is found. Weather observations have `"geometry": null` in the API response.
- One R process serves all Shiny sessions: avoid `setwd()` and shared fixed paths in `tempdir()`; the cache and `Sys.getenv()` tokens are process-wide. `app_server()` reads the tokens when a session starts.
- In `testServer()` tests, set the inputs (the first `session$setInputs()` flushes the initial observers, which reset the polygon selection) **before** setting `selected_polygons()`, and call `session$flushReact()` after setting a reactiveVal.
- `months()` on a number needs lubridate's method: the `lubridate` import in `R/rNDC.Shiny-package.R` makes sure it is registered.
- R code must be ASCII (non-ASCII is a warning in `R CMD check`): use `\u` escapes in strings. Files use LF line endings, as in rNDC.
- Column names used in dplyr/leaflet formulas are declared with `utils::globalVariables()` in `R/rNDC.Shiny-package.R`.
- "Vegetation structure" and "Ground water table" are shown in the menu as disabled "coming soon" entries.
- Output file names are built from dataset, view and polygon only, so `unique_export_file()` appends the period (`..._2023`), or a number, to a name that is already taken in the download folder (the first row keeps the plain name). `download_summary.csv` links each file to its period. The reference polygon (`<polygon>.gpkg`) is written once per polygon: `reference_polygon_file()` skips it when the file holds the same polygon, and numbers it (`<polygon>_2.gpkg`) when another polygon has the same name.
- `CITATION.cff` lists the authors and version; update it together with `DESCRIPTION` when releasing.
