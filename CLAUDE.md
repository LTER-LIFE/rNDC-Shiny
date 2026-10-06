# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

`rNDC-Shiny` (Apache-2.0, maintained for LTER-LIFE) is a Shiny GUI to LTER-LIFE's **NatureDataCube**. It is a single app (`inst/shiny/naturedatacube_app/app.R`) with **no R package code of its own**: all data access goes through the [`rNDC`](https://github.com/LTER-LIFE/rNDC) R package, currently from its `texel26` branch (`Remotes:` in `DESCRIPTION`). Prefer exported `rNDC::` functions over re-implementing requests in the app; do not add R functions to this repo's own package namespace (there is none, and no `NAMESPACE`/`R/`). The app loads its dependencies with `library()`.

## Commands

Run R through the conda env: `conda run -n base_r Rscript -e '...'`. It has shiny, leaflet, sf, terra, rstac, testthat, devtools and `rNDC` (installed from `LTER-LIFE/rNDC@texel26`) available.

- Run the app: `NDC_TOKEN=... ADC_TOKEN=... conda run -n base_r Rscript -e 'shiny::runApp("inst/shiny/naturedatacube_app", port = 3838)'`. Both tokens are required at startup (the app `stop()`s otherwise); dummy values are enough to check that the app starts and serves, but data requests then fail. `runApp()` sets the working directory to the app directory. For use from an interactive session, `data <- shiny::runApp(...)` returns what `stopApp()` is given ("Return data to R" button).
- Syntax check without running: `conda run -n base_r Rscript -e 'invisible(parse("inst/shiny/naturedatacube_app/app.R"))'`.
- Update rNDC to the latest `texel26`: `conda run -n base_r Rscript -e 'remotes::install_github("LTER-LIFE/rNDC", ref = "texel26", upgrade = "never")'`. Check `packageDescription("rNDC")$RemoteSha` when behaviour differs from what the rNDC source says.
- Container: `docker compose up --build` (or `podman compose`), reading `.env` (copy `.env.example`). The image installs rNDC from GitHub (`--build-arg RNDC_REF=<ref>` to change the branch) and runs the app directly; the app is at `http://localhost:3838/<SHINY_APP_BASE_URL>`.
- There is no test suite yet (`tests/tutorial.R` is a user tutorial, not a test, and is partly stale). If tests are added, use `testthat` and stub HTTP like rNDC does (`webmockr`, httr adapter).
- Credentials come from env vars: `NDC_TOKEN` (NatureDataCube STAC API; also the default `token` of the `rNDC::ndc_get()`, land use and nitrogen functions) and `ADC_TOKEN` (AgroDataCube: Fields, AHN, Soil map, Weather). Never write tokens to the repo or to outputs.

## Architecture

All in `inst/shiny/naturedatacube_app/app.R` (flat file, top to bottom):

1. **Config and helpers**: dataset menu (`available_datasets`), tab layout per dataset, tokens, and constants taken from rNDC internals (`landuse_default_year`, `nitrogen_layer_choices` via `rNDC:::`, because rNDC does not export them; `nitrogen_year_choices` is hardcoded).
2. **Project layers from the STAC API** via `rNDC::ndc_get()`: the `lter` collection is fetched once per session and classified by `name` into 4 projects (`classify_lter`); `snl` (~264k parcels) is fetched per map viewport (`fetch_snl_bbox`, zoom-gated, capped by `snl_fetch_limit`). NDVI statistics come from the `ndvi-lter` / `ndvi-snl` collections (`fetch_ndvi_stats_monthly`, chosen by `detect_ndvi_collection` from the polygon's source name, e.g. `Nestboxes_3`, `SNL parcel_12`).
3. **Polygon handling**: selection by clicking fixed project polygons, drawing, or uploading (zip/gpkg/shp/geojson/kml); everything is kept in EPSG:4326 with a `wkt` column used by the retrieval code.
4. **UI and server**: `overview` reactiveVal is the "shopping cart" (one row per dataset x polygon x year/dates, with the polygon `sf` in a list column). `retrieve_and_save()` loops over the rows, calls the rNDC function for each dataset, writes files (csv for the Statistics view, gpkg/tif otherwise) plus a `download_summary.csv` manifest, and zips them. The same function backs "Download" (zip) and "Return data to R" (`stopApp(list(datasets, overview, messages, summary, ...))`).

Dataset -> rNDC function:

| Dataset | Function |
|---|---|
| Agricultural fields, AHN, Soil map | `adc_url()` + `adc_get()` (AgroDataCube REST) |
| Weather | `get_closest_meteostation()`, `get_meteo_for_date()`, `get_meteo_for_long_period()` |
| NDVI (Geodata) | `download_avg_ndvi_month()`, `download_avg_ndvi_stack()` (GroenMonitor WCS) |
| NDVI (Statistics) | `ndc_get()` on `ndvi-lter` / `ndvi-snl` |
| Land Use | `get_landuse_raster()` (collection `lgn`) |
| Nitrogen | `get_nitrogen_raster()` (collections `ntot`, `nox`, `nh3`) |

The rNDC endpoint is `rNDC::ndc_endpoint()` (default: the test server `ndc-test.containers.wur.nl`; override with `options(rNDC.endpoint = ...)`).

## Gotchas

- The rNDC raster functions (`get_landuse_raster`, `get_nitrogen_raster`) **stop with an error** when nothing matches instead of returning `NULL`; the app's "no raster returned" branches are only reached for empty stacks, and the real message arrives via the generic error handler.
- `rNDC::ndc_get(mode = "sf")` converts only the first page of results (and warns if incomplete); use `mode = "fetch"` + `rstac::items_as_sf()` when all items are needed.
- `rNDC::ndc_trange()` formats a date as midnight UTC, so an end date excludes that day's observations unless one day is added.
- `rNDC::get_closest_meteostation()` returns `closest_id = character(0)` (not `NULL`) when no station id is found.
- One R process serves all Shiny sessions: avoid `setwd()` and shared fixed paths in `tempdir()`; `NDC_TOKEN`/`ADC_TOKEN` are process-wide globals.
- Several datasets are listed in the menu but not wired ("Vegetation structure", "Ground water table"); they end up as "Skipped" in the summary.
- `DESCRIPTION` only documents dependencies (`Imports`, `Remotes`); the repo is not installed as a package, and the Docker image copies `inst/shiny/naturedatacube_app` to `/app`.
