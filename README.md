# rNDC.Shiny

[![R-CMD-check](https://github.com/LTER-LIFE/rNDC-Shiny/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/LTER-LIFE/rNDC-Shiny/actions/workflows/R-CMD-check.yaml)
[![live-checks](https://github.com/LTER-LIFE/rNDC-Shiny/actions/workflows/live-checks.yaml/badge.svg)](https://github.com/LTER-LIFE/rNDC-Shiny/actions/workflows/live-checks.yaml)
[![docker-build](https://github.com/LTER-LIFE/rNDC-Shiny/actions/workflows/docker-build.yaml/badge.svg)](https://github.com/LTER-LIFE/rNDC-Shiny/actions/workflows/docker-build.yaml)

This package provides a graphical user interface to *NatureDataCube*, through an R-Shiny app, using the functions and wrappers from the [`rNDC`](https://github.com/LTER-LIFE/rNDC) R package.

The idea of the *NatureDataCube* is to offer an accessible way for researchers/ecologists to retrieve relevant data.

*NatureDataCube* is a platform based on [*AgroDataCube*](https://agrodatacube.wur.nl/), holding and providing access to data used in the context of project [LTER-LIFE](https://lter-life.nl/en).

## Installation

```r
# install.packages("remotes")
remotes::install_github("LTER-LIFE/rNDC-Shiny")
```

This also installs [`rNDC`](https://github.com/LTER-LIFE/rNDC) and the other dependencies. The package needs R >= 4.1.

## Authentication

API tokens are read from environment variables:

| Variable | Needed for |
|---|---|
| `NDC_TOKEN` | **Required.** The *NatureDataCube* STAC API: project layers, NDVI statistics, Land Use and Nitrogen. |
| `ADC_TOKEN` | Optional. The *AgroDataCube* REST API: Weather, Soil map, AHN and Agricultural fields. Without it, these datasets are disabled in the app. |

For example, `Sys.setenv(NDC_TOKEN = "<your token>", ADC_TOKEN = "<your token>")`, or put them in your `.Renviron`.

To generate your free personal API token you can go to [API Access Registration](https://ndc.wur.nl/register).

## Opening the Shiny app

```r
library(rNDC.Shiny)
data_ndc <- ndc_gui()
```

Launching the app this way stores what you retrieve in an R object, so that you can continue working with it after closing the app: choose your datasets in the app, and click "Return data to R (close app)" (this button is only shown in interactive R sessions). Make sure your working directory is the folder you want to work from (`getwd()` shows the current one, `setwd()` changes it).

`ndc_gui()` returns a list with:

- `datasets`: the retrieved data, one element per row of the overview, named after the dataset and the row (e.g. `Weather_1`, `Nitrogen_2`, `NDVI_stats_3`); `sf` objects for vector data and tables, and `terra` rasters for the raster datasets,
- `overview`: the datasets, polygons and periods that were requested,
- `messages` and `summary`: what happened to each dataset (retrieved, failed and why).

The app can also be used to only download the data: "Download dataset(s)" gives a zip file with the data, the selected polygons and a `download_summary.csv`.

See [`examples/tutorial.R`](examples/tutorial.R) for a tutorial that combines bird nest data with weather data retrieved through the app.

## Datasets

| Dataset | Output | Retrieved with | Token |
|---|---|---|---|
| Weather (KNMI, closest station) | table | `rNDC::get_closest_meteostation()`, `get_meteo_for_date()`, `get_meteo_for_long_period()` | `ADC_TOKEN` |
| Agricultural fields, Soil map | `sf` | `rNDC::adc_url()`, `rNDC::adc_get()` (all pages of the results) | `ADC_TOKEN` |
| AHN | table | `rNDC::adc_url()`, `rNDC::adc_get()` | `ADC_TOKEN` |
| NDVI, Statistics | table (monthly means per polygon) | `rNDC::ndc_get()` on `ndvi-lter` / `ndvi-snl`; only for LTER and SNL project areas | `NDC_TOKEN` |
| NDVI, Geodata | raster (monthly means) | `rNDC::download_avg_ndvi_month()`, `download_avg_ndvi_stack()` | `NDC_TOKEN`\* |
| Land Use | raster | `rNDC::get_landuse_raster()` | `NDC_TOKEN` |
| Nitrogen | raster (`ntot`, `nox`, `nh3`) | `rNDC::get_nitrogen_raster()` | `NDC_TOKEN` |

\* The NDVI rasters come from GroenMonitor, which needs no token; the app itself needs `NDC_TOKEN` to load the project areas.

For Land Use and Nitrogen, the app tells you whether the selected area has data for the chosen year before you add the dataset.

### Long retrievals

The NDVI rasters and long Weather periods make many requests (one download per day for NDVI rasters, one request per 200 days for Weather). When you add a dataset, the app estimates how long the whole overview will take to retrieve (NDVI rasters: about a quarter of a second per day, so a year takes about a minute and a half). Above about a minute it warns you, and above a limit it refuses and asks for a shorter period. While retrieving, the progress bar shows where it is.

**Where the retrieval runs.** In an interactive R session (`ndc_gui()` from R), the app retrieves in your R session, and is busy until it is done; the limit is 5 minutes. In a deployment (a non-interactive session, such as the Docker image) the retrieval runs in a background R process, so that a long retrieval does not hold up the other users; the limit is then 15 minutes. When all background processes are busy the retrieval waits in a queue (the progress bar shows the position), and while it waits or runs a "Cancel retrieval" button stops it. Settings:

| Variable | Meaning |
|---|---|
| `NDC_ASYNC` | `true` or `false`: retrieve in background processes or not (default: yes when not interactive). |
| `NDC_WORKERS` | The number of background processes, i.e. retrievals that run at the same time; more wait in a queue (default: up to 4, depending on the cores). Each uses memory (a few hundred MB). |
| `NDC_MAX_QUEUE` | The number of retrievals that may wait for a free process (default 20); beyond that, new retrievals are refused with a message. |
| `NDC_MAX_REQUEST_SECONDS` | The longest retrieval that is accepted (the option `rNDC.Shiny.max_request_seconds` does the same). |

A session retrieves one overview at a time. In the background mode, the package must be installed (the processes load it), or loaded with `devtools::load_all()` (the processes then load it from its source folder, with `pkgload`). A user who leaves the page cancels the retrieval.

Uploads can be up to 100 MB in total (set `NDC_MAX_UPLOAD_MB` to change this; Shiny's own default is 5 MB).

Areas of interest can be one of the project areas (LTER projects, or SNL parcels), polygons you draw on the map, or polygons you upload (GeoPackage, shapefile, GeoJSON, KML, or a zip file with these).

## Running with Docker or Podman

The Shiny app can also be run as a container, which installs the package (`rNDC.Shiny`) and all its dependencies, including `rNDC`, automatically.

**Setup:**

Copy `.env.example` to `.env` and fill in your API tokens:

```         
NDC_TOKEN=your_token_here
ADC_TOKEN=your_agrodatacube_token_here
SHINY_APP_BASE_URL=/naturedatacube
```

`NDC_TOKEN` is required. `ADC_TOKEN` is optional: without it, the datasets from AgroDataCube (Weather, Soil map, AHN and Agricultural fields) are disabled. `SHINY_APP_BASE_URL` is only needed when a reverse proxy publishes the app under a path. The optional `NDC_ASYNC`, `NDC_WORKERS`, `NDC_MAX_REQUEST_SECONDS` and `NDC_MAX_UPLOAD_MB` are described in the sections above; in the container the retrievals run in background processes by default (`NDC_ASYNC=true`), up to 4 at the same time.

**Run:**

``` bash
# Docker
docker compose up --build

# Podman
podman compose up --build
```

The app will be available at `http://localhost:3838/`.

By default the image installs `rNDC` from its `texel26` branch. To use another branch or tag, set `RNDC_REF` (e.g. `RNDC_REF=main docker compose up --build`).

The build downloads `rNDC` and `leaflet.extras` from GitHub, which limits anonymous requests; if the build fails because of that, pass a GitHub token as a build secret (it is not kept in the image): `GITHUB_PAT=<token> docker build --secret id=github_pat,env=GITHUB_PAT -t rndc-shiny .`.

## Development

```r
devtools::load_all()    # load the package
ndc_gui()               # run the app from the working tree
devtools::test()        # offline tests: the HTTP requests are stubbed
roxygen2::roxygenise()  # regenerate NAMESPACE and man/ after changing roxygen comments
```

- The offline tests need no tokens. The live tests (`tests/testthat/test-live.R`) check the real APIs and run with `RNDC_LIVE_TESTS=true NDC_TOKEN=... ADC_TOKEN=... Rscript -e 'devtools::test(filter = "live")'`.
- On GitHub, [`R-CMD-check`](.github/workflows/R-CMD-check.yaml) runs on every push and pull request, [`live-checks`](.github/workflows/live-checks.yaml) runs weekly against the real APIs (it needs the `NDC_TOKEN` and `ADC_TOKEN` repository secrets), and [`docker-build`](.github/workflows/docker-build.yaml) builds the image.
- See [CLAUDE.md](CLAUDE.md) for an overview of the code.

## Citation

See [CITATION.cff](CITATION.cff).
