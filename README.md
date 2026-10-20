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
| `NDC_TOKEN` | **Required.** The *NatureDataCube* STAC API: project layers, and the datasets NDVI, Land Use, Nitrogen, Soil map and AHN. |
| `ADC_TOKEN` | Optional. The *AgroDataCube* REST API: Weather and Agricultural fields, the two datasets that the NatureDataCube does not have. Without it, these two are disabled in the app. |

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

The zip file also has a folder `parameters` with a JSON file per dataset: the parameters of the retrieval (the area as it was sent, the year or period, ...; never a token). `rNDC::ndc_repeat("parameters/land_use_geodata_loobos.json")` repeats a retrieval in R, and the column `parameters` of `download_summary.csv` tells which file belongs to which row. The checkbox "Include the parameters of each dataset" under the download button turns this off.

When a limit is exceeded, or you need help: see https://ndc.wur.nl or contact LTER-LIFE@nioo.knaw.nl (the dialog of the button below, the messages about the limits and the info button say so, with the limits as they are set).

The button **Check data availability**, below the map (the info button next to "Choose dataset(s)" explains the marks), tells which datasets have data for the selected area, in two ways at once: it marks the datasets in the menu and says what was found under the settings of the selected dataset (see below), and it opens a dialog that lists all the datasets for the selected polygons (and, optionally, a period): the NatureDataCube collections are checked against their items, and the AgroDataCube and GroenMonitor datasets are listed by the coverage that is known for them (`rNDC::ndc_available()`).

The app can also be used to only download the data: "Download dataset(s)" gives a zip file with the data, the selected polygons, a `download_summary.csv` (with the `reference` and `license` of the datasets that the NatureDataCube describes) and a `CITATIONS.txt` with the references to cite. They are read from the metadata of the NatureDataCube collections (`rNDC::ndc_metadata()`: `sci:citation`, `sci:doi`, `sci:publications` and the license); the AgroDataCube datasets have none, and are listed as such. If the metadata cannot be read, the download is made without references and the messages say so.

See [`examples/tutorial.R`](examples/tutorial.R) for a tutorial that combines bird nest data with weather data retrieved through the app.

## Datasets

| Dataset | Output | Retrieved with | Token |
|---|---|---|---|
| Weather (KNMI, closest station) | table | `rNDC::adc_weather_station()`, `adc_weather_date()`, `adc_weather_long_period()` | `ADC_TOKEN` |
| Agricultural fields | `sf` | `rNDC::adc_url()`, `rNDC::adc_get()` (all pages of the results) | `ADC_TOKEN` |
| AHN, Statistics | table (area, minimum, maximum, mean and standard deviation of the heights, for the terrain and the object height) | `rNDC::ndc_ahn_stats()` (collection `ahnmetrics`) | `NDC_TOKEN` |
| AHN, Geodata | two rasters (terrain height, 5 m, and object height, 2.5 m; in metres) | `rNDC::ndc_ahn_raster()` | `NDC_TOKEN` |
| Soil map | raster (the code of the soil type in each 5 m cell) and a table with the name of each code | `rNDC::ndc_soil_raster()` (collection `soiltypes`) | `NDC_TOKEN` |
| NDVI, Statistics | table (monthly means per polygon) | `rNDC::ndc_get()` on `ndvi-lter` / `ndvi-snl`; only for LTER and SNL project areas | `NDC_TOKEN` |
| NDVI, Geodata | raster (monthly means, NDVI from 0 to 1) | `rNDC::ndc_ndvi_month()`, `ndc_ndvi_stack()` (the `ndvi` collection) | `NDC_TOKEN` |
| Land Use | raster | `rNDC::ndc_landuse_raster()` | `NDC_TOKEN` |
| Nitrogen | raster (`ntot`, `nox`, `nh3`) | `rNDC::ndc_nitrogen_raster()` | `NDC_TOKEN` |

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

Areas of interest can be one of the project areas (LTER projects, or SNL parcels), polygons you draw on the map (the polygon tool and the trash can of the toolbar on the map), or polygons you upload (GeoPackage, shapefile, GeoJSON, KML, or a zip file with these). Drawn and uploaded polygons have no NDVI statistics. Limits protect the server from requests that are too large:

* Uploads can be up to 100 MB in total (set `NDC_MAX_UPLOAD_MB` to change this; Shiny's own default is 5 MB), with at most 1000 polygons per file or layer. A zip file may hold at most 200 files and 500 MB when unpacked. Every polygon of a file is a shape of its own on the map that can be selected; the upload is listed once and removed as a whole. A file without a coordinate system is taken to be in WGS84, and polygons that cross themselves are repaired (the app tells you).
* At most 100 polygons can be selected at once, for every dataset (`NDC_MAX_POLYGONS` changes this), and their total area can be up to 2000 km2 (`NDC_MAX_AREA_KM2`), for every dataset except Weather, which uses the centre of each polygon and has no area limit. Uploads are limited in size and in the polygons per file or layer for everything. While the selection is above the limits a warning says which; with too many polygons all the datasets, Weather included, are disabled in the menu and in the Add button, and with too large an area all except Weather. **Check data availability** does not check (its dialog says which limit), and a dataset that is added all the same is refused. Select fewer or smaller polygons to enable them again.
* AgroDataCube (Agricultural fields) takes the polygon in the URL, which its server refuses above about 8000 characters (some 300 vertices). A longer polygon is sent with 6 decimals and, if that is not enough, simplified in steps of 1 to 1000 m; the message of the dataset says by how much. The files of the other datasets use the polygon as it is.

**Which datasets have data?** Nothing is checked while you select areas. Click **Check data availability** (the button is disabled until polygons are selected; a progress bar counts the datasets tested) and the app marks the datasets in the menu for the selected polygons: a check mark (green, orange where only part of the data was found, e.g. not all nitrogen layers), a cross where nothing was found, a question mark where the check could not be made. Point at a dataset, or choose it, for the details and for the polygons that were checked. The marks go as soon as you select other polygons, or change a year, a date or the distance of the Weather settings: they were for something else, so a yellow warning says that the availability should be evaluated again (nothing is checked then). They come back when you click the button or add a dataset. A dataset's own settings appearing when you choose it is no change. Land Use and Nitrogen (for the year you chose), Soil map and AHN are checked in the NatureDataCube, Agricultural fields (for the chosen year) with AgroDataCube, Weather by the distance to the closest station (at most 30 km by default; the Weather settings have a box "Largest distance to the weather station (km)" to change it), NDVI by its coverage (rasters: the area is in the Netherlands; statistics: it is an LTER or SNL project area). The **Check data availability** next to **Add to overview** asks the services about the period you chose (the day or the months): for the rasters the number of days with data ("NDVI rasters: 4 days with data between 2024-06-01 and 2024-06-30 for the selected area."), for the statistics the observations and months of the project polygon, and a period without any is "no data" (the check fails and Add stays disabled). The other datasets are told the same way for the year or the dates you chose: Land Use, Nitrogen, the soil map and the AHN name their years and layers and, when the year has none, list the years that have data; Agricultural fields say whether there are fields in the year (the register has 2009 to last year); and Weather gives the closest station, its distance and the number of days with observations in the dates (up to 500), or when its series starts. **Adding a dataset checks it too**, for each selected polygon with the settings you chose (year, NDVI view): a polygon where the services have no data for the dataset is not added, and a message says what to change (the area, the year or the period); the other polygons are added. A second **Check data availability** button, next to **Add to overview**, does all these checks for the dataset and settings above it (the year, date or months; the validity of the NDVI months; the time the retrieval would take) without adding anything, and **Add to overview** is enabled only after that check has passed: it is disabled again when the dataset, its view (Statistics or Geodata), the year, the date, a single month or a range of months, the Weather distance or the selected polygons change. A check that cannot be made (a service is down or slow) does not stop a dataset from being added. The check tells what the services know about the area.

## Running with Docker or Podman

The Shiny app can also be run as a container, which installs the package (`rNDC.Shiny`) and all its dependencies, including `rNDC`, automatically.

**Setup:**

Copy `.env.example` to `.env` and fill in your API tokens:

```         
NDC_TOKEN=your_token_here
ADC_TOKEN=your_agrodatacube_token_here
SHINY_APP_BASE_URL=/naturedatacube
```

`NDC_TOKEN` is required. `ADC_TOKEN` is optional: without it, the datasets from AgroDataCube (Weather and Agricultural fields) are disabled; everything else comes from the NatureDataCube. `SHINY_APP_BASE_URL` is only needed when a reverse proxy publishes the app under a path. The optional `NDC_ASYNC`, `NDC_WORKERS`, `NDC_MAX_REQUEST_SECONDS` and `NDC_MAX_UPLOAD_MB` are described in the sections above; in the container the retrievals run in background processes by default (`NDC_ASYNC=true`), up to 4 at the same time.

**Run:**

``` bash
# Docker
docker compose up --build

# Podman
podman compose up --build
```

The app will be available at `http://localhost:3838/`.

The image is reproducible: its base is `rocker/shiny` at a fixed R version and digest, and it installs `rNDC` at the tag of a release (`RNDC_REF` in the `Dockerfile` and `compose.yaml`, now `v0.5.2`; the image is named `rndc-shiny:0.5.2` and its labels tell which rNDC it holds). To use another tag, branch or commit, set `RNDC_REF` (e.g. `RNDC_REF=main docker compose up --build`). When a new rNDC release is needed, change the default of `RNDC_REF` in both files (and `rNDC (>= ...)` in `DESCRIPTION`).

The build downloads `rNDC` from GitHub, which limits anonymous requests; if the build fails because of that, pass a GitHub token as a build secret (it is not kept in the image): `GITHUB_PAT=<token> docker build --secret id=github_pat,env=GITHUB_PAT -t rndc-shiny .`.

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

## ToDo

- Consider allowing puse of per-user tokens.

## Citation

See [CITATION.cff](CITATION.cff).
