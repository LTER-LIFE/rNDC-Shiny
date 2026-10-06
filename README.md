# rNDC-Shiny

This package provides a graphical user interface to *NatureDataCube*, through an R-Shiny app, using the functions and wrappers from the [`rNDC`](https://github.com/LTER-LIFE/rNDC) R package.

The idea of the *NatureDataCube* is to offer an accessible way for researchers/ecologists to retrieve relevant data.

*NatureDataCube* is a platform based on [*AgroDataCube*](https://agrodatacube.wur.nl/), holding and providing access to data used in the context of project [LTER-LIFE](https://lter-life.nl/en).

## Opening the Shiny app

To use the Shiny app and continue working with the retrieved data in R, the app must be launched in a specific way so that the output is stored in an R object.

Steps:

- Install the `rNDC` package, which the app relies on: `remotes::install_github("LTER-LIFE/rNDC", ref = "texel26")`.
- Make sure your working directory is the folder you want to work from (this can be changed with `setwd("path/to/workingdirectory")`; `getwd()` shows the current one).
- Set your tokens in the R session, e.g. `Sys.setenv(NDC_TOKEN = "...", ADC_TOKEN = "...")`. `NDC_TOKEN` is required; `ADC_TOKEN` is only needed for the AgroDataCube-based datasets (Weather, Soil map, AHN, Agricultural fields).
- Launch the app from within R with `data_ndc <- shiny::runApp("inst/shiny/naturedatacube_app")` (adjust the path to where this repository is located).

Launching the app in this way ensures that the output generated through the Shiny interface is returned and stored in the R variable `data_ndc` (this can be changed to a different object name). This allows you to continue working with the retrieved data in R after closing the app: click "Return data to R (close app)" in the app (this button is only shown in interactive R sessions).

To retrieve data from the `NatureDataCube`, an API token is required. Make sure your token is available in your R session before requesting data.

## Generate an API token

To generate your free personal API token to retrieve data you can go to [API Access Registration](https://ndc.wur.nl/register).

## Running with Docker or Podman

The Shiny app can also be run as a container, which handles all package dependencies automatically.

**Setup:**

Copy `.env.example` to `.env` and fill in your API token:

```         
NDC_TOKEN=your_token_here
ADC_TOKEN=your_agrodatacube_token_here
SHINY_APP_BASE_URL=/naturedatacube
```

**Run:**

``` bash
# Docker
docker compose up --build

# Podman
podman compose up --build
```

The app will be available at `http://localhost:3838/naturedatacube`.
