# rNDC-Shiny

This package provides a graphical user interface to *NatureDataCube*, through an R-Shiny app.

The idea of the *NatureDataCube* is to offer an accessible way for researchers/ecologists to retrieve relevant data.

*NatureDataCube* is a platform based on [*AgroDataCube*](https://agrodatacube.wur.nl/), holding and providing access to data used in the context of project [LTER-LIFE](https://lter-life.nl/en).

## Opening the Shiny app

To use the Shiny app and continue working with the retrieved data in R, the app must be launched in a specific way so that the output is stored in an R object.

Steps:

- Load the `NatureDataCubeR` package (e.g. by executing `library(NatureDataCubeR)`).
- Make sure that your working directory is set to the folder you want to work from (this can be changed with `setwd("path/to/workingdirectory")`; note that `getwd()` can be used to check the current working directory).
- Launch the Shiny app from within R by typing and executing `data_ndc <- ndc_gui()`.

Launching the app in this way ensures that the output generated through the Shiny interface is returned and stored in the R variable `data_nc` (note that this can be changed to a different object name). This allows you to continue working with the retrieved data in R after closing the app.

To retrieve data from the `NatureDataCube`, an API token is required. Make sure your token is available in your R session before requesting data.

## Generate an API token

To generate your free personal API token to retrieve data you can go to [API Access Registration](https://ndc.wur.nl/register).

## Running with Docker or Podman

The Shiny app can also be run as a container, which handles all package dependencies automatically.

**Setup:**

Copy `.env.example` to `.env` and fill in your API token:

```         
NDC_TOKEN=your_token_here
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
