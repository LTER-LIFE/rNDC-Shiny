# rNDC.Shiny 0.4.0

## New

* The repository is now an R package (`rNDC.Shiny`) that depends on [rNDC](https://github.com/LTER-LIFE/rNDC) (branch `texel26`). Launch the app with `ndc_gui()`; `ndc_app()` gives the app object (e.g. for Shiny Server).
* `ADC_TOKEN` is optional: without it, the datasets from AgroDataCube (Weather, Soil map, AHN and Agricultural fields) are disabled.
* The "Return data to R (close app)" button works again (it is shown in interactive R sessions).
* The messages panel shows what happened to each dataset (retrieved, or failed and why), also when only part of the selection could be retrieved.
* The app shows whether the selected area has Land Use or Nitrogen data (for the selected year) before you add the dataset.
* In a deployment (e.g. the Docker image) retrievals run in background R processes (one `callr` process per retrieval), so that a long retrieval does not hold up the other users; the progress bar follows it from the background. When all processes are busy a retrieval waits in a queue (the page shows its position), and a "Cancel retrieval" button stops a waiting or running retrieval (its process is killed and what it made removed; leaving the page does the same). `NDC_ASYNC`, `NDC_WORKERS` and `NDC_MAX_QUEUE` set this; in an interactive R session retrieval stays in the session. A session retrieves one overview at a time.
* Uploads can be up to 100 MB (`NDC_MAX_UPLOAD_MB`), instead of Shiny's 5 MB, and the page says so.
* The progress bar uses the progress that `rNDC::ndc_with_progress()` reports (no longer the wording of rNDC's messages).
* Long retrievals are estimated when a dataset is added: the app warns above about a minute and refuses above 5 minutes (`NDC_MAX_REQUEST_SECONDS` changes the limit, which is 15 minutes in the background mode), counting the whole overview. While retrieving, the progress bar and its detail show where the retrieval is (e.g. the 3rd of 10 Weather requests).
* The Land Use year can be chosen (the years come from the NatureDataCube; only 2024 is available for now).
* The Docker image installs the package, with `rNDC` from a configurable branch (`RNDC_REF`).
* Offline tests (HTTP stubbed with webmockr), live tests, and the `R-CMD-check`, `live-checks` and `docker-build` GitHub Actions.

* Requires rNDC 0.5 or later: the helpers that this package had written for itself (all pages of a STAC search, all pages of an AgroDataCube request, the monthly NDVI statistics, the years of the rasters, and the defaults of the rasters, which were taken from rNDC's internals) are now exported by rNDC.
* The Docker build checks each step (a missing package fails the build), can use a GitHub token against rate limits (`docker build --secret id=github_pat,env=GITHUB_PAT .`), and the `docker-build` workflow also starts the container and checks that it answers. `aidecl.yaml` declares the use of AI tools.

## Fixes

* NDVI rasters are retrieved for the month(s) of their row in the overview, not for what the input widgets happen to say when downloading.
* NDVI statistics only contain the selected polygon's own feature (they used to include the neighbouring SNL parcels), and cover the whole last day of the period.
* All pages of results are retrieved: the LTER layer, the NDVI statistics, and the AgroDataCube Fields and Soil map (which were cut off at 50 and 25 features).
* A download no longer loses data when the same dataset is requested for the same polygon with different years or periods: the second file used to overwrite the first in the zip; it now gets the period in its name (e.g. `agricultural_fields_geodata_own_polygon_2023.gpkg`). Likewise, two different polygons with the same name (e.g. uploaded files with the same file name) both get their reference polygon file.
* AgroDataCube errors are reported with the message of the server.
* A missing meteorological station is reported properly.
* Downloads are safe when several people use the app at the same time (no `setwd()`, unique export folders, temporary files are removed).
* The date and year inputs have current limits (Weather from 1970, Agricultural fields from 2009, NDVI from May 2017: the first month with data, checked against the services), and the NDVI months are checked (valid months, start not after end, no future months, nothing before May 2017).
* The nitrogen years are read from the NatureDataCube.

## Internal

* The server code is split into one function per area (`R/server_*.R`), and the retrieval into one function per dataset (`R/retrieval.R`).
* The map, upload, project (LTER and SNL), overview and download parts of the server are now covered by tests.
