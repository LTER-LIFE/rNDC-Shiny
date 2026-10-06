# rNDC.Shiny 0.4.0

## New

* The repository is now an R package (`rNDC.Shiny`) that depends on [rNDC](https://github.com/LTER-LIFE/rNDC) (branch `texel26`). Launch the app with `ndc_gui()`; `ndc_app()` gives the app object (e.g. for Shiny Server).
* `ADC_TOKEN` is optional: without it, the datasets from AgroDataCube (Weather, Soil map, AHN and Agricultural fields) are disabled.
* The "Return data to R (close app)" button works again (it is shown in interactive R sessions).
* The messages panel shows what happened to each dataset (retrieved, or failed and why), also when only part of the selection could be retrieved.
* The Docker image installs the package, with `rNDC` from a configurable branch (`RNDC_REF`).
* Offline tests (HTTP stubbed with webmockr), live tests, and the `R-CMD-check`, `live-checks` and `docker-build` GitHub Actions.

## Fixes

* NDVI rasters are retrieved for the month(s) of their row in the overview, not for what the input widgets happen to say when downloading.
* NDVI statistics only contain the selected polygon's own feature (they used to include the neighbouring SNL parcels), and cover the whole last day of the period.
* All pages of results are retrieved: the LTER layer, the NDVI statistics, and the AgroDataCube Fields and Soil map (which were cut off at 50 and 25 features).
* A download no longer loses data when the same dataset is requested for the same polygon with different years or periods: the second file used to overwrite the first in the zip; it now gets the period in its name (e.g. `agricultural_fields_geodata_own_polygon_2023.gpkg`). Likewise, two different polygons with the same name (e.g. uploaded files with the same file name) both get their reference polygon file.
* AgroDataCube errors are reported with the message of the server.
* A missing meteorological station is reported properly.
* Downloads are safe when several people use the app at the same time (no `setwd()`, unique export folders, temporary files are removed).
* The date and year inputs have current limits, and the NDVI months are checked (valid months, start not after end, no future months).
* The nitrogen years are read from the NatureDataCube.
