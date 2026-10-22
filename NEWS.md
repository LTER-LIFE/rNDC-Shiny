# rNDC.Shiny 0.6.0

Needs rNDC 0.6.0.

## Changes

* **Soil map and AHN come from the NatureDataCube** (`rNDC::ndc_soil_raster()`, `ndc_ahn_raster()` and `ndc_ahn_stats()`, collections `soiltypes` and `ahnmetrics`) instead of the AgroDataCube, so they need `NDC_TOKEN` only: without `ADC_TOKEN` only Weather and Agricultural fields are disabled. The data is not the same: the Soil map is a raster with the code of the soil type in each 5 m cell (and a second file, a table with the name of each code; codes above 15 have no name in the NatureDataCube) instead of the polygons of the soil units, and AHN has two views. Statistics gives the area and the minimum, maximum, mean and standard deviation of the heights of the area, for the terrain height and the object height, in metres (the AgroDataCube gave centimetres, from the same data), computed from the rasters; Geodata gives the two rasters, one file each (5 m and 2.5 m cells; rNDC requests a very large area with larger cells). The references (`CITATIONS.txt` now has the AHN and the soil map) and the dataset texts follow.
* The NDVI rasters (NDVI, Geodata) are found through the `ndvi` collection of the NatureDataCube (`rNDC::ndc_ndvi_month()` and `ndc_ndvi_stack()`) instead of being requested from GroenMonitor day by day: the catalogue says which days have data, so only those are downloaded, and the dataset needs `NDC_TOKEN` like the others. The rasters are the same ones, but the layers are NDVI from 0 to 1 (rNDC divides the digital numbers of GroenMonitor by 250; before they were the numbers from 0 to 255).
* **The availability of the datasets is checked when you ask, not while you select areas.** The button **Check data availability** (below the map; disabled until polygons are selected) marks the datasets in the menu (a check mark where data was found, a cross where none was, a question mark where the check failed), says what was found under the settings of the dataset and opens the dialog with the table of `rNDC::ndc_available()` (the button that opened it, "Which datasets exist for the selected area?", is this one now). A progress bar counts the datasets tested ("3 of 9 datasets tested"; "5 of 24" for the table). The marks go, with a yellow warning to check again, when other polygons are selected or a year, a date or the Weather distance changes. A check that fails is tried once more, says why in the R console and in the hover text, and each request has a time limit of 20 seconds. An info button ("i") next to "Choose dataset(s)" explains the marks.
* **Limits of the selection.** Before, only the area of each polygon was limited (1000 km2) and the larger ones were left out. Now at most 100 polygons can be selected at once (`NDC_MAX_POLYGONS`), for every dataset, and their total area, summed over the polygons, can be up to 2000 km2 (`NDC_MAX_AREA_KM2`), for every dataset except Weather, which has no area limit. An upload may have at most 1000 polygons per file or layer and a size of 100 MB (`NDC_MAX_UPLOAD_MB`). While the selected polygons are above the limits a warning says which; with too many polygons all the datasets are disabled in the menu and in the Add button (Weather too), with too large an area all except Weather; **Check data availability** checks nothing (its dialog says "Not checked: ..."), and adding a dataset is refused with the same message. The panels to upload and to draw, and the messages about a limit, say the limits as they are set, and where to go when a request is above them or help is needed: https://ndc.wur.nl and LTER-LIFE@nioo.knaw.nl.
* **The longest retrieval that is accepted is 30 minutes** (1800 seconds; `NDC_MAX_REQUEST_SECONDS` changes it), in the R session and in the background mode alike; it was 5 and 15 minutes. The whole NDVI record (about 14 minutes of daily downloads) can now be retrieved, with the warning for what takes longer than a minute; an overview that would take longer than the limit is refused as before, and the text of the dialogs and messages says the limit as it is set.
* **`ndc_gui2r()` returns the data to R; `ndc_gui()` no longer has the button.** `data_ndc <- ndc_gui2r()` opens the same app with the button "Return data to R (close app)" and returns what the button gives (`datasets`, `overview`, `messages`, `summary`, `parameters`); it needs an interactive R session. `ndc_gui()` (and the Docker image) never show the button, so it is for downloading the zip. Before, `ndc_gui()` showed the button in every interactive session.
* The estimate of how long a retrieval takes knows the AHN and the soil map: the time grows with the cells of the area, up to the cells that rNDC requests (2e7), so the largest area allowed (2000 km2) takes about 65 s for the statistics and both rasters of the AHN and the soil map together (measured: 70 s, a zip of 76 MB, and 2.6 GB of memory at the most; a smaller area takes seconds). Such an overview is warned about, as longer than a minute, and refused only above the limit. A download no longer keeps the data of its rows in memory once they are written.

## New

* **Add to overview** has a second **Check data availability** button next to it. It does all the checks of adding the dataset, for the dataset and settings above it, without adding it: the settings (NDVI months), the limits of the selection, the availability of the data for each polygon (a polygon without data is left out, and the user is told which), and the time of the retrieval. **Add to overview** is disabled until that check has passed, and again whenever the dataset, its view, the year, the date, a single month or a range of months, the Weather distance or the selected polygons change. Clicking Add all the same repeats the checks; a check that cannot be made does not stop the dataset.
* **Save parameters (JSON)**, next to "Download dataset(s)", saves the parameters of the datasets in the overview without retrieving them: the files that `rNDC::ndc_repeat()` runs (one `.json` file for one dataset, a zip for several, with a `README.txt`), named like the files of the download. Weather has two files (the closest station of the area, which the app asks the AgroDataCube for, and its data); NDVI statistics have a file for the LTER and SNL project areas only. They never have a token. It uses `rNDC::ndc_params_write()`.
* The check tells what the services have for the year or the dates that are added, for every dataset, and a year or dates without data are no data (the check fails and Add stays disabled): Land Use and Nitrogen name the year and the layers ("Nitrogen 2025: ntot, nox and nh3 available for the selected area.", "Nitrogen 2040: only ntot and nox available (nh3 missing)") and, when the year has no data, the years that have ("No Land Use data for 2031 found for the selected area. Available years: 2024."); the soil map and the AHN name their years and layers ("AHN: terrain height (2019) and object height (2022) available for the selected area."); Agricultural fields say the year and, when there are none, the years of the register; Weather tells the closest station, its distance and the number of days with observations in the dates ("Weather: station 275 (16 km away) has observations on 7 days between 2024-06-01 and 2024-06-07."; at least 500 for a long period), or, when there are none, when the series of the station starts; and NDVI gives the days with rasters ("NDVI rasters: 4 days with data between 2024-06-01 and 2024-06-30 for the selected area.") or the observations of an LTER or SNL project polygon ("NDVI statistics: 25 observations in 11 months between 2024-01-01 and 2024-12-31 for <polygon>."), and a statistics view for an area that is no project area is no data. The general check under the map tells the same for the years in the inputs, the coverage of NDVI and the distance for Weather, and says to click the Check data availability button next to the Add to overview button for a period.

## Internal

* `httr` is imported (the time limit of the checks), and rNDC 0.6.0 is required. The image of the Docker file and the default `RNDC_REF` are 0.6.0.

# rNDC.Shiny 0.5.2

## Changes

* The functions of rNDC that the app calls were renamed in rNDC 0.5.2 (`get_landuse_raster()` is `ndc_landuse_raster()`, `get_meteo_for_period()` is `adc_weather_period()`, `download_avg_ndvi_month()` is `gm_ndvi_month()`, and so on, with `roi`, `trange` and `station` as the names of the arguments): the app uses the new names. It needs rNDC 0.5.2. The `parameters` files of the downloads are named after the new functions (`rNDC-ndc_landuse_raster/1`).

## New

* The download has the parameters of each retrieval, to repeat it in R: a folder `parameters` in the zip with one JSON file per dataset, named after its data file (`parameters/land_use_geodata_loobos.json`; Weather has a second file `..._station.json` for the choice of the weather station), and the column `parameters` of `download_summary.csv` names it. `rNDC::ndc_repeat("parameters/land_use_geodata_loobos.json")` runs the retrieval again, with the same area, year and period (add `token =` for the AgroDataCube; the files never hold a token). The area is in the file as it was sent: the shortened polygon for AgroDataCube. A checkbox under the download button turns it off (on by default). When the data are returned to R, the files come as texts in `parameters`. This needs the parameters of the retrieval functions of rNDC 0.5.2, and `zip` 2.3.0 (the folder is kept in the zip).
* "Which datasets exist for the selected area?" (a button under the settings of the selected dataset) opens a dialog with the table of `rNDC::ndc_available()` for the selected polygons, and for a period if you ask for one: every dataset of the NatureDataCube (checked against its items, with the number of items and the license) and the datasets of the AgroDataCube and GroenMonitor, which are listed by the coverage that is known for them (marked "expected" or "outside its coverage": nothing is asked of those services). The table is cached per area and period.
* The first dates of the sources that bound the date and year inputs (Weather 1970, Agricultural fields 2009, NDVI May 2017) are no longer written in this package: they are the coverage that `rNDC::ndc_external_datasets()` states.
* The download has the references of the datasets: the columns `reference` and `license` of `download_summary.csv` are filled, and the zip has a `CITATIONS.txt` with the citation, DOI link, license and related publications of every dataset that was retrieved. They come from the metadata that the NatureDataCube publishes for its collections (`rNDC::ndc_metadata()`, the Scientific extension: `sci:citation`, `sci:doi`, `sci:publications`): Land Use from `lgn`, Nitrogen from `ntot`, `nox` and `nh3`, the NDVI rasters from `ndvi` and the NDVI statistics from `ndvi-lter` or `ndvi-snl`. The AgroDataCube datasets have no such metadata and are listed in `CITATIONS.txt` as retrieved from the AgroDataCube. If the metadata cannot be read the download is made without references, and the messages say so. This needs rNDC 0.5.2.
* Drawing your own polygon (the polygon tool and the trash can of the map toolbar) and uploading polygon files (GeoPackage, shapefile, GeoJSON, KML, or a zip with these) are available again: both panels were switched off.
* Limits for polygons that users bring: datasets (Weather excepted) are only added for polygons up to 1000 km2 (`NDC_MAX_AREA_KM2`), and an upload may have at most 1000 polygons per file or layer. The panels say so.
* A polygon whose WKT is too long for AgroDataCube (its server refuses URLs above about 8000 characters, i.e. about 300 vertices) is sent with 6 decimals and, if needed, simplified up to 1000 m; the message of the dataset tells how much. Before, such a request failed.

* Uploaded and drawn polygons are checked: a polygon that crosses itself is repaired (a drawn one can; the toolbar allows it), z coordinates are dropped, and a polygon that cannot be repaired is left out. A file without a coordinate system (or with an "undefined" one, as a GeoPackage can have, which used to make the upload fail) is taken to be in WGS84, and the app says so. The notes are shown after the upload.
* A zip file is not read when it has more than 200 files, unpacks to more than 500 MB, or has paths outside the archive. Files in folders of a zip are found.

* The datasets are marked in the menu when an area has been selected: a check mark where data was found, a cross where none was, a question mark where the check failed (point at a dataset for the sentence). The check covers all datasets: Land Use and Nitrogen from the NatureDataCube (one search for all of them instead of one per collection, with `rNDC::stac_availability()`, new in rNDC after 0.5.1), Soil map, AHN and Agricultural fields by a request to AgroDataCube, Weather by the distance to the closest station (30 km at most, which the Weather settings have a box for), the NDVI rasters by the outline of the Netherlands and the NDVI statistics by project areas. A dataset where no data was found can still be added, with a warning.

## Fixes

* Uploads no longer leave temporary folders in the shared temp directory: they are removed when the file has been read, or, for a zipped GeoPackage that waits for a layer to be chosen, when the session ends.
* The features of an uploaded file are separate shapes: clicking one selects that polygon only (it used to select every feature of the file). The list of uploads shows one entry per file or layer with its number of features (it said 1), and "Remove" removes the whole upload.

## Internal

* The files and functions are arranged like those of rNDC: the name starts with the layer (`job_*` for the background processes, `retrieve_*` and `export_*` for retrieving and writing, `polygon_*`, `project_*`, `stac_*`, `availability_*`, `state_*`, `map_helpers_*`, `dataset_*`), the long files were split (`retrieval.R`, `polygons.R`, `async.R`, `jobs.R`, `lookups.R`), the very small ones joined to a neighbour of their layer, and each test file is named after the R file that it tests. Nothing that is exported changed (`ndc_gui()` and `ndc_app()`); see "Architecture" in CLAUDE.md for the layout.

# rNDC.Shiny 0.5.1

## New

* Retrievals in the background (in a deployment, e.g. the Docker image) wait in a queue when all background processes are busy: the progress bar shows the position ("Position 2 of 3 in the queue"). `NDC_MAX_QUEUE` (default 20) sets how many may wait; beyond that a new retrieval is refused with a message.
* A "Cancel retrieval" button stops a retrieval that waits or runs: its process is killed (with the processes it started) and what it made is removed. Leaving the page does the same, and so does stopping the app.
* The progress bar uses the progress that `rNDC::ndc_with_progress()` reports (the request that starts and the total), instead of the wording of rNDC's messages.

## Changes

* Every background retrieval is its own R process (`callr`), started by a small job manager (`R/jobs.R`) that is shared by all sessions. This replaces the `future` workers (`future` and `promises` are no longer used); `NDC_WORKERS` still sets how many run at the same time. In a package that is only loaded with `devtools::load_all()` the processes load it from its source folder (this needs `pkgload`); forked processes are no longer used.
* A background retrieval gets the API tokens in the environment of its process, not in the arguments of the job (which `callr` saves to a file).
* The check for data in the selected area counts what is not cached in one call of `rNDC::ndc_datasets(collections = )` (one collection at a time if that fails), uses `rNDC::stac_year_trange()` for the year, and passes the session's token to rNDC (it used the default of rNDC). Requires rNDC 0.5.1 or later.
* The Docker build is reproducible: the base image is `rocker/shiny` at a fixed R version and digest (it was `latest`), and `rNDC` is installed at the tag of its release (`RNDC_REF`, `v0.5.1`; it was a branch). `leaflet.extras` comes from CRAN (2.1.0 or later) and is no longer a `Remotes:` entry. A new rNDC release means a new `RNDC_REF`. `rNDC` is installed from `LTER-LIFE/rNDC` (its main branch) when the package is installed from GitHub.
* The overview table shows its dates with the same function as the download summary.
* The study sites of `rNDC::ndc_sites()` are the same polygons as the LTER projects, so the project menu has no separate group for them; the LTER projects have the `ndc_id` that NDVI statistics need. A live test checks that they still coincide.

## Fixes

* The cache of the lookups (item counts, years) has a limit of 500 entries: expired entries go first, then the oldest. It used to grow with every area that was looked at.
* The app does not read the result of a background retrieval, which can be large, each time it looks at the job: a small status file says when it is done.

## Internal

* The LTER project classes are defined once (`lter_class_patterns`): they classify the features, find the NDVI collection of a polygon from its name and build the project menu.
* Removed duplicated and unused code: the repeated clearing of the map and the sidebar when a project is chosen or polygons are drawn or uploaded, the helper for the next layer id (`next_layer_id()`), and the unused spinner of the SNL status line.
* `R/stac_helpers.R` is now `R/lookups.R` (and its tests `test-lookups.R`).
* Tests for the job manager (with a fake process and with real `callr` processes), the queue and the cancel button, and the cache limit. The Docker workflow runs a job in a fresh process of the image.

# rNDC.Shiny 0.5.0

## New

* The repository is now an R package (`rNDC.Shiny`) that depends on [rNDC](https://github.com/LTER-LIFE/rNDC). Launch the app with `ndc_gui()`; `ndc_app()` gives the app object (e.g. for Shiny Server).
* `ADC_TOKEN` is optional: without it, the datasets from AgroDataCube (Weather, Soil map, AHN and Agricultural fields) are disabled.
* The "Return data to R (close app)" button works again (it is shown in interactive R sessions).
* The messages panel shows what happened to each dataset (retrieved, or failed and why), also when only part of the selection could be retrieved.
* The app shows whether the selected area has Land Use or Nitrogen data (for the selected year) before you add the dataset.
* In a deployment (e.g. the Docker image) retrievals run in background R processes (`future` workers), so that a long retrieval does not hold up the other users; the progress bar follows it from the background. `NDC_ASYNC` and `NDC_WORKERS` set this; in an interactive R session retrieval stays in the session. A session retrieves one overview at a time.
* Uploads can be up to 100 MB (`NDC_MAX_UPLOAD_MB`), instead of Shiny's 5 MB, and the page says so.
* Long retrievals are estimated when a dataset is added: the app warns above about a minute and refuses above 5 minutes (`NDC_MAX_REQUEST_SECONDS` changes the limit, which is 15 minutes in the background mode), counting the whole overview. While retrieving, the progress bar and its detail show where the retrieval is (e.g. the 3rd of 10 Weather requests).
* The Land Use year can be chosen (the years come from the NatureDataCube; only 2024 is available for now).
* The Docker image installs the package, with `rNDC` from a configurable branch (`RNDC_REF`). The Docker build checks each step (a missing package fails the build), can use a GitHub token against rate limits (`docker build --secret id=github_pat,env=GITHUB_PAT .`), and the `docker-build` workflow also starts the container and checks that it answers.
* Offline tests (HTTP stubbed with webmockr), live tests, and the `R-CMD-check`, `live-checks` and `docker-build` GitHub Actions. `aidecl.yaml` declares the use of AI tools.

## Changes

* Requires rNDC 0.5 or later: the helpers that this package had written for itself (all pages of a STAC search, all pages of an AgroDataCube request, the monthly NDVI statistics, the years of the rasters, and the defaults of the rasters, which were taken from rNDC's internals) are now exported by rNDC.

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
* A retrieval that finishes after the user has left the page is cleaned up (no reactive values of a closed session are touched, and no zip is left behind).

## Internal

* The server code is split into one function per area (`R/server_*.R`), and the retrieval into one function per dataset (`R/retrieval.R`).
* The map, upload, project (LTER and SNL), overview and download parts of the server are now covered by tests.
