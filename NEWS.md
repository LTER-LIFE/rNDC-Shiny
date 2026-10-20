# rNDC.Shiny 0.5.2

## Changes

* The estimate of how long a retrieval takes knows the AHN and the soil map: the time grows with the cells of the area, up to the cells that rNDC requests (2e7), so the largest polygon allowed (2000 km2) takes about 65 s for the statistics and both rasters of the AHN and the soil map together (measured: 70 s, a zip of 76 MB, and 2.6 GB of memory at the most; a smaller polygon takes seconds). Such an overview is warned about, as longer than a minute, and refused only above the limit. A download no longer keeps the data of its rows in memory once they are written.
* **Soil map and AHN come from the NatureDataCube** (`rNDC::ndc_soil_raster()`, `ndc_ahn_raster()` and `ndc_ahn_stats()`, collections `soiltypes` and `ahnmetrics`) instead of the AgroDataCube, so they need `NDC_TOKEN` only: without `ADC_TOKEN` only Weather and Agricultural fields are disabled. The data is not the same: the Soil map is a raster with the code of the soil type in each 5 m cell (and a second file, a table with the name of each code; codes above 15 have no name in the NatureDataCube) instead of the polygons of the soil units, and AHN has two views. Statistics gives the area and the minimum, maximum, mean and standard deviation of the heights of the area, for the terrain height and the object height, in metres (the AgroDataCube gave centimetres, from the same data), computed from the rasters; Geodata gives the two rasters, one file each (5 m and 2.5 m cells; rNDC requests a very large area with larger cells). The availability checks, the references (`CITATIONS.txt` now has the AHN and the soil map) and the dataset texts follow. Needs the AHN and soil map functions of rNDC.
* The NDVI rasters (NDVI, Geodata) are found through the `ndvi` collection of the NatureDataCube (`rNDC::ndc_ndvi_month()` and `ndc_ndvi_stack()`) instead of being requested from GroenMonitor day by day (`gm_ndvi_month()`, `gm_ndvi_stack()`): the catalogue says which days have data, so only those are downloaded, and the dataset needs `NDC_TOKEN` like the others. The rasters are the same ones. The layers are NDVI from 0 to 1 (rNDC divides the digital numbers of GroenMonitor by 250; before they were the numbers from 0 to 255). This needs the NDVI functions of rNDC.
* The functions of rNDC that the app calls were renamed in rNDC 0.5.2 (`get_landuse_raster()` is `ndc_landuse_raster()`, `get_meteo_for_period()` is `adc_weather_period()`, `download_avg_ndvi_month()` is `gm_ndvi_month()`, and so on, with `roi`, `trange` and `station` as the names of the arguments): the app uses the new names. It needs rNDC 0.5.2. The `parameters` files of the downloads are named after the new functions (`rNDC-ndc_landuse_raster/1`).

* A check of the availability of a dataset that fails (the "?" next to Soil map, AHN, Weather and the other datasets) is tried once more, prints its error in the R console and says the reason in the hover text ("Could not check for Soil map data (HTTP 503 ...)"); before, the error was lost.

## New

* The download has the parameters of each retrieval, to repeat it in R: a folder `parameters` in the zip with one JSON file per dataset, named after its data file (`parameters/land_use_geodata_loobos.json`; Weather has a second file `..._station.json` for the choice of the weather station), and the column `parameters` of `download_summary.csv` names it. `rNDC::ndc_repeat("parameters/land_use_geodata_loobos.json")` runs the retrieval again, with the same area, year and period (add `token =` for the AgroDataCube; the files never hold a token). The area is in the file as it was sent: the shortened polygon for AgroDataCube. A checkbox under the download button turns it off (on by default). When the data are returned to R, the files come as texts in `parameters`. This needs the parameters of the retrieval functions of rNDC 0.5.2, and `zip` 2.3.0 (the folder is kept in the zip).
* The dialog of **Check data availability** has, in its footer, the limits of the app as they are set now (the area of a polygon, how long a retrieval may take, the size and the number of polygons of an upload) and where to go when a request is above them or help is needed: https://ndc.wur.nl and LTER-LIFE@nioo.knaw.nl (links). The messages about a limit that was exceeded (a polygon that is too large, an overview that would take too long, an upload with too many polygons) and the info button say it too. The address and the website are in one place in the code (`ndc_website`, `ndc_contact`).
* **Add to overview** has a **Check data availability** button next to it. It does all the checks of adding the dataset, for the dataset and settings above it, without adding it: the settings (NDVI months), the limits of the selection, the availability of the data for each polygon (a polygon without data is left out, and the user is told which), and the time of the retrieval. **Add to overview** is disabled until that check has passed, and again whenever the dataset, its view, the year, the date, a single month or a range of months, the Weather distance or the selected polygons change. Clicking Add all the same repeats the checks, as before; a check that cannot be made does not stop the dataset.
* The check of NDVI next to **Add to overview** tells what the services have for the period and the view that are added, instead of the coverage: "NDVI rasters: 4 days with data between 2024-06-01 and 2024-06-30 for the selected area.", or "NDVI statistics: 25 observations in 11 months between 2024-01-01 and 2024-12-31 for <polygon>." A period with no rasters or no observations is no data (the check fails, Add stays disabled, and adding it is refused), and so is a statistics view for an area that is no LTER or SNL project area. The general check under the map keeps telling the coverage, in clearer words, and points to the check next to Add for a period. Each question is one request (0.2-0.5 s), cached per area and period.
* An info button ("i", like the ones next to the datasets) next to "Choose dataset(s)" explains the marks, when they go and how adding a dataset is checked; it replaces the help line under that heading.
* The button **Check data availability** (below the map) is disabled until polygons are selected (like Add to overview, which also needs a dataset), and shows a progress bar with the number of datasets tested ("3 of 9 datasets tested"; adding a dataset counts its polygons); the bar of the dialog that follows (the table of all the datasets) counts the datasets that are asked one by one ("5 of 24 datasets tested", the slow part: about 10 seconds when nothing is cached). It opens a dialog with the table of `rNDC::ndc_available()` for the selected polygons, and for a period if you ask for one: every dataset of the NatureDataCube (checked against its items, with the number of items and the license) and the datasets of the AgroDataCube and GroenMonitor, which are listed by the coverage that is known for them (marked "expected" or "outside its coverage": nothing is asked of those services). The table is cached per area and period. The same click marks the datasets in the menu (see below).
* The first dates of the sources that bound the date and year inputs (Weather 1970, Agricultural fields 2009, NDVI May 2017) are no longer written in this package: they are the coverage that `rNDC::ndc_external_datasets()` states.
* The download has the references of the datasets: the columns `reference` and `license` of `download_summary.csv` are filled, and the zip has a `CITATIONS.txt` with the citation, DOI link, license and related publications of every dataset that was retrieved. They come from the metadata that the NatureDataCube publishes for its collections (`rNDC::ndc_metadata()`, the Scientific extension: `sci:citation`, `sci:doi`, `sci:publications`): Land Use from `lgn`, Nitrogen from `ntot`, `nox` and `nh3`, the NDVI rasters from `ndvi` and the NDVI statistics from `ndvi-lter` or `ndvi-snl`. The AgroDataCube datasets have no such metadata and are listed in `CITATIONS.txt` as retrieved from the AgroDataCube. If the metadata cannot be read the download is made without references, and the messages say so. This needs rNDC 0.5.2.
* Drawing your own polygon (the polygon tool and the trash can of the map toolbar) and uploading polygon files (GeoPackage, shapefile, GeoJSON, KML, or a zip with these) are available again: both panels were switched off.
* Limits for polygons that users bring: an upload may have at most 1000 polygons per file or layer (and a size, 100 MB), and at most 100 polygons can be selected at once (`NDC_MAX_POLYGONS`), for every dataset; their total area, summed over the polygons, can be up to 2000 km2 (`NDC_MAX_AREA_KM2`), for every dataset except Weather, which has no area limit. While the selected polygons are above the limits a warning (yellow) says which, all the datasets are disabled in the menu and in the Add button with too many polygons (Weather too), and all except Weather with too large an area, until they are within them, **Check data availability** checks nothing (its dialog says: "Not checked: ..."), and adding a dataset is refused with the same message and the limits and where to go. The panels to upload and to draw say so. Before, only the area was limited, polygon by polygon (2000 km2 each): the ones that were too large were left out and the others added.
* A polygon whose WKT is too long for AgroDataCube (its server refuses URLs above about 8000 characters, i.e. about 300 vertices) is sent with 6 decimals and, if needed, simplified up to 1000 m; the message of the dataset tells how much. Before, such a request failed.

* Uploaded and drawn polygons are checked: a polygon that crosses itself is repaired (a drawn one can; the toolbar allows it), z coordinates are dropped, and a polygon that cannot be repaired is left out. A file without a coordinate system (or with an "undefined" one, as a GeoPackage can have, which used to make the upload fail) is taken to be in WGS84, and the app says so. The notes are shown after the upload.
* A zip file is not read when it has more than 200 files, unpacks to more than 500 MB, or has paths outside the archive. Files in folders of a zip are found.

* The datasets are marked in the menu when **Check data availability** is clicked (not while areas are selected: nothing is checked until then): a check mark where data was found, a cross where none was, a question mark where the check failed (point at a dataset for the sentence, which is also under its settings, with the polygons that were checked). The marks go when other polygons are selected, or a year, a date or the distance of the Weather settings changes (nothing is checked then, and a warning, yellow, not the red of an error, says that the availability should be evaluated again) and come back when a check is made. The check covers all datasets: Land Use, Nitrogen, Soil map and AHN from the NatureDataCube (one search for all of them with `rNDC::stac_availability()`), Agricultural fields by a request to AgroDataCube, Weather by the distance to the closest station (30 km at most, which the Weather settings have a box for), the NDVI rasters by the outline of the Netherlands and the NDVI statistics by project areas. **Adding a dataset checks it too**, for each polygon and with the year and view chosen: a polygon where no data was found is not added (an error message says what to change), and a check that cannot be made does not stop it. A failed check says why in the R console, after a second try, and each request has a time limit of 20 seconds.

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
