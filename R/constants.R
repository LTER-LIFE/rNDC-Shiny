available_datasets <- list(
  "Atmosphere" = c("Weather", "Nitrogen"),
  "Biosphere" = c("NDVI", "Vegetation structure"),
  "Hydrosphere" = c("Ground water table"),
  "Geosphere" = c("Soil map", "AHN"),
  "Anthroposphere" = c("Agricultural fields", "Land Use")
)

all_dataset_names <- unique(unlist(available_datasets))

dataset_tab_id <- function(name) paste0(gsub("[^a-z0-9]+", "_", tolower(name)), "_tab")

dataset_target_id <- function(name, tab) paste0("tab_target_", gsub("[^a-z0-9]+", "_", tolower(name)), "_", tolower(tab))

# Which tab(s) each dataset exposes. NDVI has both Statistics + Geodata; others one.
dataset_tabs <- function(name) {
  if (identical(name, "NDVI")) return(c("Statistics", "Geodata"))
  if (identical(name, "AHN")) return(c("Statistics", "Geodata"))
  if (identical(name, "Weather")) return("Statistics")
  "Geodata"
}

dataset_default_tab <- function(name) dataset_tabs(name)[1]

# Collection IDs in the STAC catalogue
ndc_lter_collection <- "lter"

ndc_snl_collection  <- "snl"

# SNL performance knobs -------------------------------------------------------
# Below this leaflet zoom level we do NOT fetch/draw SNL parcels (too many).
# Lower = parcels appear sooner (less zooming needed) but heavier fetches.
snl_min_zoom    <- 12L

# Hard cap on parcels fetched per viewport request (testing safeguard).
snl_fetch_limit <- 1000L

# The LTER project classes, each with the pattern that identifies it in the `name` of a feature (the first
# pattern that matches wins; case is ignored). Used to classify the fetched features, to find the NDVI
# collection of a polygon from its name and to build the project menu.
lter_class_patterns <- c("Light on Nature" = "^Lantaarnpaal", "Loobos" = "^Loobos$",
                         "Nestboxes" = "Nestkast", "Nutnet" = "^Nutnet$")

lter_class_levels <- names(lter_class_patterns)

# Largest upload (MB). Shiny's own limit is 5 MB, which is too small for many shapefiles and GeoPackages.
# Set another with the environment variable NDC_MAX_UPLOAD_MB, or the option `shiny.maxRequestSize` (in bytes).
default_upload_mb <- 100

# Weather is requested in chunks of this many days, with a pause (in seconds) between the requests
weather_chunk_days <- 200L

weather_chunk_sleep <- 0.5

# How long the requests take, measured on a normal connection: seconds per Weather request (one chunk, with
# the pause) and per daily NDVI download. They are used to warn before the overview takes long: a retrieval
# keeps the app busy until it is done. A retrieval above `warn_request_seconds` is warned about, and one
# above `max_request_seconds()` is refused (option `rNDC.Shiny.max_request_seconds`, or the environment
# variable NDC_MAX_REQUEST_SECONDS; the default is 300 seconds, or 900 when retrievals run in the background,
# where they hold nobody up).
request_seconds <- c(weather_chunk = 0.9, ndvi_day = 0.25, raster_request = 2, raster_million_cells = 0.55)

# The cells of a raster of the NatureDataCube (AHN, soil map) above which rNDC requests a lower resolution (its `max_cells`),
# and the size of the cells (m) of each: they give the time of a retrieval (measured with 2100 km2: about 16 s for each
# AHN raster and 13 s for the soil map; with 5e7 cells it took twice as long and twice the memory)
raster_max_cells <- 2e7
raster_cell_size <- list(AHN = c(dtm = 5, ohn = 2.5), "Soil map" = c(soil = 5))

warn_request_seconds <- 60

# Earliest data of the sources. They bound what the date and year inputs offer. They are the coverage that rNDC
# states for the datasets of the AgroDataCube and GroenMonitor (rNDC::ndc_external_datasets(), where they are
# checked against the live services), evaluated on first use.
external_start <- function(id) {
  ext <- rNDC::ndc_external_datasets()
  ext$start[ext$id == id]
}

# KNMI daily data; the closest station may start later (then: "no data")
delayedAssign("weather_min_date", external_start("weather"))

# the GroenMonitor daily NDVI starts on 2017-05-26: the inputs are by month
delayedAssign("ndvi_min_month", as.Date(format(external_start("groenmonitor-ndvi"), "%Y-%m-01")))

# AgroDataCube field geometries (2008 has none)
delayedAssign("fields_min_year", as.integer(format(external_start("agricultural-fields"), "%Y")))

# Defaults of the rasters, taken from rNDC (>= 0.5.1) and evaluated on first use: the STAC collection of the
# Land Use rasters, its default year, and the nitrogen layers.
delayedAssign("ndc_landuse_collection", rNDC::ndc_landuse_collection())

delayedAssign("landuse_default_year", rNDC::ndc_landuse_default_year())

delayedAssign("nitrogen_layer_choices", rNDC::ndc_nitrogen_layers())

delayedAssign("ndc_soil_collection", rNDC::ndc_soil_collection())

delayedAssign("ndc_ahn_collection", rNDC::ndc_ahn_collection())

# Datasets that come from AgroDataCube and therefore need an ADC token (the others come from the NatureDataCube).
adc_datasets <- c("Weather", "Agricultural fields")

# What the layers of the AHN are
ahn_layer_labels <- list(dtm = "terrain height", ohn = "object height")

dataset_info <- list(
  "Weather" = list(title = "Weather", description = "KNMI weather data for the selected area (daily aggregates).", notes = "Choose a date or a period."),
  "Nitrogen" = list(title = "Nitrogen", description = "Nitrogen deposition layers (ntot, nox, nh3).", notes = "Select a year. Retrieval returns all three raster layers."),
  "NDVI" = list(title = "NDVI", description = "Monthly average NDVI.", notes = "Statistics: monthly NDVI summaries per polygon for LTER/SNL project areas. Geodata: monthly average NDVI raster layers. Supports single-month or range queries."),
  "Vegetation structure" = list(title = "Vegetation structure", description = "Structural vegetation measurements.", notes = "Retrieval not yet wired in this simplified version."),
  "Ground water table" = list(title = "Ground water table", description = "Groundwater depth information.", notes = "Retrieval not yet wired in this version."),
  "Soil map" = list(title = "Soil map", description = "Soil map of the Netherlands (Bodemkaart, 1:50,000) for the selected area, from the NatureDataCube: a raster with 5 m cells and the code of the soil type in each cell.", notes = "Retrieval gives the raster and a table with the name of the soil type of each code. A code that the table does not list (16 and above) has no name in the NatureDataCube."),
  "AHN" = list(title = "AHN", description = "Heights of the Netherlands (AHN), from the NatureDataCube: the terrain height (AHN3, 5 m cells) and the object height of vegetation and buildings (AHN4, 2.5 m cells), in metres.", notes = "Statistics: the area and the minimum, maximum, mean and standard deviation of the heights in the selected area. Geodata: one raster for each layer. For a large area the rasters are requested with larger cells."),
  "Agricultural fields" = list(title = "Agricultural fields", description = "Data about agricultural fields e.g. crop type.", notes = "Requires a year."),
  "Land Use" = list(title = "Land Use", description = "Land use raster layer (LGN) for the selected area.", notes = "Select a year.")
)
