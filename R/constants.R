# Constants: dataset menu, tab layout and STAC collection settings.

available_datasets <- list(
  "Atmosphere" = c("Weather", "Nitrogen"),
  "Biosphere" = c("NDVI", "Vegetation structure"),
  "Hydrosphere" = c("Ground water table"),
  "Geosphere" = c("Soil map", "AHN"),
  "Anthroposphere" = c("Agricultural fields", "Land Use")
)

all_dataset_names <- unique(unlist(available_datasets))

make_tab_id <- function(name) paste0(gsub("[^a-z0-9]+", "_", tolower(name)), "_tab")

make_target_id <- function(name, tab) paste0("tab_target_", gsub("[^a-z0-9]+", "_", tolower(name)), "_", tolower(tab))

# Which tab(s) each dataset exposes. NDVI has both Statistics + Geodata; others one.
tabs_for_dataset <- function(name) {
  if (identical(name, "NDVI")) return(c("Statistics", "Geodata"))
  if (name %in% c("Weather", "AHN")) return("Statistics")
  "Geodata"
}

default_tab_for_dataset <- function(name) tabs_for_dataset(name)[1]

# Collection IDs in the STAC catalogue
ndc_lter_collection <- "lter"

ndc_snl_collection  <- "snl"

# SNL performance knobs -------------------------------------------------------
# Below this leaflet zoom level we do NOT fetch/draw SNL parcels (too many).
# Lower = parcels appear sooner (less zooming needed) but heavier fetches.
snl_min_zoom    <- 12L

# Hard cap on parcels fetched per viewport request (testing safeguard).
snl_fetch_limit <- 1000L

# Map each LTER project class to the rule that identifies it from `name`.
# (Used both to classify fetched features and to build the UI menu.)
lter_class_levels <- c("Light on Nature", "Loobos", "Nestboxes", "Nutnet")

# STAC collection of the Land Use rasters
ndc_landuse_collection <- "lgn"

# The datasets for which the app checks whether the selected area has data, and the STAC collection(s) to ask.
# (NDVI statistics are left out: items that intersect a project area include its neighbours.)
availability_collections <- function() {
  list("Land Use" = ndc_landuse_collection, "Nitrogen" = nitrogen_layer_choices)
}

# Earliest data of the sources, checked against the live services (see test-live.R). They bound what
# the date and year inputs offer.
weather_min_date <- as.Date("1970-01-01")  # KNMI daily data; the closest station may start later (then: "no data")
ndvi_min_month <- as.Date("2017-05-01")    # the GroenMonitor daily NDVI starts on 2017-05-26
fields_min_year <- 2009L                   # AgroDataCube field geometries (2008 has none)

# Defaults taken from rNDC. These are internal (unexported) constants of that
# package, hence `:::`; ideally rNDC would export them. Evaluated lazily on first use.
delayedAssign("landuse_default_year", rNDC:::landuse_default_year)
delayedAssign("nitrogen_layer_choices", rNDC:::nitrogen_layer_choices)

# Datasets that come from AgroDataCube and therefore need an ADC token.
adc_datasets <- c("Weather", "Soil map", "AHN", "Agricultural fields")

dataset_info <- list(
  "Weather" = list(title = "Weather", description = "KNMI weather data for the selected area (daily aggregates).", notes = "Choose a date or a period."),
  "Nitrogen" = list(title = "Nitrogen", description = "Nitrogen deposition layers (ntot, nox, nh3).", notes = "Select a year. Retrieval returns all three raster layers."),
  "NDVI" = list(title = "NDVI", description = "Monthly average NDVI.", notes = "Statistics: monthly NDVI summaries per polygon for LTER/SNL project areas. Geodata: monthly average NDVI raster layers. Supports single-month or range queries."),
  "Vegetation structure" = list(title = "Vegetation structure", description = "Structural vegetation measurements.", notes = "Retrieval not yet wired in this simplified version."),
  "Ground water table" = list(title = "Ground water table", description = "Groundwater depth information.", notes = "Retrieval not yet wired in this version."),
  "Soil map" = list(title = "Soil map", description = "Soil classification and description layers for the selected area (Soiltypes).", notes = ""),
  "AHN" = list(title = "AHN", description = "Elevation data of the Netherlands (AHN).", notes = ""),
  "Agricultural fields" = list(title = "Agricultural fields", description = "Data about agricultural fields e.g. crop type.", notes = "Requires a year."),
  "Land Use" = list(title = "Land Use", description = "Land use raster layer (LGN) for the selected area.", notes = "Select a year.")
)
