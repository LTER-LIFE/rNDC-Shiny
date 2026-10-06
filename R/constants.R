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

# Defaults taken from rNDC. These are internal (unexported) constants of that
# package, hence `:::`; ideally rNDC would export them. Evaluated lazily on first use.
delayedAssign("landuse_default_year", rNDC:::landuse_default_year)
delayedAssign("nitrogen_layer_choices", rNDC:::nitrogen_layer_choices)

# Datasets that come from AgroDataCube and therefore need an ADC token.
adc_datasets <- c("Weather", "Soil map", "AHN", "Agricultural fields")
