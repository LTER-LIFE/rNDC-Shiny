# The state shared by the parts of the server: reactive values for the polygons, the project layer, the
# overview and the download, and the API tokens. Each session has its own.

# An empty overview ("shopping cart"): one row per dataset x polygon x year/dates
state_empty_overview <- function() {
  tibble::tibble(
    dataset = character(),
    view = character(),
    year = integer(),
    polygon = character(),
    wkt = character(),
    polygon_sf = list(),
    date_from = as.Date(character()),
    date_to = as.Date(character())
  )
}

state_create <- function(ndc_token, adc_token) {
  state <- list(
    # polygons: drawn on the map, selected, from a project layer ("fixed"), and uploaded
    drawn_features = reactiveVal(NULL),
    selected_polygons = reactiveVal(NULL),
    fixed_polys = reactiveVal(NULL),
    uploaded_polys = reactiveVal(NULL),
    pending_gpkg = reactiveVal(list()),

    # project selection (LTER classes, SNL)
    active_project = reactiveVal(NULL),
    snl_last_bbox = reactiveVal(NULL),
    snl_status_msg = reactiveVal(NULL),

    # datasets chosen, and their retrieval
    overview = reactiveVal(state_empty_overview()),
    download_msgs = reactiveVal(character(0)),
    # a zip that the download pre-check built and verified to contain data, so that the download handler
    # can serve it without retrieving again
    prepared_zip = reactiveVal(NULL),
    # which datasets have data for the selected area (R/availability.R), or NULL
    availability = reactiveVal(NULL),
    # can "Add to overview" be clicked? (a dataset is chosen with its settings, polygons are selected, and the selection is within the limits)
    add_enabled = reactiveVal(FALSE),
    # can "Check data availability" be clicked? (are polygons selected)
    check_enabled = reactiveVal(FALSE),
    # how far (km) the weather station may be for the Weather check; the input only exists while Weather is chosen
    weather_max_km = reactiveVal(weather_max_station_km),
    # is a retrieval of this session running in the background?
    retrieving = reactiveVal(FALSE),
    job_id = reactiveVal(NULL),  # the background job of the session (see R/job_manager.R)

    # credentials
    mytoken = ndc_token,
    agro_token = adc_token,
    adc_available = nzchar(adc_token)
  )
  # what is wrong with the selected polygons (see selection_problems()): sentences, none when they are within the limits
  state$selection_issues <- reactive(selection_problems(state$selected_polygons()))
  state
}
