# The state shared by the parts of the server: reactive values for the polygons, the project layer, the
# overview and the download, and the API tokens. Each session has its own.

# An empty overview ("shopping cart"): one row per dataset x polygon x year/dates
empty_overview <- function() {
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

create_state <- function(ndc_token, adc_token) {
  list(
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
    overview = reactiveVal(empty_overview()),
    download_msgs = reactiveVal(character(0)),
    # a zip that the download pre-check built and verified to contain data, so that the download handler
    # can serve it without retrieving again
    prepared_zip = reactiveVal(NULL),
    # is a retrieval of this session running in the background?
    retrieving = reactiveVal(FALSE),

    # credentials
    mytoken = ndc_token,
    agro_token = adc_token,
    adc_available = nzchar(adc_token)
  )
}
