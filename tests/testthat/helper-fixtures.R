# Fixtures for the offline tests. HTTP is stubbed at transport level with webmockr (as in rNDC):
# rstac and httr both go through httr, so one adapter covers the STAC and the AgroDataCube requests.

json_header <- list("Content-Type" = "application/json")

json_body <- function(x) as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null"))

unit_square <- function(x = 0, y = 0, size = 1) {
  list(list(c(x, y), c(x + size, y), c(x + size, y + size), c(x, y + size), c(x, y)))
}

wkt_square <- "POLYGON((5.75 52.05,5.76 52.05,5.76 52.06,5.75 52.06,5.75 52.05))"

# A one-polygon sf object with the columns that the app adds to selected polygons.
selected_polygon <- function(name = "Own polygon", ...) {
  sf::st_sf(wkt = wkt_square, source_name = name, layer_id = 1L, ...,
            geometry = sf::st_as_sfc(wkt_square, crs = 4326))
}

# A fixed (project) polygon as the app keeps them: layer_id, source_name, wkt and a square geometry
fixed_square <- function(layer_id, x, y, name = paste0("Project_", layer_id), size = 0.01) {
  geom <- sf::st_as_sfc(sf::st_bbox(c(xmin = x, ymin = y, xmax = x + size, ymax = y + size), crs = sf::st_crs(4326)))
  sf::st_sf(layer_id = as.integer(layer_id), source_name = name, wkt = sf::st_as_text(geom), geometry = geom)
}

# Run `code` in the server with tokens set. The first setInputs() runs the initial observers, which reset
# the selection and the project layers, so this does one before running `code`. The code can use the
# objects of the server (reactiveVals, input, output, session) and the helpers of the test file. The objects of
# the test are copied: to record what a mock was called with, mutate an environment (`rec$calls <- ...`)
# instead of reassigning a variable with `<<-`.
with_server <- function(code, env = parent.frame()) {
  withr::local_envvar(NDC_TOKEN = "t", ADC_TOKEN = "t", .local_envir = env)
  code <- substitute(code)
  scopes <- list()  # the environments of the test, outermost first, up to the package namespace
  e <- env
  while (!identical(e, topenv(env)) && !identical(e, emptyenv())) {
    scopes <- c(list(e), scopes)
    e <- parent.env(e)
  }
  shiny::testServer(app_server, {
    session$setInputs(selected_dataset = "Land Use")
    session$flushReact()
    scope <- new.env(parent = environment())
    for (s in scopes) list2env(as.list(s, all.names = TRUE), scope)
    eval(code, scope)
  })
}

# A map click as leaflet sends it (the nonce makes repeated clicks on the same spot distinct inputs)
map_click <- function(lng, lat, id = "") list(lng = lng, lat = lat, id = id, nonce = stats::runif(1))

# One row of the overview (what the app collects for each dataset and polygon)
overview_row <- function(dataset, view = "Geodata", year = NA_integer_,
                         from = as.Date(NA), to = as.Date(NA), name = "Own polygon") {
  tibble::tibble(dataset = dataset, view = view, year = year, polygon = name, wkt = wkt_square,
                 polygon_sf = list(selected_polygon()), date_from = from, date_to = to)
}

# ---- webmockr ----

local_webmock <- function(env = parent.frame()) {
  webmockr::enable(adapter = "httr", quiet = TRUE)
  withr::defer({
    webmockr::stub_registry_clear()
    webmockr::request_registry_clear()
    webmockr::disable(adapter = "httr", quiet = TRUE)
  }, envir = env)
  invisible(NULL)
}

# URIs and bodies of the requests sent so far
request_uris <- function() {
  reqs <- webmockr::request_registry()$request_signatures$hash
  vapply(reqs, function(r) as.character(r$sig$uri), character(1), USE.NAMES = FALSE)
}

last_request_body <- function() {
  reqs <- webmockr::request_registry()$request_signatures$hash
  bodies <- Filter(nzchar, vapply(reqs, function(r) if (is.null(r$sig$body)) "" else r$sig$body, character(1)))
  jsonlite::fromJSON(bodies[[length(bodies)]], simplifyVector = FALSE)
}

# ---- STAC API at example.org ----

stac_feature <- function(id, properties = list(), coords = unit_square(), assets = NULL) {
  feat <- list(type = "Feature", stac_version = "1.0.0", id = as.character(id), collection = "coll",
               geometry = list(type = "Polygon", coordinates = coords), bbox = c(0, 0, 1, 1),
               properties = c(list(datetime = "2024-01-01T00:00:00Z"), properties), links = list())
  if (!is.null(assets)) feat$assets <- assets
  feat
}

# A STAC item as served for the nitrogen/land use rasters (one WCS asset)
raster_item <- function(id, date) {
  stac_feature(id, list(`ndc:observation_date` = date, `ndc:layer_type` = "ntot", title = id),
               assets = list(wcs = list(href = paste0("https://example.org/data/", id))))
}

# Stub the STAC API at example.org (landing page and /search) and point rNDC at it. `pages` is a list
# with the items of each result page: every search returns the next page, linked as in the STAC spec.
# `status` is the HTTP status of the search.
local_stac_api <- function(pages = list(list()), status = 200L, env = parent.frame()) {
  local_webmock(env)
  webmockr::stub_registry_clear()  # a second call in the same test replaces the previous stubs
  webmockr::request_registry_clear()
  withr::local_options(rNDC.endpoint = "https://example.org/api/", .local_envir = env)
  withr::local_envvar(NDC_TOKEN = "t", .local_envir = env)

  webmockr::stub_request("get", "https://example.org/api/") |>
    webmockr::to_return(
      body = json_body(list(
        type = "Catalog", id = "x", description = "x", stac_version = "1.0.0",
        conformsTo = list("https://api.stacspec.org/v1.0.0/core",
                          "https://api.stacspec.org/v1.0.0/item-search",
                          "https://api.stacspec.org/v1.0.0/collections"),
        links = list())),
      headers = json_header)

  search <- webmockr::stub_request("post", "https://example.org/api/search")
  for (i in seq_along(pages)) {
    links <- if (i < length(pages)) {
      list(list(rel = "next", href = "https://example.org/api/search", method = "POST",
                body = list(page = i + 1L), merge = FALSE))
    } else list()
    search <- webmockr::to_return(
      search, status = status,
      body = json_body(list(type = "FeatureCollection", features = pages[[i]],
                            numberMatched = sum(lengths(pages)), links = links)),
      headers = json_header)
  }
  invisible(NULL)
}

# ---- AgroDataCube REST API ----

adc_re <- function(option) paste0("agrodatacube\\.wur\\.nl/api/v2/rest/", option)

geojson_features <- function(ids, id_field = "fieldid") {
  list(type = "FeatureCollection",
       features = lapply(ids, function(i) {
         list(type = "Feature", geometry = list(type = "Polygon", coordinates = unit_square(i %% 50, 0, 0.5)),
              properties = stats::setNames(list(i), id_field))
       }))
}

# The cache of the app is shared by all sessions: start and leave each test with an empty one.
local_clean_cache <- function(env = parent.frame()) {
  clear <- function() rm(list = ls(ndc_cache, all.names = TRUE), envir = ndc_cache)
  clear()
  withr::defer(clear(), envir = env)
}
