# Helpers on top of rNDC for NatureDataCube (STAC) and AgroDataCube requests, with a small cache.

# Search the NatureDataCube STAC API and return ALL matching items as an sf
# object (NULL if nothing matches). rNDC::ndc_get(mode = "sf") only converts the
# first page of results, so go through mode = "fetch" (all pages) instead.
ndc_get_all_sf <- function(collection, roi = NULL, trange = NULL, limit = 1000) {
  items <- rNDC::ndc_get(collection = collection, roi = roi, trange = trange,
                         limit = limit, mode = "fetch")
  if (length(items$features) == 0) return(NULL)
  rstac::items_as_sf(items)
}

# Get ALL features of a paged AgroDataCube request (Fields, Soiltypes). The API
# returns a single page (default 50 features; `page_offset` is the page number),
# so keep requesting pages until one comes back short. Returns the parsed
# GeoJSON of the first page with the features of all pages combined.
adc_get_all <- function(option, params, token, page_size = 1000L, max_pages = 100L) {
  params <- c(params, page_size = as.character(page_size))
  res <- NULL
  for (page in seq_len(max_pages) - 1L) {
    url <- rNDC::adc_url(option, params = c(params, page_offset = as.character(page)))
    cur <- rNDC::adc_get(url = url, token = token)
    if (is.null(res)) res <- cur else res$features <- c(res$features, cur$features)
    if (length(cur$features) < page_size) return(res)
  }
  warning("AgroDataCube request stopped after ", max_pages, " pages; the result may be incomplete.",
          call. = FALSE)
  res
}

# Process-level cache shared by all sessions (one R process serves them all), so
# slowly-changing STAC lookups are done once rather than once per session.
ndc_cache <- new.env()

ndc_cache_ttl <- 3600  # seconds

cache_get <- function(key) {
  hit <- ndc_cache[[key]]
  if (!is.null(hit) && difftime(Sys.time(), hit$time, units = "secs") < ndc_cache_ttl) hit$value else NULL
}

cache_set <- function(key, value) {
  if (!is.null(value)) ndc_cache[[key]] <- list(value = value, time = Sys.time())
  invisible(value)
}

# Number of STAC items of `collection` that intersect `roi` (an sf/sfc object), optionally within `trange`,
# via rNDC::ndc_count(). NA if the request fails. Cached per collection, area and time range.
count_items <- function(collection, roi, trange = NULL) {
  key <- paste("count", collection, trange, sf::st_as_text(sf::st_geometry(roi))[1], sep = "|")
  n <- cache_get(key)
  if (!is.null(n)) return(n)

  n <- tryCatch(as.numeric(rNDC::ndc_count(collection = collection, roi = roi, trange = trange)),
                error = function(e) NA_real_)
  if (length(n) != 1 || is.na(n)) return(NA_real_)
  cache_set(key, n)
}

# The counts of `count_items()` for several collections (named by collection), for one year if given
items_in_area <- function(collections, roi, year = NULL) {
  trange <- if (!is.null(year) && nzchar(as.character(year))) {
    paste0(year, "-01-01T00:00:00Z/", year, "-12-31T23:59:59Z")
  }
  vapply(collections, function(collection) count_items(collection, roi, trange), numeric(1))
}

# Years for which rasters exist: read from the STAC items of the collection(s) (via rNDC), falling back to
# the known years. The nitrogen layers are one collection each.
get_raster_years <- function(collections, key, fallback) {
  years <- cache_get(key)
  if (is.null(years)) {
    years <- tryCatch({
      y <- unlist(lapply(collections, function(col) {
        items <- rNDC::ndc_get(collection = col, mode = "fetch", limit = 100)
        if (length(items$features) == 0) return(character(0))
        dplyr::bind_rows(lapply(items$features, rNDC::stac_feature_meta, asset_name = "wcs"))$year
      }))
      sort(unique(as.character(y[!is.na(y)])))
    }, error = function(e) character(0))
    if (length(years) == 0) return(fallback)  # not cached
    cache_set(key, years)
  }
  years
}

get_nitrogen_years <- function() {
  get_raster_years(nitrogen_layer_choices, "nitrogen_years", c("2024", "2025", "2040"))
}

get_landuse_years <- function() {
  get_raster_years(ndc_landuse_collection, "landuse_years", as.character(landuse_default_year))
}
