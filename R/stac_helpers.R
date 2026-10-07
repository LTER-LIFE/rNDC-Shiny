# Helpers on top of rNDC: counts of STAC items, the years of the rasters, and a small cache.

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

# Years for which rasters exist: read from the STAC items (via rNDC), cached, falling back to the known years
# when they cannot be read.
get_raster_years <- function(key, fetch, fallback) {
  years <- cache_get(key)
  if (is.null(years)) {
    years <- tryCatch(fetch(), error = function(e) character(0))
    if (length(years) == 0) return(fallback)  # not cached
    cache_set(key, years)
  }
  years
}

get_nitrogen_years <- function() {
  get_raster_years("nitrogen_years", function() rNDC::ndc_nitrogen_years(), c("2024", "2025", "2040"))
}

get_landuse_years <- function() {
  get_raster_years("landuse_years", function() rNDC::ndc_landuse_years(), as.character(landuse_default_year))
}
