# Helpers on top of rNDC: counts of STAC items and the years of the rasters (cached, see R/cache.R).

# Number of STAC items of `collection` that intersect `roi` (an sf/sfc object), optionally within `trange`,
# via rNDC::ndc_count(). NA if the request fails. Cached per collection, area and time range.
stac_count_items <- function(collection, roi, trange = NULL, token = Sys.getenv("NDC_TOKEN")) {
  key <- cache_key("count", collection, trange, sf::st_as_text(sf::st_geometry(roi))[1])
  n <- cache_get(key)
  if (!is.null(n)) return(n)

  n <- tryCatch(as.numeric(rNDC::ndc_count(collection = collection, roi = roi, trange = trange, token = token)),
                error = function(e) NA_real_)
  if (length(n) != 1 || is.na(n)) return(NA_real_)
  cache_set(key, n)
}

# Years for which rasters exist: read from the STAC items (via rNDC), cached, falling back to the known years
# when they cannot be read.
stac_raster_years <- function(key, fetch, fallback) {
  years <- cache_get(key)
  if (is.null(years)) {
    years <- tryCatch(fetch(), error = function(e) character(0))
    if (length(years) == 0) return(fallback)  # not cached
    cache_set(key, years)
  }
  years
}

stac_nitrogen_years <- function(token = Sys.getenv("NDC_TOKEN")) {
  stac_raster_years("nitrogen_years", function() rNDC::ndc_nitrogen_years(token), c("2024", "2025", "2040"))
}

stac_landuse_years <- function(token = Sys.getenv("NDC_TOKEN")) {
  stac_raster_years("landuse_years", function() rNDC::ndc_landuse_years(token), as.character(landuse_default_year))
}
