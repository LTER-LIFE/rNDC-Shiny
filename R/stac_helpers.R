# Helpers on top of rNDC: counts of STAC items, the years of the rasters, and a small cache.

# Process-level cache shared by all sessions (one R process serves them all), so
# slowly-changing STAC lookups are done once rather than once per session.
ndc_cache <- new.env()

ndc_cache_ttl <- 3600  # seconds

cache_get <- function(key) {
  hit <- ndc_cache[[key]]
  if (!is.null(hit) && difftime(Sys.time(), hit$time, units = "secs") < ndc_cache_ttl) hit$value else NULL
}

ndc_cache_max_entries <- 500L  # every area makes new keys: without a limit the cache would only grow

# Remove the entries that have expired and, if there are still more than `max_entries`, the oldest ones
cache_prune <- function(max_entries = ndc_cache_max_entries) {
  keys <- ls(ndc_cache, all.names = TRUE)
  if (length(keys) == 0) return(invisible(NULL))
  age <- vapply(keys, function(k) as.numeric(difftime(Sys.time(), ndc_cache[[k]]$time, units = "secs")), numeric(1))
  expired <- keys[age >= ndc_cache_ttl]
  keep <- setdiff(keys, expired)
  excess <- length(keep) - max_entries
  if (excess > 0) expired <- c(expired, keep[order(age[keep], decreasing = TRUE)][seq_len(excess)])
  rm(list = expired, envir = ndc_cache)
  invisible(NULL)
}

cache_set <- function(key, value) {
  if (!is.null(value)) {
    ndc_cache[[key]] <- list(value = value, time = Sys.time())
    cache_prune()
  }
  invisible(value)
}

# Number of STAC items of `collection` that intersect `roi` (an sf/sfc object), optionally within `trange`,
# via rNDC::ndc_count(). NA if the request fails. Cached per collection, area and time range.
count_items <- function(collection, roi, trange = NULL, token = Sys.getenv("NDC_TOKEN")) {
  key <- paste("count", collection, trange, sf::st_as_text(sf::st_geometry(roi))[1], sep = "|")
  n <- cache_get(key)
  if (!is.null(n)) return(n)

  n <- tryCatch(as.numeric(rNDC::ndc_count(collection = collection, roi = roi, trange = trange, token = token)),
                error = function(e) NA_real_)
  if (length(n) != 1 || is.na(n)) return(NA_real_)
  cache_set(key, n)
}

# The counts of `count_items()` for several collections (named by collection), for one year if given. What is not
# in the cache is asked in one call of rNDC::ndc_datasets(); if that fails, one collection at a time, so that the
# collections that can be counted are.
items_in_area <- function(collections, roi, year = NULL, token = Sys.getenv("NDC_TOKEN")) {
  trange <- rNDC::stac_year_trange(year)
  if (!is.null(trange)) trange <- rNDC::ndc_trange(trange)
  area <- sf::st_as_text(sf::st_geometry(roi))[1]
  key <- function(collection) paste("count", collection, trange, area, sep = "|")

  counts <- vapply(collections, function(collection) {
    n <- cache_get(key(collection))
    if (is.null(n)) NA_real_ else n
  }, numeric(1))
  todo <- collections[is.na(counts)]
  if (length(todo) > 0) {
    fresh <- tryCatch(rNDC::ndc_datasets(roi = roi, trange = trange, token = token, matched = TRUE,
                                         collections = todo)$n_matched,
                      error = function(e) NULL)
    if (length(fresh) == length(todo) && !anyNA(fresh)) {
      for (i in seq_along(todo)) cache_set(key(todo[i]), as.numeric(fresh[i]))
      counts[todo] <- as.numeric(fresh)
    } else {
      counts[todo] <- vapply(todo, function(collection) count_items(collection, roi, trange, token), numeric(1))
    }
  }
  counts
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

get_nitrogen_years <- function(token = Sys.getenv("NDC_TOKEN")) {
  get_raster_years("nitrogen_years", function() rNDC::ndc_nitrogen_years(token), c("2024", "2025", "2040"))
}

get_landuse_years <- function(token = Sys.getenv("NDC_TOKEN")) {
  get_raster_years("landuse_years", function() rNDC::ndc_landuse_years(token), as.character(landuse_default_year))
}
