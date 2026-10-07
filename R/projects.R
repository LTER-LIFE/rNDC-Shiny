# Project layers (LTER, SNL) and NDVI statistics from the NatureDataCube STAC API.

classify_lter <- function(sf_obj) {
  if (is.null(sf_obj) || nrow(sf_obj) == 0 || !("name" %in% names(sf_obj))) return(sf_obj)
  nm <- sf_obj[["name"]]
  sf_obj$project_class <- NA_character_
  for (cls in lter_class_levels) {
    todo <- is.na(sf_obj$project_class) & grepl(lter_class_patterns[[cls]], nm, ignore.case = TRUE)
    sf_obj$project_class[todo] <- cls
  }
  sf_obj
}

# Fetch the whole LTER collection once and classify it. Returns
# list(data, error): `data` is an sf object (transformed to 4326) or NULL, and
# `error` the failure message (NULL on success or when the collection is empty).
fetch_lter_classified <- function() {
  err <- NULL
  out <- tryCatch(
    rNDC::ndc_get(collection = ndc_lter_collection, mode = "sf", limit = 1000, all_pages = TRUE),
    error = function(e) { err <<- conditionMessage(e); NULL }
  )
  if (is.null(out) || nrow(out) == 0) return(list(data = NULL, error = err))
  if (is.na(sf::st_crs(out))) sf::st_crs(out) <- 4326
  out <- sf::st_transform(out, 4326)
  list(data = classify_lter(out), error = NULL)
}

# LTER project polygons: list(data, error) as returned by fetch_lter_classified().
# Only successful fetches are cached.
get_lter_data <- function() {
  data <- cache_get("lter")
  if (!is.null(data)) return(list(data = data, error = NULL))
  res <- fetch_lter_classified()
  cache_set("lter", res$data)
  res
}

# Fetch SNL parcels intersecting a bounding box (xmin, ymin, xmax, ymax in
# lon/lat). Returns a list(status, data, error) so the caller can tell the
# difference between "no parcels here" and "the request failed". Always live.
fetch_snl_bbox <- function(bbox) {
  if (is.null(bbox) || length(bbox) != 4 || any(!is.finite(bbox))) {
    return(list(status = "error", data = NULL, error = "Invalid bounding box."))
  }
  # Only the first `snl_fetch_limit` parcels of the view are wanted (the page says when there are more), so
  # not all pages: rNDC warns that the first page is incomplete, which is expected here.
  out <- tryCatch(
    withCallingHandlers(
      rNDC::ndc_get(collection = ndc_snl_collection, roi = unname(bbox), mode = "sf", limit = snl_fetch_limit),
      warning = function(w) if (grepl("matched items were returned", conditionMessage(w))) invokeRestart("muffleWarning")
    ),
    error = function(e) structure("ndc_error", message = conditionMessage(e))
  )
  if (is.character(out) && identical(as.character(out), "ndc_error")) {
    return(list(status = "error", data = NULL, error = attr(out, "message")))
  }
  if (is.null(out) || nrow(out) == 0) {
    return(list(status = "empty", data = NULL, error = NULL))
  }
  if (is.na(sf::st_crs(out))) sf::st_crs(out) <- 4326
  out <- sf::st_transform(out, 4326)
  list(status = "ok", data = out, error = NULL)
}

# NDVI statistics via the STAC NDVI collections (ndvi-lter / ndvi-snl)
# These collections hold per-polygon NDVI summary stats (vector features),
# observed irregularly (cloud-cover dependent). We aggregate to MONTHLY values
# so the Statistics output lines up with the monthly NDVI rasters in Geodata.

# Decide which NDVI collection a selected project area belongs to, based on the
# sequential source name assigned when the project was loaded. Returns
# "ndvi-lter", "ndvi-snl", or NA (for non-project polygons: uploads / drawn).
detect_ndvi_collection <- function(source_name) {
  if (is.null(source_name) || is.na(source_name) || !nzchar(source_name)) return(NA_character_)
  # SNL parcels were named "SNL parcel_N"
  if (grepl("^SNL parcel(_\\d+)?$", source_name)) return("ndvi-snl")
  # LTER classes were named after the 4 project groups
  if (grepl(paste0("^(", paste(lter_class_levels, collapse = "|"), ")(_\\d+)?$"), source_name)) return("ndvi-lter")
  NA_character_
}

# Fetch NDVI stats for a region of interest from a given collection, restricted
# to a date range, and aggregate to one row per polygon (ndc_id) per month.
# Returns list(status, data, error). Only fields actually present in the
# collection output are used (ndvi_mean, ndvi_std, observation_date, ndc_id);
# each is guarded so a missing field degrades gracefully instead of erroring.
fetch_ndvi_stats_monthly <- function(roi_sf, collection, date_from, date_to) {
  if (is.null(roi_sf)) {
    return(list(status = "error", data = NULL, error = "No region of interest."))
  }
  if (is.null(collection) || is.na(collection)) {
    return(list(status = "skip", data = NULL,
                error = "NDVI statistics are only available for LTER and SNL project areas."))
  }

  roi_sf <- tryCatch({
    g <- roi_sf
    if (is.na(sf::st_crs(g))) sf::st_crs(g) <- 4326
    sf::st_transform(g, 4326)
  }, error = function(e) NULL)
  if (is.null(roi_sf)) {
    return(list(status = "error", data = NULL, error = "Could not prepare region of interest."))
  }

  stats <- tryCatch(
    rNDC::get_ndvi_stats(roi_sf, collection,
                         from = if (is.null(date_from)) NA else date_from,
                         to = if (is.null(date_to)) NA else date_to),
    error = function(e) structure("ndc_error", message = conditionMessage(e))
  )
  if (is.character(stats) && identical(as.character(stats), "ndc_error")) {
    return(list(status = "error", data = NULL, error = attr(stats, "message")))
  }
  if (nrow(stats) == 0) return(list(status = "empty", data = NULL, error = NULL))

  # One polygon: leave out its id, and the standard deviation if the collection does not have it
  data <- as.data.frame(stats[, intersect(c("month", "ndvi_mean", "ndvi_std"), names(stats))])
  list(status = "ok", data = data, error = NULL)
}
