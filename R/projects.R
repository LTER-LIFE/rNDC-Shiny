# Project layers (LTER, SNL) and NDVI statistics from the NatureDataCube STAC API.

classify_lter <- function(sf_obj) {
  if (is.null(sf_obj) || nrow(sf_obj) == 0 || !("name" %in% names(sf_obj))) return(sf_obj)
  nm <- sf_obj[["name"]]
  sf_obj$project_class <- dplyr::case_when(
    grepl("^Lantaarnpaal", nm, ignore.case = TRUE) ~ "Light on Nature",
    grepl("^Loobos$",      nm, ignore.case = TRUE) ~ "Loobos",
    grepl("Nestkast",      nm, ignore.case = TRUE) ~ "Nestboxes",
    grepl("^Nutnet$",      nm, ignore.case = TRUE) ~ "Nutnet",
    TRUE ~ NA_character_
  )
  sf_obj
}

# Fetch the whole LTER collection once and classify it. Returns
# list(data, error): `data` is an sf object (transformed to 4326) or NULL, and
# `error` the failure message (NULL on success or when the collection is empty).
fetch_lter_classified <- function() {
  err <- NULL
  out <- tryCatch(
    ndc_get_all_sf(collection = ndc_lter_collection),
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
  # Build a proper sf polygon (with an explicit CRS) for the viewport rectangle
  # and pass THAT as the RoI. ndc_roi() handles sf objects cleanly; passing a
  # bare numeric vector goes through st_bbox.numeric() which (a) needs names and
  # (b) yields a bbox with NA crs that then errors on the internal st_transform.
  roi_poly <- tryCatch(
    sf::st_as_sf(
      sf::st_as_sfc(
        sf::st_bbox(c(xmin = unname(bbox[1]), ymin = unname(bbox[2]),
                      xmax = unname(bbox[3]), ymax = unname(bbox[4])),
                    crs = sf::st_crs(4326))
      )
    ),
    error = function(e) NULL
  )
  if (is.null(roi_poly)) {
    return(list(status = "error", data = NULL, error = "Could not build viewport polygon."))
  }
  out <- tryCatch(
    rNDC::ndc_get(
      collection = ndc_snl_collection,
      roi        = roi_poly,
      mode       = "sf",
      limit      = snl_fetch_limit
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
  if (grepl(paste0("^(", paste(c("Light on Nature", "Loobos", "Nestboxes", "Nutnet"),
                                collapse = "|"), ")(_\\d+)?$"), source_name)) return("ndvi-lter")
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

  # Build a temporal range string for the STAC query (if dates are available).
  trange <- NULL
  if (!is.null(date_from) && !is.na(date_from) && !is.null(date_to) && !is.na(date_to)) {
    trange <- tryCatch(
      # date_to is a whole day: ndc_trange() turns a date into 00:00:00Z, which
      # would drop that day's observations, so extend the end to 23:59:59.
      rNDC::ndc_trange(c(lubridate::as_datetime(date_from),
                         lubridate::as_datetime(date_to) + 86399)),
      error = function(e) NULL
    )
  }

  out <- tryCatch({
    ndc_get_all_sf(collection = collection, roi = roi_sf, trange = trange)
  }, error = function(e) structure("ndc_error", message = conditionMessage(e)))

  if (is.character(out) && identical(as.character(out), "ndc_error")) {
    return(list(status = "error", data = NULL, error = attr(out, "message")))
  }
  if (is.null(out) || nrow(out) == 0) {
    return(list(status = "empty", data = NULL, error = NULL))
  }

  # The query returns every NDVI feature that INTERSECTS the polygon, which for
  # SNL parcels includes the neighbouring parcels. Project polygons carry the
  # `ndc_id` that also identifies their NDVI features: keep only those.
  roi_ids <- if ("ndc_id" %in% names(roi_sf)) unique(as.character(roi_sf$ndc_id)) else character(0)
  roi_ids <- roi_ids[!is.na(roi_ids)]
  if (length(roi_ids) > 0 && "ndc_id" %in% names(out)) {
    out <- out[as.character(out$ndc_id) %in% roi_ids, , drop = FALSE]
    if (nrow(out) == 0) return(list(status = "empty", data = NULL, error = NULL))
  }

  # Drop geometry: statistics output is a plain table.
  df <- tryCatch(sf::st_drop_geometry(out), error = function(e) as.data.frame(out))

  if (!("observation_date" %in% names(df))) {
    return(list(status = "error", data = NULL,
                error = "Expected field 'observation_date' not found in NDVI collection."))
  }
  if (!("ndvi_mean" %in% names(df))) {
    return(list(status = "error", data = NULL,
                error = "Expected field 'ndvi_mean' not found in NDVI collection."))
  }

  df$month <- substr(as.character(df$observation_date), 1, 7)  # "YYYY-MM"
  # Aggregate per sub-polygon (ndc_id) and month if ndc_id is present, so values
  # are correct even if the area maps to multiple ndc_id features; the ndc_id
  # column itself is dropped from the final output below.
  group_cols <- intersect(c("ndc_id", "month"), names(df))
  if (!("month" %in% group_cols)) group_cols <- "month"

  has_std <- "ndvi_std" %in% names(df)

  agg <- df |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarise(
      ndvi_mean = mean(suppressWarnings(as.numeric(ndvi_mean)), na.rm = TRUE),
      ndvi_std  = if (has_std) mean(suppressWarnings(as.numeric(ndvi_std)), na.rm = TRUE) else NA_real_,
      .groups = "drop"
    ) |>
    dplyr::arrange(month)

  agg <- as.data.frame(agg)
  if (!has_std) agg$ndvi_std <- NULL          # don't show a column the source didn't provide
  if ("ndc_id" %in% names(agg)) agg$ndc_id <- NULL  # requested: leave out ndc_id

  list(status = "ok", data = agg, error = NULL)
}
