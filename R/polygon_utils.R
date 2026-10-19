# Helpers for polygons: WKT columns, source names and ids, drawn polygons, the colours of uploads, and the limits on the polygons that users bring.

polygon_add_wkt <- function(sf_obj, col_name = "wkt") {
  sf_obj <- sf::st_transform(sf_obj, 4326)
  sf_obj[[col_name]] <- sf::st_as_text(sf::st_geometry(sf_obj))
  sf_obj
}

polygon_assign_names <- function(sf_obj, base_name, overview_df, fixed_df, uploaded_df, drawn_df) {
  if (is.null(sf_obj) || nrow(sf_obj) == 0) return(sf_obj)

  existing <- character(0)
  if (!is.null(overview_df) && nrow(overview_df) > 0) existing <- c(existing, as.character(overview_df$polygon))
  if (!is.null(fixed_df) && nrow(fixed_df) > 0 && "source_name" %in% names(fixed_df)) existing <- c(existing, as.character(fixed_df$source_name))
  if (!is.null(uploaded_df) && nrow(uploaded_df) > 0 && "source_name" %in% names(uploaded_df)) existing <- c(existing, as.character(uploaded_df$source_name))
  if (!is.null(drawn_df) && nrow(drawn_df) > 0 && "source_name" %in% names(drawn_df)) existing <- c(existing, as.character(drawn_df$source_name))
  existing <- existing[!is.na(existing) & nzchar(existing)]

  esc_base <- gsub("([\\W])", "\\\\\\1", base_name)
  parse_index <- function(name) {
    if (identical(name, base_name)) return(1L)
    m <- regmatches(name, regexec(paste0("^", esc_base, "_(\\d+)$"), name))[[1]]
    if (length(m) == 2) return(as.integer(m[2]))
    0L
  }

  max_idx <- if (length(existing) == 0) 0L else max(vapply(existing, parse_index, integer(1)), na.rm = TRUE)
  start_idx <- if (max_idx >= 1L) max_idx + 1L else 1L

  # Always suffix the index, including _1 on the very first polygon.
  out_names <- paste0(base_name, "_", seq.int(from = start_idx, length.out = nrow(sf_obj)))

  sf_obj$source_name <- out_names
  sf_obj
}

# The layer id for the next polygon: one more than the largest id in the given sf objects (NULL is none)
polygon_next_id <- function(...) {
  ids <- unlist(lapply(list(...), function(x) if (is.null(x) || nrow(x) == 0) integer(0) else as.integer(x$layer_id)))
  max(c(ids, 0L), na.rm = TRUE) + 1L
}

# Number the features of a new upload: each gets its own `layer_id` (the id of its shape on the map), and all
# share an `upload_id`, which identifies the upload (a file, or a layer of a GeoPackage) in the list of uploads
# and when it is removed. `existing` is the sf object of the uploads so far (or NULL).
polygon_number_uploaded <- function(new, existing = NULL) {
  last <- function(column) {
    if (is.null(existing) || nrow(existing) == 0 || !column %in% names(existing)) return(0L)
    max(as.integer(existing[[column]]), 0L, na.rm = TRUE)
  }
  new$layer_id <- last("layer_id") + seq_len(nrow(new))
  new$upload_id <- last("upload_id") + 1L
  new
}

polygon_from_drawing <- function(feat, start_layer_id = 1) {
  if (is.null(feat) || is.null(feat$geometry) || feat$geometry$type != "Polygon") return(NULL)
  coords <- feat$geometry$coordinates[[1]]
  coords <- do.call(rbind, lapply(coords, function(x) as.numeric(unlist(x))))
  sf_obj <- sf::st_as_sf(sf::st_sfc(sf::st_polygon(list(coords)), crs = 4326))
  # the toolbar allows a polygon to cross itself: repair it (the note, if any, is the attribute "notes")
  sf_obj <- polygon_clean(sf_obj, "The polygon")
  if (nrow(sf_obj) == 0) return(sf_obj)  # nothing left of it: the caller says why (the notes)
  notes <- polygon_notes(sf_obj)
  sf_obj <- sf_obj[1, ]
  attr(sf_obj, "notes") <- notes
  sf_obj$wkt <- sf::st_as_text(sf::st_geometry(sf_obj))
  sf_obj$layer_id <- as.integer(start_layer_id)
  sf_obj
}

polygon_from_geojson <- function(feat) {
  if (is.null(feat) || is.null(feat$geometry) || feat$geometry$type != "Polygon") return(NULL)
  coords <- feat$geometry$coordinates[[1]]
  coords <- do.call(rbind, lapply(coords, function(x) as.numeric(unlist(x))))
  sf_obj <- sf::st_as_sf(sf::st_sfc(sf::st_polygon(list(coords)), crs = 4326))
  sf_obj$wkt <- sf::st_as_text(sf::st_geometry(sf_obj))
  sf_obj$layer_id <- if (!is.null(feat$properties) && !is.null(feat$properties$layerId)) as.integer(feat$properties$layerId) else NA_integer_
  sf_obj
}

polygon_upload_colors <- function(up_sf, drawn_features_val = NULL, fixed_polys_val = NULL) {
  if (is.null(up_sf) || nrow(up_sf) == 0) return(up_sf)
  up_sf$color <- "#2b8cbe"
  has <- function(x) !is.null(x) && nrow(x) > 0
  if (!has(drawn_features_val) && !has(fixed_polys_val)) return(up_sf)  # nothing to overlap with
  layer_ids <- unique(up_sf$layer_id)
  for (lid in layer_ids) {
    idxs <- which(up_sf$layer_id == lid)
    col <- "#2b8cbe"
    if (!is.null(drawn_features_val) && nrow(drawn_features_val) > 0) {
      ints_drawn <- sf::st_intersects(up_sf[idxs, ], drawn_features_val, sparse = FALSE)
      if ((is.matrix(ints_drawn) && any(ints_drawn)) || (is.logical(ints_drawn) && any(ints_drawn))) col <- "#444444"
    }
    if (!is.null(fixed_polys_val) && nrow(fixed_polys_val) > 0) {
      ints_fixed <- sf::st_intersects(up_sf[idxs, ], fixed_polys_val, sparse = FALSE)
      if ((is.matrix(ints_fixed) && any(ints_fixed)) || (is.logical(ints_fixed) && any(ints_fixed))) col <- "black"
    }
    up_sf$color[idxs] <- col
  }
  up_sf
}

polygon_zoom_to <- function(sf_obj) {
  if (is.null(sf_obj) || nrow(sf_obj) == 0) return(invisible(NULL))
  bb <- sf::st_bbox(sf::st_transform(sf_obj, 4326))
  leaflet::leafletProxy("map") %>%
    leaflet::fitBounds(
      as.numeric(bb["xmin"]),
      as.numeric(bb["ymin"]),
      as.numeric(bb["xmax"]),
      as.numeric(bb["ymax"])
    )
}

# ---- Limits on the polygons that users bring ----

# Limits on the polygons that users bring (drawn or uploaded).

# The largest area (km2) of a polygon for which datasets can be added (not for Weather, which only uses the
# centroid). A huge area means a huge download. Set another with the environment variable NDC_MAX_AREA_KM2, or the
# option `rNDC.Shiny.max_area_km2`.
default_max_area_km2 <- 2000

max_area_km2 <- function() getOption("rNDC.Shiny.max_area_km2", default_max_area_km2)

# The largest number of features that one upload (or layer) may have: every feature is a shape on the map and a
# polygon to select.
max_upload_features <- 1000L

# What a zip file may hold: a zip with more files, or that unpacks to more, is not read (a small zip can unpack to
# gigabytes).
max_zip_files <- 200L

max_zip_unpacked_mb <- 500

# The datasets whose retrieval depends on the area of the polygon
polygon_limited_datasets <- function() setdiff(all_dataset_names, "Weather")

# Area in km2 of each polygon of an sf object (in the Dutch grid, EPSG:28992)
polygon_area_km2 <- function(poly) {
  as.numeric(sf::st_area(sf::st_transform(sf::st_geometry(poly), 28992))) / 1e6
}

# The names of the polygons of `sel` (an sf object with a `source_name`) that are above the area limit
polygon_too_large <- function(sel, limit = max_area_km2()) {
  if (is.null(sel) || nrow(sel) == 0) return(character(0))
  names <- if ("source_name" %in% names(sel)) as.character(sel$source_name) else rep("Own polygon", nrow(sel))
  names[polygon_area_km2(sel) > limit]
}

polygon_too_large_message <- function(names, limit = max_area_km2()) {
  paste0("Not added: ", if (length(names) == 1) "the polygon " else "the polygons ",
         paste(names, collapse = ", "), if (length(names) == 1) " is" else " are",
         " larger than the limit of ", format(limit, big.mark = ","), " km\u00b2 for this dataset. ",
         "Choose a smaller area.")
}
