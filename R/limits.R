# Limits on the polygons that users bring (drawn or uploaded), and the polygon that goes to AgroDataCube.

# The largest area (km2) of a polygon for which datasets can be added (not for Weather, which only uses the
# centroid). A huge area means a huge download. Set another with the environment variable NDC_MAX_AREA_KM2, or the
# option `rNDC.Shiny.max_area_km2`.
default_max_area_km2 <- 1000
max_area_km2 <- function() getOption("rNDC.Shiny.max_area_km2", default_max_area_km2)

# The largest number of features that one upload (or layer) may have: every feature is a shape on the map and a
# polygon to select.
max_upload_features <- 1000L

# What a zip file may hold: a zip with more files, or that unpacks to more, is not read (a small zip can unpack to
# gigabytes).
max_zip_files <- 200L
max_zip_unpacked_mb <- 500

# The datasets whose retrieval depends on the area of the polygon
area_limited_datasets <- function() setdiff(all_dataset_names, "Weather")

# Area in km2 of each polygon of an sf object (in the Dutch grid, EPSG:28992)
polygon_area_km2 <- function(poly) {
  as.numeric(sf::st_area(sf::st_transform(sf::st_geometry(poly), 28992))) / 1e6
}

# The names of the polygons of `sel` (an sf object with a `source_name`) that are above the area limit
polygons_too_large <- function(sel, limit = max_area_km2()) {
  if (is.null(sel) || nrow(sel) == 0) return(character(0))
  names <- if ("source_name" %in% names(sel)) as.character(sel$source_name) else rep("Own polygon", nrow(sel))
  names[polygon_area_km2(sel) > limit]
}

too_large_message <- function(names, limit = max_area_km2()) {
  paste0("Not added: ", if (length(names) == 1) "the polygon " else "the polygons ",
         paste(names, collapse = ", "), if (length(names) == 1) " is" else " are",
         " larger than the limit of ", format(limit, big.mark = ","), " km\u00b2 for this dataset. ",
         "Choose a smaller area.")
}

# ---- The polygon in an AgroDataCube request ----

# AgroDataCube takes the polygon in the URL, and its server refuses URLs longer than about 8000 characters (a
# polygon of 300 vertices, 5600 characters of WKT and 7500 of URL, works; 500 vertices fails). The WKT may have at
# most this many characters, which leaves room for the encoding (about 1.35 times as long) and the other
# parameters.
adc_max_wkt_chars <- 5000L

# The WKT of the polygon in `row` for AgroDataCube: list(wkt, tolerance). Normally the WKT of the row. If that is
# too long, the polygon is written with 6 decimals (about 0.1 m) and, if needed, simplified in the Dutch grid with
# a tolerance in metres (0 when it was not simplified). Stops if even that is not enough.
adc_geometry <- function(row, max_chars = adc_max_wkt_chars) {
  if (nchar(row$wkt) <= max_chars) return(list(wkt = row$wkt, tolerance = 0))

  geom <- sf::st_geometry(row$polygon_sf[[1]])
  wkt <- sf::st_as_text(geom, digits = 6)
  if (length(wkt) == 1 && nchar(wkt) <= max_chars) return(list(wkt = wkt, tolerance = 0))

  projected <- sf::st_transform(geom, 28992)
  for (tolerance in c(1, 2, 5, 10, 20, 50, 100, 200, 500, 1000)) {
    simple <- sf::st_transform(sf::st_simplify(projected, dTolerance = tolerance, preserveTopology = TRUE), 4326)
    if (all(sf::st_is_empty(simple))) break
    wkt <- sf::st_as_text(simple, digits = 6)
    if (length(wkt) == 1 && nchar(wkt) <= max_chars) return(list(wkt = wkt, tolerance = tolerance))
  }
  stop("the polygon is too complex for AgroDataCube, even simplified (the limit is ", max_chars,
       " characters of WKT)", call. = FALSE)
}

# A note for the message of a retrieved dataset when the polygon had to be simplified
simplified_note <- function(geometry) {
  if (geometry$tolerance > 0) {
    paste0(" (polygon simplified by up to ", geometry$tolerance, " m to fit the AgroDataCube request)")
  } else {
    ""
  }
}
