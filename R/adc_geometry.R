# The polygon in an AgroDataCube request: its server refuses URLs above about 8000 characters.

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
adc_simplified_note <- function(geometry) {
  if (geometry$tolerance > 0) {
    paste0(" (polygon simplified by up to ", geometry$tolerance, " m to fit the AgroDataCube request)")
  } else {
    ""
  }
}
