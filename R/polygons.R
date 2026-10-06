# Helpers for polygons: WKT columns, drawn/uploaded geometries, source names and map zoom.

add_wkt_column <- function(sf_obj, col_name = "wkt") {
  sf_obj <- sf::st_transform(sf_obj, 4326)
  sf_obj[[col_name]] <- sf::st_as_text(sf::st_geometry(sf_obj))
  sf_obj
}

assign_sequential_source_names <- function(sf_obj, base_name, overview_df, fixed_df, uploaded_df, drawn_df) {
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

convert_drawn_to_sf <- function(feat, start_layer_id = 1) {
  if (is.null(feat) || is.null(feat$geometry) || feat$geometry$type != "Polygon") return(NULL)
  coords <- feat$geometry$coordinates[[1]]
  coords <- do.call(rbind, lapply(coords, function(x) as.numeric(unlist(x))))
  sf_obj <- sf::st_as_sf(sf::st_sfc(sf::st_polygon(list(coords)), crs = 4326))
  sf_obj$wkt <- sf::st_as_text(sf::st_geometry(sf_obj))
  sf_obj$layer_id <- as.integer(start_layer_id)
  sf_obj
}

convert_geojson_feature_to_sf <- function(feat) {
  if (is.null(feat) || is.null(feat$geometry) || feat$geometry$type != "Polygon") return(NULL)
  coords <- feat$geometry$coordinates[[1]]
  coords <- do.call(rbind, lapply(coords, function(x) as.numeric(unlist(x))))
  sf_obj <- sf::st_as_sf(sf::st_sfc(sf::st_polygon(list(coords)), crs = 4326))
  sf_obj$wkt <- sf::st_as_text(sf::st_geometry(sf_obj))
  sf_obj$layer_id <- if (!is.null(feat$properties) && !is.null(feat$properties$layerId)) as.integer(feat$properties$layerId) else NA_integer_
  sf_obj
}

read_polygons_from_path <- function(path, layer = NULL) {
  sf_obj <- tryCatch({
    if (!is.null(layer)) sf::st_read(path, layer = layer, quiet = TRUE) else sf::st_read(path, quiet = TRUE)
  }, error = function(e) NULL)
  if (is.null(sf_obj)) return(NULL)
  poly_only <- sf_obj[sf::st_is(sf_obj, c("POLYGON", "MULTIPOLYGON")), , drop = FALSE]
  if (nrow(poly_only) == 0) return(NULL)
  if (is.na(sf::st_crs(poly_only))) sf::st_crs(poly_only) <- 4326
  sf::st_transform(poly_only, 4326)
}

process_uploaded_files <- function(files_df, start_layer_id = 1) {
  if (is.null(files_df) || nrow(files_df) == 0) return(list(imported = NULL, pending_gpkg = NULL))

  imported_list <- list()
  pending_gpkg <- list()

  read_uploaded_shapefile_bundle <- function(files_df, idx) {
    shp_name <- files_df$name[idx]
    base <- tools::file_path_sans_ext(basename(shp_name))
    same_base <- tools::file_path_sans_ext(basename(files_df$name)) == base
    bundle <- files_df[same_base, , drop = FALSE]
    exts <- tolower(tools::file_ext(bundle$name))

    required <- c("shp", "shx", "dbf")
    missing <- setdiff(required, exts)
    if (length(missing) > 0) {
      showNotification(
        paste0("Shapefile upload incomplete for '", shp_name, "'. Missing: ", paste(missing, collapse = ", "), ". Please upload .shp, .shx, .dbf (and .prj if available)."),
        type = "error", duration = 8
      )
      return(NULL)
    }

    tmp <- tempfile("shp_bundle_")
    dir.create(tmp)
    for (j in seq_len(nrow(bundle))) {
      file.copy(bundle$datapath[j], file.path(tmp, basename(bundle$name[j])), overwrite = TRUE)
    }

    shp_path <- file.path(tmp, paste0(base, ".shp"))
    sf_obj <- tryCatch(sf::st_read(shp_path, quiet = TRUE), error = function(e) NULL)
    if (is.null(sf_obj)) return(NULL)

    poly_only <- sf_obj[sf::st_is(sf_obj, c("POLYGON", "MULTIPOLYGON")), , drop = FALSE]
    if (nrow(poly_only) == 0) return(NULL)
    if (is.na(sf::st_crs(poly_only))) sf::st_crs(poly_only) <- 4326
    sf::st_transform(poly_only, 4326)
  }

  for (i in seq_len(nrow(files_df))) {
    f <- files_df[i, ]
    fname <- f$name
    datapath <- f$datapath
    ext <- tolower(tools::file_ext(fname))

    if (ext == "zip") {
      tmp <- tempfile("unzip_")
      dir.create(tmp)
      utils::unzip(datapath, exdir = tmp)

      gpkg_files    <- list.files(tmp, pattern = "\\.gpkg$", full.names = TRUE, ignore.case = TRUE)
      shp_files     <- list.files(tmp, pattern = "\\.shp$", full.names = TRUE, ignore.case = TRUE)
      geojson_files <- list.files(tmp, pattern = "\\.(geojson|json)$", full.names = TRUE, ignore.case = TRUE)
      kml_files     <- list.files(tmp, pattern = "\\.kml$", full.names = TRUE, ignore.case = TRUE)

      if (length(gpkg_files) > 0) {
        for (g in gpkg_files) {
          lyr_info <- tryCatch(sf::st_layers(g), error = function(e) NULL)
          if (!is.null(lyr_info) && length(lyr_info$name) > 1) {
            pending_gpkg[[length(pending_gpkg) + 1]] <- list(
              name = paste0(fname, " -> ", basename(g)),
              datapath = g,
              layers = lyr_info$name,
              original_name = fname
            )
          } else {
            sf_obj <- read_polygons_from_path(g)
            if (!is.null(sf_obj)) {
              sf_obj$source_name <- fname
              imported_list[[length(imported_list) + 1]] <- sf_obj
            }
          }
        }
      } else if (length(shp_files) > 0) {
        sf_obj <- read_polygons_from_path(shp_files[1])
        if (!is.null(sf_obj)) {
          sf_obj$source_name <- fname
          imported_list[[length(imported_list) + 1]] <- sf_obj
        }
      } else if (length(geojson_files) > 0) {
        sf_obj <- read_polygons_from_path(geojson_files[1])
        if (!is.null(sf_obj)) {
          sf_obj$source_name <- fname
          imported_list[[length(imported_list) + 1]] <- sf_obj
        }
      } else if (length(kml_files) > 0) {
        lyr_info <- tryCatch(sf::st_layers(kml_files[1]), error = function(e) NULL)
        if (!is.null(lyr_info) && length(lyr_info$name) > 0) {
          sf_obj <- tryCatch(sf::st_read(kml_files[1], layer = lyr_info$name[1], quiet = TRUE), error = function(e) NULL)
          sf_obj <- if (!is.null(sf_obj)) sf_obj[sf::st_is(sf_obj, c("POLYGON", "MULTIPOLYGON")), , drop = FALSE] else NULL
          if (!is.null(sf_obj) && nrow(sf_obj) > 0) {
            sf_obj <- sf::st_transform(sf_obj, 4326)
            sf_obj$source_name <- fname
            imported_list[[length(imported_list) + 1]] <- sf_obj
          }
        }
      }

    } else if (ext == "gpkg") {
      lyr_info <- tryCatch(sf::st_layers(datapath), error = function(e) NULL)
      if (!is.null(lyr_info) && length(lyr_info$name) > 1) {
        pending_gpkg[[length(pending_gpkg) + 1]] <- list(
          name = fname,
          datapath = datapath,
          layers = lyr_info$name,
          original_name = fname
        )
      } else {
        sf_obj <- read_polygons_from_path(datapath)
        if (!is.null(sf_obj)) {
          sf_obj$source_name <- fname
          imported_list[[length(imported_list) + 1]] <- sf_obj
        }
      }

    } else if (ext == "shp") {
      sf_obj <- read_uploaded_shapefile_bundle(files_df, i)
      if (!is.null(sf_obj)) {
        sf_obj$source_name <- fname
        imported_list[[length(imported_list) + 1]] <- sf_obj
      }

    } else if (ext %in% c("geojson", "json", "kml")) {
      sf_obj <- read_polygons_from_path(datapath)
      if (!is.null(sf_obj)) {
        sf_obj$source_name <- fname
        imported_list[[length(imported_list) + 1]] <- sf_obj
      }
    }
  }

  imported_sf <- NULL
  if (length(imported_list) > 0) imported_sf <- dplyr::bind_rows(imported_list)
  list(imported = imported_sf, pending_gpkg = pending_gpkg)
}

assign_uploaded_colors <- function(up_sf, drawn_features_val = NULL, fixed_polys_val = NULL) {
  if (is.null(up_sf) || nrow(up_sf) == 0) return(up_sf)
  up_sf$color <- "#2b8cbe"
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

same_na <- function(x, y) {
  (is.na(x) && is.na(y)) || (!is.na(x) && !is.na(y) && identical(x, y))
}

safe_filename <- function(x) gsub("[^A-Za-z0-9_\\-]+", "_", x)

zoom_to_sf <- function(sf_obj) {
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
