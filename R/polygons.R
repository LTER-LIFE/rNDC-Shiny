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

# The layer id for the next polygon: one more than the largest id in the given sf objects (NULL is none)
next_layer_id <- function(...) {
  ids <- unlist(lapply(list(...), function(x) if (is.null(x) || nrow(x) == 0) integer(0) else as.integer(x$layer_id)))
  max(c(ids, 0L), na.rm = TRUE) + 1L
}

# Number the features of a new upload: each gets its own `layer_id` (the id of its shape on the map), and all
# share an `upload_id`, which identifies the upload (a file, or a layer of a GeoPackage) in the list of uploads
# and when it is removed. `existing` is the sf object of the uploads so far (or NULL).
number_uploaded <- function(new, existing = NULL) {
  last <- function(column) {
    if (is.null(existing) || nrow(existing) == 0 || !column %in% names(existing)) return(0L)
    max(as.integer(existing[[column]]), 0L, na.rm = TRUE)
  }
  new$layer_id <- last("layer_id") + seq_len(nrow(new))
  new$upload_id <- last("upload_id") + 1L
  new
}

convert_drawn_to_sf <- function(feat, start_layer_id = 1) {
  if (is.null(feat) || is.null(feat$geometry) || feat$geometry$type != "Polygon") return(NULL)
  coords <- feat$geometry$coordinates[[1]]
  coords <- do.call(rbind, lapply(coords, function(x) as.numeric(unlist(x))))
  sf_obj <- sf::st_as_sf(sf::st_sfc(sf::st_polygon(list(coords)), crs = 4326))
  # the toolbar allows a polygon to cross itself: repair it (the note, if any, is the attribute "notes")
  sf_obj <- clean_polygons(sf_obj, "The polygon")
  if (nrow(sf_obj) == 0) return(sf_obj)  # nothing left of it: the caller says why (the notes)
  notes <- polygon_notes(sf_obj)
  sf_obj <- sf_obj[1, ]
  attr(sf_obj, "notes") <- notes
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

# Is the coordinate system of `x` missing? Files without one come as NA, or, from a GeoPackage, as an "undefined"
# engineering system that cannot be transformed.
crs_missing <- function(x) {
  crs <- sf::st_crs(x)
  is.na(crs) || grepl("Undefined (Cartesian|geographic) SRS", crs$wkt)
}

# Make polygons from an sf object as the app wants them: only (multi)polygons, without Z/M, in EPSG:4326, and
# valid (a self-intersecting polygon is repaired; one that cannot be is dropped). Returns NULL if there was no polygon
# at all, and an object without rows if none was left after the repair.
# What was done that the user should know goes in the attribute "notes" (a character vector, see `polygon_notes()`).
# `label` names the object in the notes (e.g. the file).
clean_polygons <- function(x, label = "The polygons") {
  notes <- character(0)
  x <- x[sf::st_is(x, c("POLYGON", "MULTIPOLYGON")), , drop = FALSE]
  if (nrow(x) == 0) return(NULL)

  if (crs_missing(x)) {
    x <- suppressWarnings(sf::st_set_crs(x, 4326))  # (replacing an "undefined" system warns)
    notes <- c(notes, paste0(label, " has no coordinate system: WGS84 (longitude, latitude) is assumed."))
  }
  x <- sf::st_transform(sf::st_zm(x, drop = TRUE), 4326)

  # Validity is checked and repaired in the Dutch grid (a plane): for longitude and latitude sf works on the
  # sphere, where the repair of a self-intersecting polygon gives a wrong, still invalid result.
  grid <- tryCatch(sf::st_transform(sf::st_geometry(x), 28992), error = function(e) NULL)
  if (!is.null(grid)) {
    valid <- tryCatch(sf::st_is_valid(grid), error = function(e) rep(NA, length(grid)))
    invalid <- which(is.na(valid) | !valid)
    if (length(invalid) > 0) {
      fixed <- tryCatch({
        repaired <- sf::st_make_valid(grid[invalid])
        # a collection (polygons and lines, say) keeps its polygons
        collection <- as.character(sf::st_geometry_type(repaired)) == "GEOMETRYCOLLECTION"
        if (any(collection)) {
          repaired[collection] <- suppressWarnings(sf::st_collection_extract(repaired[collection], "POLYGON"))
        }
        repaired
      }, error = function(e) NULL)
      if (is.null(fixed)) {
        keep <- setdiff(seq_len(nrow(x)), invalid)
        notes <- c(notes, paste0(label, ": ", length(invalid), " invalid polygon(s) could not be repaired and were left out."))
      } else {
        geometry <- sf::st_geometry(x)
        geometry[invalid] <- sf::st_transform(fixed, 4326)
        sf::st_geometry(x) <- geometry
        empty <- sf::st_is_empty(x)
        keep <- which(!empty)
        notes <- c(notes, paste0(label, ": ", length(invalid), " invalid polygon(s) (e.g. self-intersecting) repaired",
                                 if (any(empty)) paste0(", ", sum(empty), " left out because nothing was left of them") else "", "."))
      }
      x <- x[keep, , drop = FALSE]  # possibly none: then an sf object without rows, which still has its notes
    }
  }
  attr(x, "notes") <- notes
  x
}

# The notes that `clean_polygons()` left on its result
polygon_notes <- function(x) {
  notes <- attr(x, "notes")
  if (is.null(notes)) character(0) else notes
}

read_polygons_from_path <- function(path, layer = NULL, label = basename(path)) {
  sf_obj <- tryCatch({
    if (!is.null(layer)) sf::st_read(path, layer = layer, quiet = TRUE) else sf::st_read(path, quiet = TRUE)
  }, error = function(e) NULL)
  if (is.null(sf_obj)) return(NULL)
  clean_polygons(sf_obj, label)
}

# Folders for unpacking uploads, which are removed when `process_uploaded_files()` is done, except the ones with a
# GeoPackage that waits for a layer to be chosen.
upload_tempdir <- function(prefix) {
  dir <- tempfile(prefix)
  dir.create(dir)
  dir
}

# What is wrong with a zip file, from its listing (`utils::unzip(list = TRUE)`: Name and Length), or NULL: too
# many files, too large when unpacked (a zip bomb), or paths that would be written outside the folder.
zip_problem <- function(listing, max_files = max_zip_files, max_mb = max_zip_unpacked_mb) {
  if (nrow(listing) > max_files) {
    return(paste0("it has ", nrow(listing), " files (at most ", max_files, " are accepted)"))
  }
  if (sum(as.numeric(listing$Length)) > max_mb * 1024^2) {
    return(paste0("it is larger than ", max_mb, " MB when unpacked"))
  }
  if (any(grepl("(^|[/\\\\])\\.\\.([/\\\\]|$)|^[/\\\\]|^[A-Za-z]:", listing$Name))) {
    return("it has files with paths outside the archive")
  }
  NULL
}

# Read the polygon files that were uploaded (`files_df`: name and datapath, as Shiny's fileInput gives them).
# Returns the `imported` polygons (an sf object with a `source_name`, NULL if none), the GeoPackages that wait for a
# layer to be chosen (`pending_gpkg`), the `notes` for the user (what was assumed, repaired or refused) and the
# folders (`kept_dirs`) that hold the waiting GeoPackages, which the caller removes when the session ends.
process_uploaded_files <- function(files_df) {
  if (is.null(files_df) || nrow(files_df) == 0) return(list(imported = NULL, pending_gpkg = NULL))

  imported_list <- list()
  pending_gpkg <- list()
  notes <- character(0)
  dirs <- character(0)
  # the folders that hold a GeoPackage that waits for a layer to be chosen
  kept_dirs <- function() {
    paths <- vapply(pending_gpkg, function(p) p$datapath, "")
    dirs[vapply(dirs, function(d) any(startsWith(paths, paste0(d, "/"))), NA)]
  }
  on.exit(unlink(setdiff(dirs, kept_dirs()), recursive = TRUE))

  # an object that was read: keep its polygons, and what we have to say about them
  add_polygons <- function(sf_obj, fname) {
    if (is.null(sf_obj)) return(invisible(NULL))
    notes <<- c(notes, polygon_notes(sf_obj))
    if (nrow(sf_obj) == 0) return(invisible(NULL))
    sf_obj$source_name <- fname
    imported_list[[length(imported_list) + 1]] <<- sf_obj
    invisible(NULL)
  }

  # a GeoPackage: with several layers it waits for the user to choose one
  add_gpkg <- function(path, fname, label) {
    lyr_info <- tryCatch(sf::st_layers(path), error = function(e) NULL)
    if (!is.null(lyr_info) && length(lyr_info$name) > 1) {
      pending_gpkg[[length(pending_gpkg) + 1]] <<- list(name = label, datapath = path, layers = lyr_info$name,
                                                        original_name = fname)
    } else {
      add_polygons(read_polygons_from_path(path, label = fname), fname)
    }
  }

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

    tmp <- upload_tempdir("shp_bundle_")
    dirs <<- c(dirs, tmp)
    for (j in seq_len(nrow(bundle))) {
      file.copy(bundle$datapath[j], file.path(tmp, basename(bundle$name[j])), overwrite = TRUE)
    }
    read_polygons_from_path(file.path(tmp, paste0(base, ".shp")), label = shp_name)
  }

  for (i in seq_len(nrow(files_df))) {
    fname <- files_df$name[i]
    datapath <- files_df$datapath[i]
    ext <- tolower(tools::file_ext(fname))

    if (ext == "zip") {
      problem <- tryCatch(zip_problem(utils::unzip(datapath, list = TRUE)), error = function(e) "it is not a readable zip file")
      if (!is.null(problem)) {
        notes <- c(notes, paste0(fname, " was not read: ", problem, "."))
        next
      }
      tmp <- upload_tempdir("unzip_")
      dirs <- c(dirs, tmp)
      utils::unzip(datapath, exdir = tmp)

      gpkg_files    <- list.files(tmp, pattern = "\\.gpkg$", full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
      shp_files     <- list.files(tmp, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
      geojson_files <- list.files(tmp, pattern = "\\.(geojson|json)$", full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
      kml_files     <- list.files(tmp, pattern = "\\.kml$", full.names = TRUE, recursive = TRUE, ignore.case = TRUE)

      if (length(gpkg_files) > 0) {
        for (g in gpkg_files) add_gpkg(g, fname, paste0(fname, " -> ", basename(g)))
      } else if (length(shp_files) > 0) {
        add_polygons(read_polygons_from_path(shp_files[1], label = fname), fname)
      } else if (length(geojson_files) > 0) {
        add_polygons(read_polygons_from_path(geojson_files[1], label = fname), fname)
      } else if (length(kml_files) > 0) {
        lyr_info <- tryCatch(sf::st_layers(kml_files[1]), error = function(e) NULL)
        if (!is.null(lyr_info) && length(lyr_info$name) > 0) {
          add_polygons(read_polygons_from_path(kml_files[1], layer = lyr_info$name[1], label = fname), fname)
        }
      }

    } else if (ext == "gpkg") {
      add_gpkg(datapath, fname, fname)

    } else if (ext == "shp") {
      add_polygons(read_uploaded_shapefile_bundle(files_df, i), fname)

    } else if (ext %in% c("geojson", "json", "kml")) {
      add_polygons(read_polygons_from_path(datapath, label = fname), fname)
    }
  }

  imported_sf <- NULL
  if (length(imported_list) > 0) imported_sf <- dplyr::bind_rows(imported_list)
  # the folders of the waiting GeoPackages stay (and are removed when the session ends)
  list(imported = imported_sf, pending_gpkg = pending_gpkg, notes = notes, kept_dirs = kept_dirs())
}

assign_uploaded_colors <- function(up_sf, drawn_features_val = NULL, fixed_polys_val = NULL) {
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
