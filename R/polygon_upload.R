# Polygons that users bring (uploads and drawings): reading, cleaning and the checks of zip files.

# Is the coordinate system of `x` missing? Files without one come as NA, or, from a GeoPackage, as an "undefined"
# engineering system that cannot be transformed.
polygon_crs_missing <- function(x) {
  crs <- sf::st_crs(x)
  is.na(crs) || grepl("Undefined (Cartesian|geographic) SRS", crs$wkt)
}

# Make polygons from an sf object as the app wants them: only (multi)polygons, without Z/M, in EPSG:4326, and
# valid (a self-intersecting polygon is repaired; one that cannot be is dropped). Returns NULL if there was no polygon
# at all, and an object without rows if none was left after the repair.
# What was done that the user should know goes in the attribute "notes" (a character vector, see `polygon_notes()`).
# `label` names the object in the notes (e.g. the file).
polygon_clean <- function(x, label = "The polygons") {
  notes <- character(0)
  x <- x[sf::st_is(x, c("POLYGON", "MULTIPOLYGON")), , drop = FALSE]
  if (nrow(x) == 0) return(NULL)

  if (polygon_crs_missing(x)) {
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

# The notes that `polygon_clean()` left on its result
polygon_notes <- function(x) {
  notes <- attr(x, "notes")
  if (is.null(notes)) character(0) else notes
}

polygon_read <- function(path, layer = NULL, label = basename(path)) {
  sf_obj <- tryCatch({
    if (!is.null(layer)) sf::st_read(path, layer = layer, quiet = TRUE) else sf::st_read(path, quiet = TRUE)
  }, error = function(e) NULL)
  if (is.null(sf_obj)) return(NULL)
  polygon_clean(sf_obj, label)
}

# Folders for unpacking uploads, which are removed when `polygon_process_upload()` is done, except the ones with a
# GeoPackage that waits for a layer to be chosen.
polygon_upload_dir <- function(prefix) {
  dir <- tempfile(prefix)
  dir.create(dir)
  dir
}

# What is wrong with a zip file, from its listing (`utils::unzip(list = TRUE)`: Name and Length), or NULL: too
# many files, too large when unpacked (a zip bomb), or paths that would be written outside the folder.
polygon_zip_problem <- function(listing, max_files = max_zip_files, max_mb = max_zip_unpacked_mb) {
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
polygon_process_upload <- function(files_df) {
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
      add_polygons(polygon_read(path, label = fname), fname)
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

    tmp <- polygon_upload_dir("shp_bundle_")
    dirs <<- c(dirs, tmp)
    for (j in seq_len(nrow(bundle))) {
      file.copy(bundle$datapath[j], file.path(tmp, basename(bundle$name[j])), overwrite = TRUE)
    }
    polygon_read(file.path(tmp, paste0(base, ".shp")), label = shp_name)
  }

  for (i in seq_len(nrow(files_df))) {
    fname <- files_df$name[i]
    datapath <- files_df$datapath[i]
    ext <- tolower(tools::file_ext(fname))

    if (ext == "zip") {
      problem <- tryCatch(polygon_zip_problem(utils::unzip(datapath, list = TRUE)), error = function(e) "it is not a readable zip file")
      if (!is.null(problem)) {
        notes <- c(notes, paste0(fname, " was not read: ", problem, "."))
        next
      }
      tmp <- polygon_upload_dir("unzip_")
      dirs <- c(dirs, tmp)
      utils::unzip(datapath, exdir = tmp)

      gpkg_files    <- list.files(tmp, pattern = "\\.gpkg$", full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
      shp_files     <- list.files(tmp, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
      geojson_files <- list.files(tmp, pattern = "\\.(geojson|json)$", full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
      kml_files     <- list.files(tmp, pattern = "\\.kml$", full.names = TRUE, recursive = TRUE, ignore.case = TRUE)

      if (length(gpkg_files) > 0) {
        for (g in gpkg_files) add_gpkg(g, fname, paste0(fname, " -> ", basename(g)))
      } else if (length(shp_files) > 0) {
        add_polygons(polygon_read(shp_files[1], label = fname), fname)
      } else if (length(geojson_files) > 0) {
        add_polygons(polygon_read(geojson_files[1], label = fname), fname)
      } else if (length(kml_files) > 0) {
        lyr_info <- tryCatch(sf::st_layers(kml_files[1]), error = function(e) NULL)
        if (!is.null(lyr_info) && length(lyr_info$name) > 0) {
          add_polygons(polygon_read(kml_files[1], layer = lyr_info$name[1], label = fname), fname)
        }
      }

    } else if (ext == "gpkg") {
      add_gpkg(datapath, fname, fname)

    } else if (ext == "shp") {
      add_polygons(read_uploaded_shapefile_bundle(files_df, i), fname)

    } else if (ext %in% c("geojson", "json", "kml")) {
      add_polygons(polygon_read(datapath, label = fname), fname)
    }
  }

  imported_sf <- NULL
  if (length(imported_list) > 0) imported_sf <- dplyr::bind_rows(imported_list)
  # the folders of the waiting GeoPackages stay (and are removed when the session ends)
  list(imported = imported_sf, pending_gpkg = pending_gpkg, notes = notes, kept_dirs = kept_dirs())
}
