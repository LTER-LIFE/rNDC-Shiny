# One row of the overview: retrieve it, write the file with a name of its own, and describe what happened.

# Retrieve the dataset of one row of the overview. With a `workdir`, the data and the polygon are written
# there; without one, nothing is written. Returns the retrieved `data` and its `name` (NULL if nothing was
# retrieved), the rows for the download summary (`manifest`) and the `messages`. With `parameters`, the rNDC functions
# record their parameters (see rNDC::ndc_repeat()): with a `workdir` they go to the folder `parameters` of it, named
# after the data file; without one they are returned in `parameters` (the texts of the JSON files, by the call:
# `data`, and for Weather `station`).
retrieve_row <- function(row, index, workdir = NULL, ndc_token, adc_token, parameters = TRUE) {
  save_files <- !is.null(workdir)
  ds <- row$dataset
  view <- row$view
  polygon <- row$polygon
  date_label <- export_date_label(row$year, row$date_from, row$date_to)
  summary_row <- function(file_type, file_path, status, note = "", parameters = NA_character_) {
    export_manifest_row(ds, view, polygon, date_label, file_type, file_path, status, note, parameters = parameters)
  }

  # the files that the retrieval functions record their parameters in start with `stem` (hidden, in the folder of the
  # download, or in a folder of their own); they are moved or read below, and what is left is removed
  stem <- NULL
  recorded <- list()
  if (isTRUE(parameters)) {
    scratch <- if (save_files) workdir else tempfile("ndc_params_")
    dir.create(scratch, showWarnings = FALSE, recursive = TRUE)
    stem <- file.path(scratch, paste0(".params_", index))
    on.exit({
      unlink(export_parameter_files(stem))
      if (!save_files) unlink(scratch, recursive = TRUE)
    }, add = TRUE)
  }

  # Filename base includes the polygon name; all output filenames lowercase.
  polygon_name <- tolower(safe_filename(as.character(polygon)))
  file_base <- tolower(safe_filename(paste0(ds, "_", view, "_", polygon_name)))

  manifest <- list()
  messages <- character(0)
  data <- NULL
  name <- NULL

  if (save_files && !is.null(row$polygon_sf[[1]])) {
    polygon_row <- tryCatch(export_reference_polygon(row, workdir, polygon_name, date_label), error = function(e) NULL)
    if (!is.null(polygon_row)) manifest <- c(manifest, list(polygon_row))
  }

  tryCatch({
    outcome <- retrieve_dataset(row, index, ndc_token, adc_token, stem)

    if (is.null(outcome$data)) {
      messages <- c(messages, outcome$message)
      manifest <- c(manifest, list(summary_row(outcome$file_type, NA_character_, outcome$status)))
    } else {
      data <- outcome$data
      name <- paste0(outcome$name, "_", index)
      if (save_files) {
        file <- export_unique_file(workdir, file_base, outcome$ext, date_label)
        ok <- outcome$writer(file)
        placed <- if (ok && !is.null(stem)) export_place_parameters(stem, workdir, file) else NA_character_
        manifest <- c(manifest, list(summary_row(outcome$ext, basename(file), if (ok) "ok" else "failed",
                                                 parameters = placed)))
        messages <- c(messages, if (ok) outcome$ok_msg else outcome$fail_msg)
      } else {
        if (!is.null(stem)) recorded <- export_read_parameters(stem)
        manifest <- c(manifest, list(summary_row(outcome$ext, NA_character_, "ok")))
        messages <- c(messages, outcome$ok_msg)
      }
    }
  }, error = function(e) {
    messages <<- c(messages, paste0("Failed: ", ds, " - ", e$message))
    manifest <<- c(manifest, list(summary_row(NA_character_, NA_character_, "failed", note = e$message)))
  })

  list(data = data, name = name, manifest = manifest, messages = messages, parameters = recorded)
}

# Path for the file of a dataset in `dir`. The names are built from the dataset, the view and the polygon,
# so rows that differ only in year or period would overwrite each other: a row whose name is already taken
# gets its period appended (`..._2023`), or a number if that is taken too.
export_unique_file <- function(dir, base, ext, date_label) {
  file <- file.path(dir, paste0(base, ".", ext))
  if (!file.exists(file)) return(file)

  period <- tolower(safe_filename(gsub(" - ", "_to_", date_label)))
  candidates <- c(if (nzchar(period)) paste0(base, "_", period), paste0(base, "_", 2:999))
  for (candidate in candidates) {
    file <- file.path(dir, paste0(candidate, ".", ext))
    if (!file.exists(file)) return(file)
  }
  stop("Could not find a free file name for ", base, ".", ext, call. = FALSE)
}

# Export the selected polygon as a GeoPackage, once per polygon. In the download summary this row is
# labelled as the reference polygon (dataset "Area") rather than the dataset it was exported alongside.
export_reference_polygon <- function(row, workdir, polygon_name, date_label) {
  geom <- sf::st_sf(polygon = as.character(row$polygon), geometry = sf::st_geometry(row$polygon_sf[[1]]))
  if (is.na(sf::st_crs(geom))) sf::st_crs(geom) <- 4326

  file <- export_polygon_file(workdir, polygon_name, geom)
  if (is.null(file)) return(NULL)  # this polygon was exported already

  ok <- export_write_data(geom, file)
  export_manifest_row("Area", "Reference polygon", row$polygon, date_label, "gpkg",
               if (ok) basename(file) else NA_character_,
               if (ok) "ok" else "failed", note = "selected polygon geometry")
}

# Path for the file of the polygon `geom` in `dir`: NULL if the same polygon is already there. Polygons are
# named after the polygon, and different polygons can share a name (e.g. uploaded files with the same file
# name), so a name taken by another polygon gets a number appended.
export_polygon_file <- function(dir, name, geom) {
  for (candidate in c(name, paste0(name, "_", 2:999))) {
    file <- file.path(dir, paste0(candidate, ".gpkg"))
    if (!file.exists(file)) return(file)
    if (export_same_polygon(file, geom)) return(NULL)
  }
  stop("Could not find a free file name for ", name, ".gpkg", call. = FALSE)
}

# Does the GeoPackage `file` hold the same geometry as `geom`?
export_same_polygon <- function(file, geom) {
  existing <- tryCatch(sf::st_read(file, quiet = TRUE), error = function(e) NULL)
  if (is.null(existing) || nrow(existing) != nrow(geom)) return(FALSE)
  if (sf::st_crs(existing) != sf::st_crs(geom)) existing <- sf::st_transform(existing, sf::st_crs(geom))
  same <- sf::st_equals(sf::st_geometry(existing), sf::st_geometry(geom), sparse = FALSE)
  all(diag(same))
}
