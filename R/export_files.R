# What a retrieval writes: the files of the download (data, parameters, zip) and the rows of the download summary.

# The files that were recorded for the stem
export_parameter_files <- function(stem) {
  files <- list.files(dirname(stem), all.files = TRUE, full.names = TRUE)
  files[startsWith(basename(files), paste0(basename(stem), "_")) & endsWith(files, ".json")]
}

# The slot (`data`, `station`) of a recorded file
export_parameter_slot <- function(stem, file) {
  sub("\\.json$", "", substring(basename(file), nchar(basename(stem)) + 2))
}

# Move the files recorded for the stem to the folder `parameters` of the download, named after `data_file`: the file
# of the data is `<name>.json`, another one `<name>_<slot>.json`. Returns the path of the file of the data in the zip
# (`parameters/<name>.json`), or NA if the retrieval recorded none.
export_place_parameters <- function(stem, workdir, data_file) {
  files <- export_parameter_files(stem)
  if (length(files) == 0) return(NA_character_)
  dir.create(file.path(workdir, "parameters"), showWarnings = FALSE)
  base <- tools::file_path_sans_ext(basename(data_file))
  placed <- NA_character_
  for (file in files) {
    slot <- export_parameter_slot(stem, file)
    name <- if (slot == "data") base else paste0(base, "_", slot)
    target <- file.path(workdir, "parameters", paste0(name, ".json"))
    if (file.exists(target)) next  # (the names of the data files are free: this does not happen)
    if (file.rename(file, target) && slot == "data") placed <- paste0("parameters/", name, ".json")
  }
  placed
}

# The texts of the files recorded for the stem, by slot
export_read_parameters <- function(stem) {
  files <- export_parameter_files(stem)
  texts <- lapply(files, function(file) paste(readLines(file, warn = FALSE, encoding = "UTF-8"), collapse = "\n"))
  stats::setNames(texts, vapply(files, function(file) export_parameter_slot(stem, file), ""))
}

# A row of the download summary
export_manifest_row <- function(dataset, view, polygon, date, file_type, file_path, status, note = "",
                         reference = NA_character_, license = NA_character_, parameters = NA_character_) {
  tibble::tibble(dataset = dataset, view = view, polygon = polygon, date = date, file_type = file_type,
                 file_path = file_path, status = status, note = note, reference = reference, license = license,
                 parameters = parameters)
}

export_date_label <- function(year, date_from, date_to) {
  if (!is.na(year)) return(as.character(year))

  if (!is.na(date_from) && !is.na(date_to)) {
    if (date_from == date_to) return(format(date_from, "%Y-%m-%d"))
    return(paste0(format(date_from, "%Y-%m-%d"), " - ", format(date_to, "%Y-%m-%d")))
  }

  if (!is.na(date_from)) return(format(date_from, "%Y-%m-%d"))
  if (!is.na(date_to)) return(format(date_to, "%Y-%m-%d"))
  ""
}

# Did we retrieve any data? A row counts if its status is "ok" and it isn't the polygon-geometry helper
# file. No file path is required, so this also works when nothing is written.
export_any_data <- function(manifest) {
  if (nrow(manifest) == 0 || !"status" %in% names(manifest)) return(FALSE)
  is_ok <- !is.na(manifest$status) & manifest$status == "ok"
  not_polygon <- if ("note" %in% names(manifest)) {
    is.na(manifest$note) | manifest$note != "selected polygon geometry"
  } else TRUE
  any(is_ok & not_polygon)
}

# `root` avoids setwd(), which would change the working directory of every session served by this R process.
export_zip <- function(workdir, zipfile) {
  files <- list.files(workdir, recursive = TRUE, full.names = FALSE, no.. = TRUE)
  # mirror: the files in folders (`parameters`) keep their folder; the default puts every file at the top
  if (length(files) > 0) zip::zipr(zipfile, files = files, root = workdir, mode = "mirror")
  invisible(zipfile)
}

# Write a retrieved object to `outfile`, in the format of its extension. Returns TRUE if it was written.
export_write_data <- function(obj, outfile) {
  if (is.null(obj)) return(FALSE)

  ext <- tolower(tools::file_ext(outfile))

  if (ext == "csv") {
    # tables: the attributes of an sf object, or a data frame
    df <- if (inherits(obj, "sf")) sf::st_drop_geometry(obj) else if (inherits(obj, "data.frame")) obj else NULL

    if (is.null(df) || nrow(df) == 0) return(FALSE)
    utils::write.csv(df, outfile, row.names = FALSE)
    return(TRUE)
  }

  if (ext == "gpkg") {
    if (inherits(obj, "sf")) {
      sf::st_write(obj, outfile, delete_dsn = TRUE, quiet = TRUE)
      return(TRUE)
    }

    if (inherits(obj, "SpatVector")) {
      terra::writeVector(obj, outfile, overwrite = TRUE)
      return(TRUE)
    }

    return(FALSE)
  }

  if (ext %in% c("tif", "tiff")) {
    if (inherits(obj, "SpatRaster")) {
      terra::writeRaster(obj, outfile, overwrite = TRUE)
      return(TRUE)
    }
    return(FALSE)
  }

  FALSE
}
