# References and licenses of the retrieved datasets, from the metadata that the NatureDataCube publishes for
# its datasets (STAC collections, read with rNDC::ndc_metadata()): they fill the columns `reference` and
# `license` of the download summary, and the file CITATIONS.txt of the zip. Datasets from other services
# (AgroDataCube) have no such metadata.

# The AgroDataCube datasets: not described in the NatureDataCube
export_agrodatacube_datasets <- c("Weather", "Agricultural fields")

# The STAC collections that describe a dataset of the app (none for a dataset without metadata there).
# `view` and `polygon` tell the NDVI statistics (collection of the project of the polygon) from the rasters.
export_dataset_collections <- function(dataset, view = "Geodata", polygon = NA_character_) {
  statistics <- identical(tolower(as.character(view)), "statistics")
  collections <- switch(as.character(dataset),
    "Land Use" = ndc_landuse_collection,
    "Nitrogen" = nitrogen_layer_choices,
    "Soil map" = ndc_soil_collection,
    "AHN" = ndc_ahn_collection,
    "NDVI" = if (statistics) project_ndvi_collection(polygon) else "ndvi",
    character(0)
  )
  as.character(collections[!is.na(collections)])
}

# The collections of every row of the download summary that was retrieved (a list, one element per row)
export_summary_collections <- function(summary) {
  lapply(seq_len(nrow(summary)), function(i) {
    if (!identical(summary$status[i], "ok")) return(character(0))
    export_dataset_collections(summary$dataset[i], summary$view[i], summary$polygon[i])
  })
}

# Fill `reference` and `license` in the download summary. The metadata is read with one request; if that fails
# the summary stays as it is and the reason is in `message`. Returns list(summary, metadata, message): the
# `metadata` is NULL when it was not needed or could not be read.
export_add_references <- function(summary, ndc_token, read_metadata = rNDC::ndc_metadata) {
  nothing <- function(message = character(0)) list(summary = summary, metadata = NULL, message = message)
  if (nrow(summary) == 0) return(nothing())
  collections <- export_summary_collections(summary)
  needed <- unique(unlist(collections))
  if (length(needed) == 0) return(nothing())

  metadata <- tryCatch(read_metadata(needed, token = ndc_token), error = function(e) e)
  if (inherits(metadata, "error")) {
    return(nothing(paste0("References could not be read: ", conditionMessage(metadata))))
  }

  summary$reference <- as.character(summary$reference)
  summary$license <- as.character(summary$license)
  for (i in seq_along(collections)) {
    ids <- collections[[i]]
    if (length(ids) == 0) next
    refs <- unname(rNDC::ndc_citation(ids, metadata = metadata, license = FALSE))
    refs <- unique(refs[!is.na(refs)])
    licenses <- metadata$license[match(ids, metadata$id)]
    licenses <- unique(licenses[!is.na(licenses)])
    if (length(refs) > 0) summary$reference[i] <- paste(refs, collapse = " | ")
    if (length(licenses) > 0) summary$license[i] <- paste(licenses, collapse = "; ")
  }
  list(summary = summary, metadata = metadata, message = character(0))
}

# The text of CITATIONS.txt: for every dataset that was retrieved, the reference, DOI and license of its
# collections (the collections with the same text together), the related publications, and which datasets have
# no reference in the NatureDataCube. `metadata` is NULL if it could not be read. `date` is the date of the
# retrieval.
export_citations_text <- function(summary, metadata, date = Sys.Date()) {
  collections <- export_summary_collections(summary)
  ok <- !is.na(summary$status) & summary$status == "ok" & summary$dataset != "Area"
  datasets <- unique(summary$dataset[ok])

  lines <- c("CITATIONS",
             paste0("Datasets retrieved on ", format(date, "%Y-%m-%d"), " with the NatureDataCube app. ",
                    "Please cite the data that you use."),
             "")
  without <- character(0)

  for (dataset in datasets) {
    ids <- unique(unlist(collections[ok & summary$dataset == dataset]))
    if (length(ids) == 0) {
      without <- c(without, dataset)
      next
    }
    lines <- c(lines, dataset, strrep("-", nchar(dataset)))
    if (is.null(metadata)) {
      lines <- c(lines, paste0("The reference could not be read. See the metadata of the collection(s) ",
                               paste(ids, collapse = ", "), " in the NatureDataCube."), "")
      next
    }
    texts <- rNDC::ndc_citation(ids, metadata = metadata, publications = TRUE)
    for (text in unique(texts[!is.na(texts)])) {
      same <- names(texts)[!is.na(texts) & texts == text]
      lines <- c(lines, paste0("Collection(s): ", paste(same, collapse = ", ")), text, "")
    }
    if (all(is.na(texts))) {
      lines <- c(lines, paste0("No reference is published for the collection(s) ", paste(ids, collapse = ", "), "."), "")
    }
  }

  if (length(without) > 0) {
    from_adc <- intersect(without, export_agrodatacube_datasets)
    other <- setdiff(without, from_adc)
    lines <- c(lines, "Datasets without a reference in the NatureDataCube", strrep("-", 51))
    if (length(from_adc) > 0) {
      lines <- c(lines, paste0(paste(from_adc, collapse = ", "), ": retrieved from the AgroDataCube ",
                               "(https://agrodatacube.wur.nl/). See the provider for how to cite it."))
    }
    if (length(other) > 0) lines <- c(lines, paste0(paste(other, collapse = ", "), ": no reference is available."))
    lines <- c(lines, "")
  }
  lines
}

# Write CITATIONS.txt in `dir`
export_write_citations <- function(dir, summary, metadata, date = Sys.Date()) {
  con <- file(file.path(dir, "CITATIONS.txt"), open = "w", encoding = "UTF-8")
  on.exit(close(con))
  writeLines(export_citations_text(summary, metadata, date), con)
  invisible(file.path(dir, "CITATIONS.txt"))
}
