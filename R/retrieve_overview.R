# Retrieving all the rows of an overview, and packaging the result. These functions use no Shiny objects, so
# they can run in the app's own process or in a background process (see R/job_manager.R). `progress` is a
# function(detail, value) that shows how far the retrieval is.

retrieve_no_progress <- function(detail = NULL, value = NULL) invisible(NULL)

# Retrieve every row of the overview `ov`. Returns the data (`results`, named after the datasets and their
# rows), the rows for the download summary (`manifest_rows`), the `messages` and, when nothing is written
# (`workdir` is NULL), the `parameters` of the retrievals (see retrieve_row()).
retrieve_overview <- function(ov, workdir, ndc_token, adc_token, progress = retrieve_no_progress, parameters = TRUE) {
  n <- nrow(ov)
  results <- list()
  recorded <- list()
  manifest_rows <- list()
  messages <- character(0)

  for (i in seq_len(n)) {
    label <- paste0(ov$dataset[i], " (", ov$polygon[i], ")")
    progress(detail = label, value = (i - 1) / n)
    # the retrieval functions say how far they are (see rNDC::ndc_with_progress()): show it
    res <- retrieve_with_progress(
      retrieve_row(ov[i, ], i, workdir, ndc_token = ndc_token, adc_token = adc_token, parameters = parameters),
      label, i, n, progress
    )
    if (!is.null(res$data)) results[[res$name]] <- res$data
    for (extra in names(res$more)) results[[extra]] <- res$more[[extra]]
    if (length(res$parameters) > 0) recorded[[res$name]] <- res$parameters
    manifest_rows <- c(manifest_rows, res$manifest)
    messages <- c(messages, res$messages)
    progress(value = i / n)
  }

  list(results = results, manifest_rows = manifest_rows, messages = messages, parameters = recorded)
}

# Retrieve the overview and package it: with `save_files`, the data, the download summary and the references of
# the datasets (CITATIONS.txt) are written to a folder (`workdir`, a new temporary one by default), and zipped when
# a `zipfile` is given (the folder is then removed). The summary has the reference and license of the datasets
# that the NatureDataCube describes (see R/export_references.R), and with `parameters` the files with the parameters of the
# retrievals are in the folder `parameters` (when nothing is written they are returned: `parameters`). Returns the data (`datasets`; empty with `return_data = FALSE`), the `overview`, the
# `messages`, the `summary` and whether any data was produced. With `pack`, rasters and vectors of terra are
# wrapped, so that they can be sent from a background process (they are pointers otherwise).
retrieve_and_package <- function(ov, zipfile = NULL, save_files = TRUE, workdir = NULL,
                                 ndc_token = Sys.getenv("NDC_TOKEN"), adc_token = Sys.getenv("ADC_TOKEN"),
                                 progress = retrieve_no_progress, return_data = TRUE, pack = FALSE, parameters = TRUE) {
  if (save_files) {
    if (is.null(workdir)) {
      # unique per call: all sessions share one R process and one tempdir()
      workdir <- tempfile("ndc_export_")
    }
    if (dir.exists(workdir)) unlink(workdir, recursive = TRUE)
    dir.create(workdir, recursive = TRUE)
  } else {
    workdir <- NULL
  }

  run <- retrieve_overview(ov, workdir, ndc_token, adc_token, progress, parameters = parameters)

  manifest_df <- if (length(run$manifest_rows) > 0) dplyr::bind_rows(run$manifest_rows) else tibble::tibble()
  produced_any <- export_any_data(manifest_df)

  # the reference and the license of the datasets, from the metadata of the NatureDataCube
  messages <- run$messages
  metadata <- NULL
  if (produced_any) {
    refs <- export_add_references(manifest_df, ndc_token)
    manifest_df <- refs$summary
    metadata <- refs$metadata
    messages <- c(messages, refs$message)
  }

  if (save_files) {
    if (produced_any) export_write_citations(workdir, manifest_df, metadata)
    utils::write.csv(manifest_df, file.path(workdir, "download_summary.csv"), row.names = FALSE)

    # Only write a zip when data was actually produced.
    if (!is.null(zipfile) && produced_any) export_zip(workdir, zipfile)
    # The zip is the deliverable: don't leave the export folder in the shared tempdir()
    if (!is.null(zipfile)) unlink(workdir, recursive = TRUE)
  }

  datasets <- if (return_data) run$results else list()
  if (pack) datasets <- lapply(datasets, retrieve_pack)

  out <- list(
    datasets = datasets,
    overview = ov,
    messages = messages,
    summary = manifest_df,
    parameters = run$parameters,
    produced_any = produced_any
  )

  if (save_files) {
    out$out_dir <- if (is.null(zipfile)) workdir  # with a zip the folder is removed
    out$zipfile <- zipfile
  }

  out
}

# terra objects are pointers to memory of the process that made them: wrap them to send them to another
# process, and unwrap them there.
retrieve_pack <- function(x) if (inherits(x, c("SpatRaster", "SpatVector"))) terra::wrap(x) else x

retrieve_unpack <- function(x) if (inherits(x, c("PackedSpatRaster", "PackedSpatVector"))) terra::unwrap(x) else x
