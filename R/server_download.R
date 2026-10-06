# Retrieving the datasets in the overview, and downloading or returning them. Returns `retrieve_and_save()`.
server_download <- function(input, output, session, state, helpers) {
  overview <- state$overview
  download_msgs <- state$download_msgs
  prepared_zip <- state$prepared_zip
  mytoken <- state$mytoken
  agro_token <- state$agro_token

  # Retrieve the datasets in the overview (see R/retrieval.R). With `save_files`, the data are also written
  # to a folder (and zipped when a `zipfile` is given); otherwise they are only returned.
  retrieve_and_save <- function(zipfile = NULL, save_files = TRUE, workdir = NULL) {
    ov <- overview()
    req(nrow(ov) > 0)

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

    download_msgs(character(0))
    local_msgs <- character(0)
    results <- list()
    manifest_rows <- list()

    withProgress(message = "Retrieving datasets...", value = 0, {
      for (i in seq_len(nrow(ov))) {
        res <- retrieve_row(ov[i, ], i, workdir, ndc_token = mytoken, adc_token = agro_token)
        if (!is.null(res$data)) results[[res$name]] <- res$data
        manifest_rows <- c(manifest_rows, res$manifest)
        local_msgs <- c(local_msgs, res$messages)
        incProgress(1 / nrow(ov))
      }
    })

    manifest_df <- if (length(manifest_rows) > 0) dplyr::bind_rows(manifest_rows) else tibble::tibble()
    produced_any <- any_data_produced(manifest_df)

    if (save_files) {
      utils::write.csv(manifest_df, file.path(workdir, "download_summary.csv"), row.names = FALSE)

      # Only write a zip when data was actually produced.
      if (!is.null(zipfile) && produced_any) zip_export(workdir, zipfile)
      # The zip is the deliverable: don't leave the export folder in the shared tempdir()
      if (!is.null(zipfile)) unlink(workdir, recursive = TRUE)
    }

    # Show what happened to each dataset ("Retrieved: ...", "Failed: ... - reason") in the messages
    # panel. The no-data notice is added by the download pre-check observer.
    download_msgs(local_msgs)

    out <- list(
      datasets = results,
      overview = ov,
      messages = download_msgs(),
      summary = manifest_df,
      produced_any = produced_any
    )

    if (save_files) {
      out$out_dir <- if (is.null(zipfile)) workdir  # with a zip the folder is removed
      out$zipfile <- zipfile
    }

    out
  }

  output$download_ui <- renderUI({
    if (nrow(overview()) == 0) {
      div(style = "color: grey; font-style: italic;", "Download button will appear here once you add a dataset.")
    } else {
      div(style = "display:flex; gap:10px; align-items:center;",
          # Visible button: checks for data first, then triggers the hidden
          # real download only if there is something to download.
          actionButton("check_and_download", "Download dataset(s)", class = "btn-custom"),
          div(style = "position:absolute; left:-9999px; width:1px; height:1px; overflow:hidden;",
              downloadButton("download_data", "Download dataset(s)", class = "btn-custom")),
          # "Return data to R" stops the app and hands the data to the calling R
          # session (`x <- shiny::runApp(...)`). Only offered in an interactive
          # session: in a container it would just shut the app down.
          if (interactive()) actionButton("return_to_r", "Return data to R (close app)", class = "btn-custom"))
    }
  })

  # Pre-check before downloading: build the zip once and check whether it has
  # data. If not, show the no-data notice only (no zip, no download dialog).
  # If it does, remember the built zip and trigger the hidden download button.
  observeEvent(input$check_and_download, {
    tmp_zip <- tempfile(fileext = ".zip")
    res <- retrieve_and_save(zipfile = tmp_zip, save_files = TRUE)
    produced <- isTRUE(res$produced_any)
    if (!produced || !file.exists(tmp_zip)) {
      prepared_zip(NULL)
      showNotification(
        "No data is available within your selection. Please try a different area, time period, or dataset.",
        type = "warning", duration = 8
      )
      download_msgs(c("No data is available within your selection. Please try a different area, time period, or dataset.",
                      res$messages))
    } else {
      old_zip <- isolate(prepared_zip())
      if (!is.null(old_zip)) unlink(old_zip)
      prepared_zip(tmp_zip)
      session$sendCustomMessage("ndc_trigger_download", TRUE)
    }
  })

  # Remove the prepared zip when the session ends.
  session$onSessionEnded(function() {
    zp <- isolate(prepared_zip())
    if (!is.null(zp)) unlink(zp)
  })

  # Serves the zip the pre-check already built and verified.
  output$download_data <- downloadHandler(
    filename = function() paste0("naturedatacube_", Sys.Date(), ".zip"),
    content = function(file) {
      zp <- prepared_zip()
      if (!is.null(zp) && file.exists(zp)) {
        file.copy(zp, file, overwrite = TRUE)
      } else {
        retrieve_and_save(zipfile = file, save_files = TRUE)
      }
    }
  )

  output$download_messages <- renderUI({
    msgs <- download_msgs()
    if (length(msgs) == 0) {
      p("Updates on dataset retrieval will appear here.", style = "color: grey; font-style: italic;")
    } else {
      HTML(paste(htmltools::htmlEscape(msgs), collapse = "<br>"))
    }
  })

  observeEvent(input$return_to_r, {
    res <- retrieve_and_save(save_files = FALSE)
    stopApp(res)
  })

  invisible(list(retrieve_and_save = retrieve_and_save))
}
