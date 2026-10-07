# Retrieving the datasets in the overview, and downloading or returning them. Returns `retrieve_and_save()`.
server_download <- function(input, output, session, state, helpers) {
  overview <- state$overview
  download_msgs <- state$download_msgs
  prepared_zip <- state$prepared_zip
  retrieving <- state$retrieving
  mytoken <- state$mytoken
  agro_token <- state$agro_token

  # Retrieve the datasets in the overview (see R/retrieval.R). With `save_files`, the data are also written
  # to a folder (and zipped when a `zipfile` is given); otherwise they are only returned.
  retrieve_and_save <- function(zipfile = NULL, save_files = TRUE, workdir = NULL) {
    ov <- overview()
    req(nrow(ov) > 0)

    download_msgs(character(0))
    out <- withProgress(
      message = "Retrieving datasets...", value = 0,
      retrieve_and_package(ov, zipfile, save_files, workdir, ndc_token = mytoken, adc_token = agro_token,
                           progress = report_progress)
    )

    # Show what happened to each dataset ("Retrieved: ...", "Failed: ... - reason") in the messages
    # panel. The no-data notice is added by the download pre-check observer.
    download_msgs(out$messages)
    out
  }

  # Run a retrieval and hand its result to `on_done`. In the app's own process the result is there at once; in
  # the asynchronous mode (see R/async.R) the retrieval runs in a background process, the app stays free for
  # everyone, and `on_done` is called when it is finished. A session retrieves one overview at a time.
  run_retrieval <- function(zipfile = NULL, save_files = TRUE, return_data = TRUE, on_done) {
    if (isTRUE(retrieving())) {
      showNotification("A retrieval is already running: wait until it is done.", type = "warning")
      return(invisible(NULL))
    }
    if (!async_enabled()) {
      on_done(retrieve_and_save(zipfile = zipfile, save_files = save_files))
      return(invisible(NULL))
    }

    ov <- overview()
    req(nrow(ov) > 0)
    retrieving(TRUE)
    download_msgs(character(0))
    progress_file <- tempfile("ndc_progress_")
    progress <- async_progress(session, progress_file)
    finish <- function() {
      progress$close()
      retrieving(FALSE)
    }

    # everything the job needs is passed to it: it has no access to the session
    job <- retrieve_and_package
    writer <- progress_writer(progress_file)
    ndc_token <- mytoken
    adc_token <- agro_token
    promises::then(
      promises::future_promise(
        job(ov, zipfile, save_files, NULL, ndc_token, adc_token, progress = writer,
            return_data = return_data, pack = TRUE),
        globals = list(job = job, ov = ov, zipfile = zipfile, save_files = save_files, ndc_token = ndc_token,
                       adc_token = adc_token, writer = writer, return_data = return_data),
        seed = NULL
      ),
      onFulfilled = function(res) {
        finish()
        res$datasets <- lapply(res$datasets, unpack_result)
        download_msgs(res$messages)
        on_done(res)
      },
      onRejected = function(e) {
        finish()
        # the session is passed explicitly: in a callback there may be no default one
        showNotification(paste0("The retrieval failed: ", conditionMessage(e)), type = "error", duration = 15,
                         session = session)
      }
    ) |>
      # whatever else goes wrong in the callbacks must not disappear as an unhandled promise error
      promises::catch(function(e) {
        message("Unexpected error after a retrieval: ", conditionMessage(e))
      })
    invisible(NULL)
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
    run_retrieval(zipfile = tmp_zip, save_files = TRUE, return_data = FALSE, on_done = function(res) {
      if (session$isClosed()) {  # the user left while the retrieval was running
        unlink(tmp_zip)
        return(invisible(NULL))
      }
      produced <- isTRUE(res$produced_any)
      if (!produced || !file.exists(tmp_zip)) {
        prepared_zip(NULL)
        showNotification(
          "No data is available within your selection. Please try a different area, time period, or dataset.",
          type = "warning", duration = 8, session = session
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
    run_retrieval(save_files = FALSE, return_data = TRUE, on_done = return_data_to_r)
  })

  invisible(list(retrieve_and_save = retrieve_and_save))
}
