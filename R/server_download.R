# Retrieving the datasets in the overview, and downloading or returning them. Returns `retrieve_and_save()`.
server_download <- function(input, output, session, state, helpers) {
  overview <- state$overview
  download_msgs <- state$download_msgs
  prepared_zip <- state$prepared_zip
  retrieving <- state$retrieving
  job_id <- state$job_id
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

  # The job of this session in the background mode: its id, and what to do when it ends or the user leaves.
  # (A plain environment: the reactive values of a closed session cannot be read, see `job_id`.)
  current <- new.env()

  # Run a retrieval and hand its result to `on_done`. In the app's own process the result is there at once; in
  # the asynchronous mode (see R/async.R and R/jobs.R) the retrieval runs in a background process, the app stays
  # free for everyone, and `on_done` is called when it is finished. The job waits in a queue when all workers
  # are busy, and the user can cancel it. A session retrieves one overview at a time. If the user leaves
  # meanwhile, the job is cancelled, or `on_abandoned` is called with its result (to remove what was made).
  run_retrieval <- function(zipfile = NULL, save_files = TRUE, return_data = TRUE, on_done,
                            on_abandoned = function(res) invisible(NULL)) {
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
    jobs <- ndc_jobs()
    id <- job_submit(
      jobs, "retrieve_and_package",
      list(ov = ov, zipfile = zipfile, save_files = save_files, workdir = NULL, return_data = return_data,
           pack = TRUE),
      files = zipfile,
      # the tokens go to the process through its environment, not in the arguments (which are saved to a file)
      env = c(NDC_TOKEN = as.character(mytoken), ADC_TOKEN = as.character(agro_token))
    )
    if (is.null(id)) {
      showNotification("The server is busy: too many retrievals are waiting. Try again in a few minutes.",
                       type = "warning", duration = 10)
      return(invisible(NULL))
    }

    download_msgs(character(0))
    retrieving(TRUE)
    job_id(id)
    current$jobs <- jobs
    current$id <- id
    current$on_abandoned <- on_abandoned
    watch_job(jobs, id, session,
              on_done = function(res) {
                res$datasets <- lapply(res$datasets, unpack_result)
                download_msgs(res$messages)
                on_done(res)
              },
              on_failed = function(error) {
                # the session is passed explicitly: in a callback there may be no default one
                showNotification(paste0("The retrieval failed: ", error), type = "error", duration = 15,
                                 session = session)
              },
              on_cancelled = function() showNotification("The retrieval was cancelled.", session = session),
              finish = function() {
                current$id <- NULL
                retrieving(FALSE)
                job_id(NULL)
              })
    invisible(NULL)
  }

  # The user cancels the retrieval that waits or runs
  observeEvent(input$cancel_retrieval, {
    if (!is.null(current$id)) job_cancel(current$jobs, current$id)  # watch_job() notices and cleans up
  })

  # The user leaves: stop the job (a waiting or running one is cancelled, and what it made removed), or, if it
  # was finished already, let `on_abandoned` remove its result. No reactive values here: the session is closed.
  session$onSessionEnded(function() {
    id <- current$id
    if (is.null(id)) return(invisible(NULL))
    jobs <- current$jobs
    job_tick(jobs)
    if (identical(job_status(jobs, id)$state, "done")) {
      out <- job_collect(jobs, id)
      current$on_abandoned(out$value)
    } else {
      job_cancel(jobs, id)
      job_collect(jobs, id)
    }
    current$id <- NULL
  })

  output$download_ui <- renderUI({
    if (nrow(overview()) == 0) {
      div(style = "color: grey; font-style: italic;", "Download button will appear here once you add a dataset.")
    } else {
      busy <- !is.null(job_id())
      div(style = "display:flex; gap:10px; align-items:center;",
          if (busy) {
            tagList(
              actionButton("cancel_retrieval", "Cancel retrieval", class = "btn-custom"),
              span(style = "color: grey; font-style: italic;", "Retrieving in the background...")
            )
          } else {
            # Visible button: checks for data first, then triggers the hidden
            # real download only if there is something to download.
            actionButton("check_and_download", "Download dataset(s)", class = "btn-custom")
          },
          # always there: the pre-check clicks it when the retrieval is done
          div(style = "position:absolute; left:-9999px; width:1px; height:1px; overflow:hidden;",
              downloadButton("download_data", "Download dataset(s)", class = "btn-custom")),
          # "Return data to R" stops the app and hands the data to the calling R
          # session (`x <- shiny::runApp(...)`). Only offered in an interactive
          # session: in a container it would just shut the app down.
          if (interactive() && !busy) actionButton("return_to_r", "Return data to R (close app)", class = "btn-custom"))
    }
  })

  # Pre-check before downloading: build the zip once and check whether it has
  # data. If not, show the no-data notice only (no zip, no download dialog).
  # If it does, remember the built zip and trigger the hidden download button.
  observeEvent(input$check_and_download, {
    tmp_zip <- tempfile(fileext = ".zip")
    run_retrieval(zipfile = tmp_zip, save_files = TRUE, return_data = FALSE,
                  on_abandoned = function(res) unlink(tmp_zip),  # the user left while it was running
                  on_done = function(res) {
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
