# Retrieving in a background process.
#
# One R process serves all the sessions of the app, and a retrieval can take minutes: done in that process, it
# holds everyone up. With the asynchronous mode, the retrieval runs in a background R process (see R/jobs.R):
# the app stays responsive, jobs wait in a queue when all workers are busy, and the user can cancel. The mode
# is on in a deployment (a non-interactive session, e.g. the Docker image) and off in an interactive R session,
# where the app has a single user and the start of a worker would only cost time. `NDC_ASYNC=true` or `false`
# chooses, `NDC_WORKERS` sets the number of workers and `NDC_MAX_QUEUE` the number of jobs that may wait.

# Is retrieval done in background processes? Set by `setup_async()`.
async_enabled <- function() isTRUE(getOption("rNDC.Shiny.async", FALSE))

default_workers <- function() {
  cores <- suppressWarnings(parallel::detectCores())
  if (is.na(cores)) cores <- 2L
  max(1L, min(4L, cores - 1L))
}

default_max_queue <- 20L

# Is the package loaded with devtools::load_all() (and so not installed, which a new worker needs)?
is_dev_package <- function() exists(".__DEVTOOLS__", envir = asNamespace("rNDC.Shiny"))

# Can a background process use the package? It loads the installed package; a package that is only loaded with
# load_all() is loaded again in the worker with pkgload.
async_possible <- function(dev = is_dev_package(), pkgload = requireNamespace("pkgload", quietly = TRUE)) {
  !dev || pkgload
}

# A positive whole number from an environment variable, or `default`
env_count <- function(name, default) {
  value <- suppressWarnings(as.integer(Sys.getenv(name)))
  if (is.na(value) || value < 1) default else value
}

# Decide whether to retrieve in the background (see above) and set the options for it. Returns whether the
# mode is on.
setup_async <- function() {
  choice <- tolower(Sys.getenv("NDC_ASYNC"))
  async <- if (choice %in% c("true", "1", "yes")) {
    TRUE
  } else if (choice %in% c("false", "0", "no")) {
    FALSE
  } else {
    !interactive()
  }

  if (async && !async_possible()) {
    message("Retrieving in the background needs the package to be installed (or the 'pkgload' package): ",
            "retrieving in the app's own process instead.")
    async <- FALSE
  }

  options(rNDC.Shiny.async = async)
  if (async) {
    options(rNDC.Shiny.workers = env_count("NDC_WORKERS", default_workers()),
            rNDC.Shiny.max_queue = env_count("NDC_MAX_QUEUE", default_max_queue))
  }
  invisible(async)
}

# ---- Progress from a background process ----
# The worker cannot talk to the page: it writes where it is to a file, and the app reads the file every
# half second and shows it (see `watch_job()` in R/server_download.R).

# A function(detail, value) that writes the progress to `file` (replacing it as a whole)
progress_writer <- function(file) {
  force(file)  # the function outlives this call: it must carry the file name, not a promise to evaluate it
  state <- new.env()
  state$value <- 0
  state$detail <- ""
  function(detail = NULL, value = NULL) {
    if (!is.null(value)) state$value <- value
    if (!is.null(detail)) state$detail <- detail
    tmp <- paste0(file, ".tmp")
    saveRDS(list(value = state$value, detail = state$detail), tmp)
    file.rename(tmp, file)
    invisible(NULL)
  }
}

# The progress in `file`: list(value, detail), or NULL if there is none (yet)
read_progress <- function(file) {
  tryCatch(if (file.exists(file)) readRDS(file) else NULL, error = function(e) NULL)
}

# What the page says to a user whose job waits
queue_text <- function(status) {
  paste0("Position ", status$position, " of ", status$queued, " in the queue. It starts when a worker is free.")
}

# Follow the job `id` of the manager `jobs` in the page of `session`: a progress bar that says where the job is
# (waiting in the queue, or the progress it reports), and, when the job ends, one of the callbacks:
# `on_done(result)`, `on_failed(error)` or `on_cancelled()`. `finish()` is called first, in all three cases.
# The manager is asked every half second; this is also what starts waiting jobs and notices finished ones.
watch_job <- function(jobs, id, session, on_done, on_failed, on_cancelled, finish, interval = 500) {
  progress <- shiny::Progress$new(session, min = 0, max = 1)
  progress$set(value = 0, message = "Retrieving datasets...", detail = "Starting...")
  observer <- NULL
  end <- function() {
    observer$destroy()
    tryCatch(progress$close(), error = function(e) NULL)  # e.g. the session has ended
    finish()
  }
  observer <- shiny::observe({
    shiny::invalidateLater(interval, session)
    job_tick(jobs)
    status <- job_status(jobs, id)
    shiny::isolate({
      state <- if (is.null(status)) "cancelled" else status$state
      if (state == "queued") {
        progress$set(value = 0, message = "Waiting in the queue", detail = queue_text(status))
      } else if (state == "running") {
        p <- status$progress
        progress$set(value = if (is.null(p)) 0 else p$value, message = "Retrieving datasets...",
                     detail = if (is.null(p)) "Starting..." else p$detail)
      } else {
        end()
        out <- job_collect(jobs, id)
        if (state == "done") on_done(out$value)
        else if (state == "failed") on_failed(out$error)
        else on_cancelled()
      }
    })
  })
  invisible(observer)
}
