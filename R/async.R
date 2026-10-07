# Retrieving in a background process.
#
# One R process serves all the sessions of the app, and a retrieval can take minutes: done in that process, it
# holds everyone up. With the asynchronous mode, the retrieval runs in a background R process (a `future`
# worker) and the app stays responsive; jobs wait in a queue when all workers are busy. The mode is on in a
# deployment (a non-interactive session, e.g. the Docker image) and off in an interactive R session, where
# the app has a single user and the start of a worker would only cost time. `NDC_ASYNC=true` or `false`
# chooses, and `NDC_WORKERS` sets the number of workers.

# Is retrieval done in background processes? Set by `setup_async()`.
async_enabled <- function() isTRUE(getOption("rNDC.Shiny.async", FALSE))

default_workers <- function() max(1L, min(4L, future::availableCores() - 1L))

# Is the package loaded with devtools::load_all() (and so not installed, which a new worker needs)?
is_dev_package <- function() exists(".__DEVTOOLS__", envir = asNamespace("rNDC.Shiny"))

# The kind of background process to start: new R processes (`multisession`) load the installed package, so
# a package that is only loaded with load_all() works with forked processes (`multicore`) or not at all (NULL).
async_strategy <- function(dev = is_dev_package(), multicore = future::supportsMulticore()) {
  if (!dev) return(future::multisession)
  if (multicore) future::multicore else NULL
}

# Decide whether to retrieve in the background (see above), and set the future plan for it. A plan that is
# set already (not sequential) is kept. Returns whether the mode is on.
setup_async <- function() {
  choice <- tolower(Sys.getenv("NDC_ASYNC"))
  async <- if (choice %in% c("true", "1", "yes")) {
    TRUE
  } else if (choice %in% c("false", "0", "no")) {
    FALSE
  } else {
    !interactive()
  }

  if (async && inherits(future::plan(), "sequential")) {
    workers <- suppressWarnings(as.integer(Sys.getenv("NDC_WORKERS")))
    if (is.na(workers) || workers < 1) workers <- default_workers()

    strategy <- async_strategy()
    if (is.null(strategy)) {
      message("Retrieving in the background needs the package to be installed (or forked processes): ",
              "retrieving in the app's own process instead.")
      async <- FALSE
    } else {
      future::plan(strategy, workers = workers)
    }
  }

  options(rNDC.Shiny.async = async)
  invisible(async)
}

# ---- Progress from a background process ----
# The worker cannot talk to the page: it writes where it is to a file, and the app reads the file every
# half second and shows it.

# A function(detail, value) that writes the progress to `file` (replacing it as a whole)
progress_writer <- function(file) {
  # the function is sent to another process: it must carry the file name, not a promise to evaluate it
  force(file)
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

# Show the progress that a background process writes to `file` as a progress bar of the page. Call
# `$close()` of the result when the job is done.
async_progress <- function(session, file) {
  progress <- shiny::Progress$new(session, min = 0, max = 1)
  progress$set(value = 0, message = "Retrieving datasets...")
  observer <- shiny::observe({
    shiny::invalidateLater(500, session)
    p <- read_progress(file)
    if (!is.null(p)) progress$set(value = p$value, detail = p$detail)
  })
  list(close = function() {
    observer$destroy()
    tryCatch(progress$close(), error = function(e) NULL)  # e.g. the session has ended
    unlink(c(file, paste0(file, ".tmp")))
  })
}
