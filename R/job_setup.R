# Retrieving in a background process.
#
# One R process serves all the sessions of the app, and a retrieval can take minutes: done in that process, it
# holds everyone up. With the asynchronous mode, the retrieval runs in a background R process (see R/job_manager.R):
# the app stays responsive, jobs wait in a queue when all workers are busy, and the user can cancel. The mode
# is on in a deployment (a non-interactive session, e.g. the Docker image) and off in an interactive R session,
# where the app has a single user and the start of a worker would only cost time. `NDC_ASYNC=true` or `false`
# chooses, `NDC_WORKERS` sets the number of workers and `NDC_MAX_QUEUE` the number of jobs that may wait.

# Is retrieval done in background processes? Set by `job_setup_async()`.
job_async_enabled <- function() isTRUE(getOption("rNDC.Shiny.async", FALSE))

job_default_workers <- function() {
  cores <- suppressWarnings(parallel::detectCores())
  if (is.na(cores)) cores <- 2L
  max(1L, min(4L, cores - 1L))
}

job_default_max_queue <- 20L

# Is the package loaded with devtools::load_all() (and so not installed, which a new worker needs)?
job_is_dev_package <- function() exists(".__DEVTOOLS__", envir = asNamespace("rNDC.Shiny"))

# Can a background process use the package? It loads the installed package; a package that is only loaded with
# load_all() is loaded again in the worker with pkgload.
job_async_possible <- function(dev = job_is_dev_package(), pkgload = requireNamespace("pkgload", quietly = TRUE)) {
  !dev || pkgload
}

# Decide whether to retrieve in the background (see above) and set the options for it. Returns whether the
# mode is on.
job_setup_async <- function() {
  choice <- tolower(Sys.getenv("NDC_ASYNC"))
  async <- if (choice %in% c("true", "1", "yes")) {
    TRUE
  } else if (choice %in% c("false", "0", "no")) {
    FALSE
  } else {
    !interactive()
  }

  if (async && !job_async_possible()) {
    message("Retrieving in the background needs the package to be installed (or the 'pkgload' package): ",
            "retrieving in the app's own process instead.")
    async <- FALSE
  }

  options(rNDC.Shiny.async = async)
  if (async) {
    options(rNDC.Shiny.workers = env_count("NDC_WORKERS", job_default_workers()),
            rNDC.Shiny.max_queue = env_count("NDC_MAX_QUEUE", job_default_max_queue))
  }
  invisible(async)
}
