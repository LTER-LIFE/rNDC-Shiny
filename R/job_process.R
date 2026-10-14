# The process of a job (see R/job_manager.R): what the new R process runs, how it is started, and the progress file that it writes.

# Save the outcome of a job: the result (which can be large) first, then the small status file that
# `job_tick()` looks at, so that a status always has its result. Each file is written whole before it appears.
job_save_outcome <- function(out, result_file, status_file) {
  for (x in list(list(out, result_file), list(list(ok = out$ok, error = out$error), status_file))) {
    tmp <- paste0(x[[2]], ".tmp")
    saveRDS(x[[1]], tmp)
    file.rename(tmp, x[[2]])
  }
  invisible(NULL)
}

# What the new R process runs: load this package (the installed one, or the source folder `dev_path` when the
# package is loaded with devtools), call `fun` and save the outcome. An error is saved too, so that the app can
# say what went wrong. The function has no environment of its own (callr gives it the global one): it must not
# use anything but base R and the package.
job_worker <- function(fun, args, result_file, progress_file, status_file, dev_path = NULL) {
  if (!is.null(dev_path)) pkgload::load_all(dev_path, quiet = TRUE)
  ns <- asNamespace("rNDC.Shiny")
  out <- tryCatch({
    f <- get(fun, envir = ns)
    # a function that reports progress gets the writer
    if ("progress" %in% names(formals(f))) args$progress <- ns$job_progress_writer(progress_file)
    list(ok = TRUE, value = do.call(f, args))
  }, error = function(e) list(ok = FALSE, error = conditionMessage(e)))
  ns$job_save_outcome(out, result_file, status_file)
  invisible(NULL)
}

# Start a job in a new R process (callr). Its output goes to files: an unread pipe would block it when full.
job_start_process <- function(job) {
  dev_path <- if (job_is_dev_package()) getNamespaceInfo("rNDC.Shiny", "path")
  stderr <- file.path(job$dir, "stderr.txt")
  process <- callr::r_bg(
    job_worker, args = list(job$fun, job$args, job$result_file, job$progress_file, job$status_file, dev_path),
    stdout = file.path(job$dir, "stdout.txt"), stderr = stderr,
    env = c(callr::rcmd_safe_env(), job$env, TMPDIR = job$dir, TMP = job$dir, TEMP = job$dir),
    supervise = TRUE
  )
  list(
    is_alive = function() process$is_alive(),
    kill = function() {
      # the process and the processes that it started (e.g. GDAL)
      tryCatch(process$kill_tree(), error = function(e) process$kill())
    },
    error = function() {
      if (!file.exists(stderr)) return("")
      paste(utils::tail(readLines(stderr, warn = FALSE), 5), collapse = " ")
    }
  )
}

# ---- Progress from a background process ----

# Progress from a background process: the worker cannot talk to the page, it writes where it is to a file, and the
# app reads the file every half second and shows it (see `download_watch_job()` in R/server_download.R).

# A function(detail, value) that writes the progress to `file` (replacing it as a whole)
job_progress_writer <- function(file) {
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
job_progress_read <- function(file) {
  tryCatch(if (file.exists(file)) readRDS(file) else NULL, error = function(e) NULL)
}
