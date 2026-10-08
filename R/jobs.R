# Background jobs: a queue of jobs that run in separate R processes, one process per job. There is one manager
# for the whole R process (`ndc_jobs()`), shared by all sessions. At most `max_workers` jobs run at the same
# time; the others wait in the order in which they were submitted. A job that is waiting or running can be
# cancelled: a running job's process is killed (with its child processes) and everything it made is removed.
#
# The manager uses no Shiny objects. It does not run by itself: `job_tick()` starts waiting jobs and notices
# finished ones, and the sessions that wait for a job call it every half second (see `watch_job()`).
#
# A job is a function of this package, given by name, and its arguments (plain data): a new process cannot
# take over closures or the session. The function gets a `progress` argument if it has one (see
# `progress_writer()`), and its
# value is saved to a file, from which `job_collect()` reads it.

# The states of a job: "queued", "running", "done", "failed" or "cancelled" (a finished job stays in the manager
# until its result is collected with `job_collect()`).

# `start` is a function(job) that starts the process of a job and returns a list with `is_alive()`, `kill()` and
# `error()` (the last text of its error output); tests use their own.
new_job_manager <- function(max_workers = 1L, max_queue = Inf, start = start_job_process) {
  manager <- new.env(parent = emptyenv())
  manager$max_workers <- max_workers
  manager$max_queue <- max_queue
  manager$start <- start
  manager$jobs <- list()
  manager$counter <- 0L
  manager
}

# The manager of the app (made when it is first needed, with the options of `setup_async()`)
.jobs <- new.env(parent = emptyenv())

ndc_jobs <- function() {
  if (is.null(.jobs$manager)) {
    .jobs$manager <- new_job_manager(max_workers = getOption("rNDC.Shiny.workers", default_workers()),
                                     max_queue = getOption("rNDC.Shiny.max_queue", default_max_queue))
  }
  .jobs$manager
}

jobs_in_state <- function(manager, state) {
  Filter(function(job) job$state == state, manager$jobs)
}

# Submit a job: call the function `fun` (the name of a function of this package) with the list `args`. `files`
# are files that the job makes elsewhere and that go when it is cancelled. `env` is a named character vector of
# environment variables for the process of the job: this is where secrets such as API tokens go, and not in `args`,
# which callr saves to a file for the process. (A variable that a ~/.Renviron defines is overridden by it when the
# process starts: with the same value in practice, since the app takes its tokens from the same variables.)
# Returns the id of the job, or NULL if too many jobs are waiting.
job_submit <- function(manager, fun, args, files = character(0), env = character(0)) {
  # a job that finds all workers busy waits, unless too many wait already
  busy <- length(jobs_in_state(manager, "running")) >= manager$max_workers
  if (busy && length(jobs_in_state(manager, "queued")) >= manager$max_queue) return(NULL)

  manager$counter <- manager$counter + 1L
  id <- paste0("job", manager$counter)
  # everything a job makes (also the temporary files of its process) goes in one folder
  dir <- tempfile("ndc_job_")
  dir.create(dir, recursive = TRUE)
  manager$jobs[[id]] <- list(
    id = id, state = "queued", fun = fun, args = args, files = files, env = env, dir = dir,
    result_file = file.path(dir, "result.rds"), status_file = file.path(dir, "status.rds"),
    progress_file = file.path(dir, "progress.rds"),
    submitted = Sys.time(), started = NULL, handle = NULL, error = NULL
  )
  job_tick(manager)
  id
}

# Start waiting jobs while there are free workers, and look at the running jobs: one whose process has ended is
# done (its result is there) or failed.
job_tick <- function(manager) {
  for (job in jobs_in_state(manager, "running")) {
    if (job$handle$is_alive()) next
    if (file.exists(job$status_file)) {
      # only the small status file: the result can be large, and is read once, when it is collected
      result <- tryCatch(readRDS(job$status_file), error = function(e) NULL)
      if (is.list(result) && isTRUE(result$ok)) {
        manager$jobs[[job$id]]$state <- "done"
      } else {
        manager$jobs[[job$id]]$state <- "failed"
        manager$jobs[[job$id]]$error <- if (is.list(result)) result$error else "The result could not be read."
      }
    } else {
      manager$jobs[[job$id]]$state <- "failed"
      error <- tryCatch(job$handle$error(), error = function(e) "")
      manager$jobs[[job$id]]$error <- paste("The background process stopped unexpectedly.", error)
    }
  }

  for (job in jobs_in_state(manager, "queued")) {
    if (length(jobs_in_state(manager, "running")) >= manager$max_workers) break
    handle <- tryCatch(manager$start(job), error = function(e) e)
    if (inherits(handle, "error")) {
      manager$jobs[[job$id]]$state <- "failed"
      manager$jobs[[job$id]]$error <- paste("The background process could not be started:", conditionMessage(handle))
    } else {
      manager$jobs[[job$id]]$state <- "running"
      manager$jobs[[job$id]]$started <- Sys.time()
      manager$jobs[[job$id]]$handle <- handle
    }
  }
  invisible(manager)
}

# Where a job is: its `state`, its `position` in the queue (1 is next; 0 when it is not waiting), the number of
# jobs waiting (`queued`), the `progress` it reported, its `error` and the seconds it has been `running`.
# NULL for a job that is not known (any more).
job_status <- function(manager, id) {
  job <- manager$jobs[[id]]
  if (is.null(job)) return(NULL)
  queued <- names(jobs_in_state(manager, "queued"))
  list(
    state = job$state,
    position = if (job$state == "queued") match(id, queued) else 0L,
    queued = length(queued),
    progress = if (job$state == "running") read_progress(job$progress_file),
    error = job$error,
    running = if (!is.null(job$started)) as.numeric(difftime(Sys.time(), job$started, units = "secs"))
  )
}

# Stop a job that is waiting or running, and remove what it made. Returns whether there was one to stop.
job_cancel <- function(manager, id, tick = TRUE) {
  job <- manager$jobs[[id]]
  if (is.null(job) || !job$state %in% c("queued", "running")) return(invisible(FALSE))
  if (job$state == "running") {
    tryCatch(job$handle$kill(), error = function(e) NULL)
  }
  unlink(c(job$files, job$dir), recursive = TRUE)
  manager$jobs[[id]]$state <- "cancelled"
  # a freed worker is for the next job
  if (tick) job_tick(manager)
  invisible(TRUE)
}

# Take the outcome of a finished job and forget the job: list(state, value, error). The value is what the
# function returned. For a job that is not finished the state says so and there is no value.
job_collect <- function(manager, id) {
  job <- manager$jobs[[id]]
  if (is.null(job)) return(list(state = "unknown", value = NULL, error = NULL))
  out <- list(state = job$state, value = NULL, error = job$error)
  if (job$state == "done") {
    out$value <- tryCatch(readRDS(job$result_file)$value, error = function(e) {
      out$state <<- "failed"
      out$error <<- "The result could not be read."
      NULL
    })
  }
  if (!job$state %in% c("queued", "running")) {
    unlink(job$dir, recursive = TRUE)
    manager$jobs[[id]] <- NULL
  }
  out
}

# Stop all jobs (when the app ends)
job_shutdown <- function(manager) {
  for (id in names(manager$jobs)) {
    job_cancel(manager, id, tick = FALSE)  # no waiting job may take the place of one that is stopped
    job <- manager$jobs[[id]]
    if (!is.null(job)) unlink(job$dir, recursive = TRUE)
  }
  manager$jobs <- list()
  invisible(NULL)
}

# ---- The process of a job ----

# Save the outcome of a job: the result (which can be large) first, then the small status file that
# `job_tick()` looks at, so that a status always has its result. Each file is written whole before it appears.
save_job_outcome <- function(out, result_file, status_file) {
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
    if ("progress" %in% names(formals(f))) args$progress <- ns$progress_writer(progress_file)
    list(ok = TRUE, value = do.call(f, args))
  }, error = function(e) list(ok = FALSE, error = conditionMessage(e)))
  ns$save_job_outcome(out, result_file, status_file)
  invisible(NULL)
}

# Start a job in a new R process (callr). Its output goes to files: an unread pipe would block it when full.
start_job_process <- function(job) {
  dev_path <- if (is_dev_package()) getNamespaceInfo("rNDC.Shiny", "path")
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
