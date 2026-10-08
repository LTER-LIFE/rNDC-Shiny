# Run by test-async.R in a separate R process: a job is sent to a fresh background process with the installed
# package, as the app does. It runs offline (a dataset without retrieval) and prints "JOB OK".
# This is a script, and not code of the test itself, because what goes wrong depends on where it is created.

suppressMessages(library(rNDC.Shiny))
ns <- asNamespace("rNDC.Shiny")

m <- ns$new_job_manager(max_workers = 1)
poly <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 0)))), crs = 4326))
ov <- tibble::tibble(dataset = "Vegetation structure", view = "Geodata", year = NA_integer_, polygon = "p",
                     wkt = sf::st_as_text(sf::st_geometry(poly)), polygon_sf = list(poly),
                     date_from = as.Date(NA), date_to = as.Date(NA))
id <- ns$job_submit(m, "retrieve_and_package",
                    list(ov = ov, zipfile = NULL, save_files = FALSE, workdir = NULL, return_data = TRUE, pack = TRUE),
                    env = c(NDC_TOKEN = "t", ADC_TOKEN = "t"))

t0 <- Sys.time()
repeat {
  ns$job_tick(m)
  state <- ns$job_status(m, id)$state
  if (!state %in% c("queued", "running") || as.numeric(Sys.time() - t0, units = "secs") > 90) break
  Sys.sleep(0.1)
}

status <- ns$job_status(m, id)
if (!identical(status$state, "done")) stop("the job did not finish: ", status$state, " ", status$error)
stopifnot(identical(ns$read_progress(m$jobs[[id]]$progress_file)$value, 1))
out <- ns$job_collect(m, id)
stopifnot(identical(out$value$messages, "Skipped: Vegetation structure is not wired to a retrieval endpoint yet."))
cat("JOB OK\n")
