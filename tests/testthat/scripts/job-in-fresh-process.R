# Run by test-async.R in a separate R process: a job is sent to a fresh background process (`multisession`)
# with the same objects as the app sends. It runs offline (a dataset without retrieval) and prints "JOB OK".
# This is a script, and not code of the test itself, because what goes wrong when something refers to the
# session that sends it (e.g. a closure with an unevaluated argument) depends on where it is created.

suppressMessages(library(rNDC.Shiny))
ns <- asNamespace("rNDC.Shiny")
future::plan(future::multisession, workers = 2)

pfile <- tempfile()
writer <- ns$progress_writer(pfile)
job <- ns$retrieve_and_package
poly <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 0)))), crs = 4326))
ov <- tibble::tibble(dataset = "Vegetation structure", view = "Geodata", year = NA_integer_, polygon = "p",
                     wkt = sf::st_as_text(sf::st_geometry(poly)), polygon_sf = list(poly),
                     date_from = as.Date(NA), date_to = as.Date(NA))

res <- NULL
err <- NULL
p <- promises::future_promise(
  job(ov, NULL, FALSE, NULL, "t", "t", progress = writer, return_data = TRUE, pack = TRUE),
  globals = list(job = job, ov = ov, writer = writer), seed = NULL
)
promises::then(p, function(r) res <<- r, function(e) err <<- e)
t0 <- Sys.time()
while (is.null(res) && is.null(err) && as.numeric(Sys.time() - t0, units = "secs") < 90) later::run_now(0.05)

if (!is.null(err)) stop("the job failed: ", conditionMessage(err))
if (is.null(res)) stop("the job did not finish")
stopifnot(identical(res$messages, "Skipped: Vegetation structure is not wired to a retrieval endpoint yet."))
stopifnot(ns$read_progress(pfile)$value == 1)
cat("JOB OK\n")
