# How long retrieving the overview takes, estimated before it is added, and the progress shown while it runs.
# A retrieval keeps the app busy until it is done, so a long one is warned about, and a too long one refused.

# Estimated seconds to retrieve each overview row (the vectors have one element per row). Only the datasets
# that make many requests count: Weather (one request per `weather_chunk_days` days) and the NDVI rasters
# (one download per day). The others take a few seconds.
estimate_row_seconds <- function(dataset, view, date_from, date_to) {
  vapply(seq_along(dataset), function(i) {
    from <- date_from[i]
    to <- date_to[i]
    days <- if (is.na(from) || is.na(to)) NA_real_ else as.numeric(to - from) + 1

    if (dataset[i] == "Weather") {
      if (is.na(days)) days <- 1
      return(max(1, ceiling(days / weather_chunk_days)) * request_seconds[["weather_chunk"]])
    }
    if (dataset[i] == "NDVI" && tolower(as.character(view[i])) != "statistics") {
      return(if (is.na(days)) 0 else days * request_seconds[["ndvi_day"]])
    }
    0
  }, numeric(1))
}

# "less than a minute", "about 1 minute", "about 5 minutes"
format_duration <- function(seconds) {
  if (seconds < 60) return("less than a minute")
  minutes <- round(seconds / 60)
  paste0("about ", minutes, if (minutes == 1) " minute" else " minutes")
}

long_retrieval_warning <- function(seconds) {
  paste0("Retrieving the datasets in the overview will take ", format_duration(seconds), ". ",
         if (async_enabled()) "It runs in the background: the progress bar shows where it is, and you can cancel it."
         else "The app is busy until it is done.")
}

too_long_retrieval_message <- function(seconds) {
  paste0("The overview would take ", format_duration(seconds), " to retrieve, more than the limit of ",
         format_duration(max_request_seconds()), ". Choose a shorter period, or download the datasets ",
         "in the overview first and then clear it before adding more.")
}

# ---- Progress while retrieving ----

# Show `detail` and/or move the progress bar to `value` (0-1). Only works inside withProgress().
report_progress <- function(detail = NULL, value = NULL) {
  shiny::setProgress(value = value, detail = detail)
}

# What a report of a retrieval function (rNDC::ndc_with_progress(): "Downloading ... (3/10)", request 3 of 10 is
# starting) says about the progress: the text to show, and the position of the bar when the request is counted.
# `index` is the row of the overview and `n` the number of rows, so the bar covers all of them.
progress_from_report <- function(message, current, total, label, index, n) {
  message <- trimws(gsub("\\s+", " ", message))
  if (!nzchar(message)) return(list(detail = NULL, value = NULL))

  value <- NULL
  if (!is.na(current) && !is.na(total) && total > 0) {
    value <- min(1, max(0, (index - 1 + (current - 1) / total) / n))
  }
  shown <- if (nchar(message) > 70) paste0(substr(message, 1, 67), "...") else message
  list(detail = paste0(label, ": ", shown), value = value)
}

# Evaluate `expr` and show what the rNDC functions report (one report before each request) as the progress
# detail and the position of the bar. Reports without a count are passed on at most four times a second.
with_request_progress <- function(expr, label, index, n, report = report_progress) {
  last <- 0
  rNDC::ndc_with_progress(expr, report = function(message, current, total) {
    p <- progress_from_report(message, current, total, label, index, n)
    now <- as.numeric(Sys.time())
    if (!is.null(p$detail) && (!is.null(p$value) || now - last > 0.25)) {
      last <<- now
      report(detail = p$detail, value = p$value)
    }
  })
}
