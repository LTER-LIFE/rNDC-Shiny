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
         if (async_enabled()) "It runs in the background: the progress bar shows where it is."
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

# What a message of a retrieval function says about the progress: the text to show, and the position of
# the bar if the message ends in a count like "(3/10)" (the 3rd of 10 requests is starting). `index` is
# the row of the overview and `n` the number of rows, so the bar covers all of them.
progress_from_message <- function(message, label, index, n) {
  message <- trimws(gsub("\\s+", " ", message))
  if (!nzchar(message)) return(list(detail = NULL, value = NULL))

  value <- NULL
  count <- regmatches(message, regexec("\\((\\d+)/(\\d+)\\)$", message))[[1]]
  if (length(count) == 3 && as.numeric(count[3]) > 0) {
    value <- (index - 1 + (as.numeric(count[2]) - 1) / as.numeric(count[3])) / n
    value <- min(1, max(0, value))
  }
  shown <- if (nchar(message) > 70) paste0(substr(message, 1, 67), "...") else message
  list(detail = paste0(label, ": ", shown), value = value)
}

# Evaluate `expr` and show the messages that it emits (rNDC says e.g. "Downloading 2024-01-01 -> 2024-07-18
# (1/10)") as the progress detail. The messages still reach the console. Some functions emit many (one per
# day), so the page is updated at most four times a second, unless the message moves the bar.
with_progress_messages <- function(expr, label, index, n, report = report_progress) {
  last <- 0
  withCallingHandlers(expr, message = function(m) {
    p <- progress_from_message(conditionMessage(m), label, index, n)
    now <- as.numeric(Sys.time())
    if (!is.null(p$detail) && (!is.null(p$value) || now - last > 0.25)) {
      last <<- now
      report(detail = p$detail, value = p$value)
    }
  })
}
