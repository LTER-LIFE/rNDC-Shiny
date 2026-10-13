# The list of all datasets for the selected area (and period): the table of rNDC::ndc_available() as the app
# shows it. The functions here use no Shiny objects (see R/server_catalogue.R for the dialog).

# "yes", "no" or "unknown", with a mark. A dataset of another service (not `verified`) is said to be expected or
# not: that comes from its known coverage, not from a check of the service.
catalogue_status <- function(available, verified) {
  text <- ifelse(is.na(available), "unknown", ifelse(available, "yes", "no"))
  mark <- ifelse(is.na(available), "?", ifelse(available, "\u2713", "\u2717"))
  detail <- ifelse(verified | is.na(available), "", ifelse(available, " (expected)", " (outside its coverage)"))
  paste0(mark, " ", text, detail)
}

# The period of a dataset: "from 2017-05-26", "2024-01-01 to 2024-12-31", or "" (unknown)
catalogue_period <- function(start, end) {
  day <- function(x) ifelse(is.na(x), NA_character_, format(x, "%Y-%m-%d"))
  from <- day(start)
  to <- day(end)
  ifelse(!is.na(from) & !is.na(to), paste(from, "to", to), ifelse(!is.na(from), paste("from", from), ""))
}

# The table to show, from the result of rNDC::ndc_available(): the datasets with data first, then the ones that
# could not be checked, then the others; in the order of the result within these groups.
catalogue_display <- function(available) {
  if (is.null(available) || nrow(available) == 0) {
    return(data.frame(Dataset = character(), Source = character(), `Data for the area` = character(),
                      Items = character(), Period = character(), License = character(), check.names = FALSE))
  }
  rank <- ifelse(is.na(available$available), 2L, ifelse(available$available, 1L, 3L))
  available <- available[order(rank, seq_len(nrow(available))), , drop = FALSE]
  title <- ifelse(is.na(available$title) | !nzchar(available$title), available$id, available$title)
  data.frame(
    Dataset = title,
    Source = available$source,
    `Data for the area` = catalogue_status(available$available, available$verified),
    Items = ifelse(is.na(available$n_items), "", format(available$n_items, big.mark = ",", scientific = FALSE, trim = TRUE)),
    Period = catalogue_period(available$start, available$end),
    License = ifelse(is.na(available$license), "", available$license),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

# The sentence above the table
catalogue_note <- function(available, trange = NULL) {
  period <- if (is.null(trange)) "" else paste0(" in the period ", trange[1], " to ", trange[2])
  n_yes <- sum(available$available, na.rm = TRUE)
  external <- any(!available$verified)
  paste0(n_yes, " of ", nrow(available), " datasets have data for the selected area", period, ".",
         if (external) paste0(" The datasets of other services (AgroDataCube, GroenMonitor) are listed by the coverage ",
                              "that is known for them: nothing is asked of those services.") else "",
         " The periods are as published with the datasets and can be out of date.")
}
