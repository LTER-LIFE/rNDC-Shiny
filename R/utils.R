# What everything shares.

# A positive whole number from an environment variable, or `default`
env_count <- function(name, default) {
  value <- suppressWarnings(as.integer(Sys.getenv(name)))
  if (is.na(value) || value < 1) default else value
}

same_na <- function(x, y) {
  (is.na(x) && is.na(y)) || (!is.na(x) && !is.na(y) && identical(x, y))
}

safe_filename <- function(x) gsub("[^A-Za-z0-9_\\-]+", "_", x)

# ---- Where to turn when a limit is reached, or help is needed ----

ndc_website <- "https://ndc.wur.nl"

ndc_contact <- "LTER-LIFE@nioo.knaw.nl"

# A sentence for the messages about a limit: where to go for `what` ("larger areas or help")
contact_sentence <- function(what = "help") {
  paste0("For ", what, ", see ", ndc_website, " or contact ", ndc_contact, ".")
}

# The limits of the app, as they are now (they can be set with environment variables, see ndc_app()), as text
limits_text <- function() {
  paste0("up to ", format(max_polygons(), big.mark = ","), " selected polygons, with a total area of up to ",
         format(max_area_km2(), big.mark = ","), " km\u00b2 (Weather has no area limit), ",
         "a retrieval of ", retrieve_format_duration(max_request_seconds()), " at the most, and uploads of up to ",
         format(round(max_upload_mb()), trim = TRUE), " MB and ", max_upload_features, " polygons per file or layer")
}

# The paragraph with the limits and where to go (links), for the dialogs
contact_note <- function() {
  tags$p(style = "text-align:left; font-size:12px; color:#5a6472; margin:0 0 10px 0;",
         "The limits of the app: ", limits_text(), ". If your request is above them, or you need help, see ",
         tags$a(href = ndc_website, target = "_blank", rel = "noopener noreferrer", ndc_website), " or contact ",
         tags$a(href = paste0("mailto:", ndc_contact), ndc_contact), ".")
}

# The sentences about a selection above the limits (see selection_problems()), as one text
problems_text <- function(problems) paste(unname(problems), collapse = " ")
