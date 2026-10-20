# Which datasets have data for the selected area: the check of R/availability.R when the "Check data availability" button is
# clicked (all datasets, for the selected polygons) and when a dataset is added to the overview (that dataset, for each
# polygon). The result is shown as marks in the dataset menu and as the sentence under the settings of the selected
# dataset. They stay until a check is made again, or until the selection of polygons, or a year or date (or the distance
# of the Weather settings), changes: then they were for something else, so they are cleared, with a warning that the
# availability should be evaluated again (nothing is checked). Returns list(check_added =): the check of a dataset that is
# being added.

# The longest (seconds) that a request of the check may take: a slow service must not hold the app for long
availability_timeout <- 20

server_availability <- function(input, output, session, state, helpers) {
  selected_polygons <- state$selected_polygons
  availability_state <- state$availability
  weather_max_km <- state$weather_max_km
  check_enabled <- state$check_enabled
  selection_issues <- state$selection_issues
  mytoken <- state$mytoken
  agro_token <- state$agro_token

  # The button "Check data availability" can be clicked when polygons are selected (a project's, an uploaded file's or a
  # drawn one: whatever is selected on the map), like Add to overview
  observe({
    polygons <- selected_polygons()
    enabled <- !is.null(polygons) && nrow(polygons) > 0
    check_enabled(enabled)
    session$sendCustomMessage("ndc_toggle_check_button", enabled)
  })

  # The largest distance to the weather station (an input of the Weather settings). A value that is not a positive
  # number is ignored, and the last good one is kept: the input is gone when another dataset is chosen.
  observeEvent(input$weather_max_km, {
    km <- suppressWarnings(as.numeric(input$weather_max_km))
    if (length(km) == 1 && is.finite(km) && km > 0) weather_max_km(km)
  })

  # the info button next to "Choose dataset(s)"
  observeEvent(input$info_availability, {
    showModal(modalDialog(title = "Data availability", availability_help(), easyClose = TRUE, footer = modalButton("Close")))
  })

  # The marks and the sentence go, and the user is warned (yellow, not the red of an error) that the availability should be
  # evaluated again; nothing is checked. Nothing is said when there were no marks.
  clear_marks <- function(changed) {
    had_marks <- !is.null(isolate(availability_state()))
    availability_state(NULL)
    session$sendCustomMessage("ndc_availability", NULL)
    if (had_marks) {
      showNotification(paste0("The ", changed, " changed, so the availability of the datasets is no longer known. ",
                              "Click Check data availability to evaluate it again."), type = "warning", duration = 10)
    }
  }

  # Another selection of polygons
  observeEvent(selected_polygons(), clear_marks("selected area"), ignoreNULL = FALSE, ignoreInit = TRUE)

  # Another year, date or distance. The inputs of a dataset only exist while it is chosen: one that appears (or goes) is no
  # change, only a new value of an input that had one is.
  settings <- reactive(list(
    selected_year = input$selected_year, landuse_year = input$landuse_year, nitrogen_year = input$nitrogen_year,
    ndvi_mode = input$ndvi_mode, ndvi_year = input$ndvi_year, ndvi_month = input$ndvi_month,
    ndvi_from_year = input$ndvi_from_year, ndvi_from_month = input$ndvi_from_month,
    ndvi_to_year = input$ndvi_to_year, ndvi_to_month = input$ndvi_to_month,
    weather_mode = input$weather_mode, weather_date = input$weather_date, weather_period = input$weather_period,
    weather_max_km = weather_max_km()
  ))
  last_settings <- NULL
  observeEvent(settings(), {
    now <- settings()
    changed <- !is.null(last_settings) && any(vapply(names(now), function(k) {
      !is.null(now[[k]]) && !is.null(last_settings[[k]]) && !identical(now[[k]], last_settings[[k]])
    }, NA))
    last_settings <<- now
    if (changed) clear_marks("year, date or distance")
  }, ignoreNULL = FALSE)

  # The check of `area` (an sf object) for the `datasets`: the table, or NULL if it could not be made
  check <- function(area, datasets = all_dataset_names, years = list(), view = NULL, progress = NULL, period = NULL) {
    tryCatch(
      httr::with_config(httr::timeout(availability_timeout),
                        availability_check(area, mytoken, agro_token, years, weather_max_km(), datasets = datasets, view = view,
                                           progress = progress, period = period)),
      error = function(e) {
        message("The availability check failed: ", conditionMessage(e))
        NULL
      }
    )
  }

  # Show `rows` (the check of some datasets, made for the polygons called `where`) in the menu and under the settings,
  # in addition to the earlier checks of the other datasets
  show_rows <- function(rows, where) {
    rows$area <- where
    table <- availability_merge(isolate(availability_state()), rows)
    availability_state(table)
    session$sendCustomMessage("ndc_availability", list(pending = FALSE, datasets = as.list(table$dataset),
                                                       status = as.list(table$status), detail = as.list(table$detail)))
  }

  polygon_names <- function(area) {
    names <- if ("source_name" %in% names(area)) as.character(area$source_name) else "Own polygon"
    paste(unique(names[!is.na(names)]), collapse = ", ")
  }

  # A selection above the limits: with too many polygons all the datasets are disabled in the menu and in the Add button,
  # with too large an area all the datasets except Weather (which has no area limit), until the selection is within the limits;
  # the user is told which limits it is above.
  last_issues <- character(0)
  observeEvent(selection_issues(), {
    issues <- selection_issues()
    session$sendCustomMessage("ndc_over_limit", list(all = "count" %in% names(issues), others = length(issues) > 0))
    if (length(issues) > 0 && !identical(issues, last_issues)) {
      disabled <- if ("count" %in% names(issues)) "All the datasets, Weather included, are" else "All the datasets except Weather are"
      showNotification(paste0("The selection is above the limits: ", problems_text(issues), " ", disabled,
                              " disabled until the selected polygons are within the limits. ",
                              contact_sentence("larger areas or help")), type = "warning", duration = 15)
    }
    last_issues <<- issues
  }, ignoreNULL = FALSE)

  # the button: all the datasets, for the selected polygons (the dialog of R/server_catalogue.R opens with it). Nothing is
  # checked when the selection is above the limits: the dialog says which.
  observeEvent(input$check_availability, {
    sel <- selected_polygons()
    if (is.null(sel) || nrow(sel) == 0 || length(selection_issues()) > 0) return(NULL)
    years <- list("Agricultural fields" = input$selected_year, "Land Use" = input$landuse_year, "Nitrogen" = input$nitrogen_year)
    session$sendCustomMessage("ndc_availability", list(pending = TRUE))
    # the progress bar says how many of the datasets have been tested
    table <- withProgress(message = "Checking datasets...", value = 0, {
      check(sel, years = years, progress = function(done, total) {
        setProgress(value = done / total, detail = sprintf("%d of %d datasets tested", done, total))
      })
    })
    if (is.null(table)) {
      availability_state(NULL)
      session$sendCustomMessage("ndc_availability", list(pending = FALSE, datasets = list()))
      showNotification("The availability of the datasets could not be checked.", type = "warning", duration = 8)
      return(NULL)
    }
    show_rows(table, polygon_names(sel))
  })

  # A dataset that is being added: `added` has one row for each polygon (the columns of the overview). Returns, for each
  # row, whether the services have no data for it (then it is not added); a check that could not be made does not stop it.
  # The result is also shown, for the dataset, as the marks (the status the polygons share, or "partial").
  check_added <- function(added, dataset, view, year) {
    years <- list()
    if (dataset %in% c("Agricultural fields", "Land Use", "Nitrogen") && length(year) == 1 && !is.na(year)) years[[dataset]] <- year
    # one dataset, so the progress bar counts the polygons
    rows <- withProgress(message = paste0("Checking ", dataset, "..."), value = 0, {
      lapply(seq_len(nrow(added)), function(i) {
        setProgress(value = (i - 1) / nrow(added), detail = sprintf("%d of %d polygons tested", i - 1, nrow(added)))
        # the period asked for (two dates), for the datasets that have one
        period <- c(added$date_from[i], added$date_to[i])
        if (anyNA(period)) period <- NULL
        table <- check(added$polygon_sf[[i]], datasets = dataset, years = years, view = view, period = period)
        if (is.null(table)) NULL else table[table$dataset == dataset, , drop = FALSE]
      })
    })
    if (all(vapply(rows, is.null, NA))) return(list(none = rep(FALSE, nrow(added)), rows = rows))
    show_rows(availability_combine(rows), paste(unique(added$polygon), collapse = ", "))
    none <- vapply(rows, function(r) !is.null(r) && identical(r$status, "no"), NA)
    list(none = none, rows = rows)
  }

  output$availability <- renderUI({
    row <- availability_of(availability_state(), input$selected_dataset)
    if (is.null(row) || row$status %in% c("unknown", "na") || !nzchar(row$detail)) return(NULL)
    colour <- if (row$status == "yes") "#2e7d32" else "#b26a00"
    tags$div(style = "margin-top:8px;",
             tags$div(style = paste0("font-size:13px; color:", colour, ";"), row$detail),
             if (!is.null(row$area) && !is.na(row$area) && nzchar(row$area)) {
               tags$div(style = "font-size:11px; color:#888;", paste0("Checked for: ", row$area))
             })
  })

  list(check_added = check_added)
}
