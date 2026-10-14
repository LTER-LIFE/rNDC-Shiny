# Which datasets have data for the selected area: the check of R/availability.R when the selection changes (and
# when the year of a dataset changes), marks in the dataset menu, and the sentence under the settings of the
# selected dataset.
server_availability <- function(input, output, session, state, helpers) {
  selected_polygons <- state$selected_polygons
  availability_state <- state$availability
  weather_max_km <- state$weather_max_km
  mytoken <- state$mytoken
  agro_token <- state$agro_token

  # as soon as the selection changes the menu shows that the check is coming (it starts after a short pause, so
  # that selecting polygons one after another does not make a check for each)
  observeEvent(selected_polygons(), {
    sel <- selected_polygons()
    session$sendCustomMessage("ndc_availability", if (is.null(sel) || nrow(sel) == 0) NULL else list(pending = TRUE))
  }, ignoreNULL = FALSE)

  # The largest distance to the weather station (an input of the Weather settings). A value that is not a positive
  # number is ignored, and the last good one is kept: the input is gone when another dataset is chosen.
  observeEvent(input$weather_max_km, {
    km <- suppressWarnings(as.numeric(input$weather_max_km))
    if (length(km) == 1 && is.finite(km) && km > 0) weather_max_km(km)
  })

  checked <- reactive({
    sel <- selected_polygons()
    if (is.null(sel) || nrow(sel) == 0) return(NULL)
    years <- list("Agricultural fields" = input$selected_year, "Land Use" = input$landuse_year,
                  "Nitrogen" = input$nitrogen_year)
    tryCatch(availability_check(sel, mytoken, agro_token, years, weather_max_km()), error = function(e) NULL)
  }) %>% debounce(400)

  observe({
    table <- checked()
    availability_state(table)
    session$sendCustomMessage("ndc_availability", if (is.null(table)) {
      if (is.null(isolate(selected_polygons()))) NULL else list(pending = FALSE, datasets = list())
    } else {
      list(pending = FALSE, datasets = as.list(table$dataset), status = as.list(table$status),
           detail = as.list(table$detail))
    })
  })

  output$availability <- renderUI({
    row <- availability_of(availability_state(), input$selected_dataset)
    if (is.null(row) || row$status %in% c("unknown", "na") || !nzchar(row$detail)) return(NULL)
    colour <- if (row$status == "yes") "#2e7d32" else "#b26a00"
    tags$div(style = paste0("margin-top:8px; font-size:13px; color:", colour, ";"), row$detail)
  })

  invisible(NULL)
}
