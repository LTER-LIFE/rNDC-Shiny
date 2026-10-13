# "Which datasets exist for the selected area?": a dialog with the table of rNDC::ndc_available() for the selected
# polygons and, if the user wants, a period (R/catalogue.R). It is asked when the dialog is opened and when the area
# or period changes, not on every selection, and cached per area and period.
server_catalogue <- function(input, output, session, state, helpers) {
  selected_polygons <- state$selected_polygons
  mytoken <- state$mytoken

  observeEvent(input$show_catalogue, {
    showModal(modalDialog(
      title = "Datasets for the selected area",
      uiOutput("catalogue_note"),
      checkboxInput("catalogue_limit", "Only datasets with data in a period", value = FALSE),
      conditionalPanel("input.catalogue_limit",
                       dateRangeInput("catalogue_period", NULL, start = Sys.Date() - 31, end = Sys.Date() - 1,
                                      min = weather_min_date, max = Sys.Date())),
      tableOutput("catalogue_table"),
      easyClose = TRUE, footer = modalButton("Close"), size = "l"
    ))
  })

  # the period, or NULL
  catalogue_trange <- reactive({
    period <- input$catalogue_period
    if (isTRUE(input$catalogue_limit) && length(period) == 2 && !anyNA(period)) as.character(period) else NULL
  })

  # list(table, error): the table of rNDC::ndc_available() for the selected area, or the reason it is not there
  catalogue_data <- reactive({
    sel <- selected_polygons()
    if (is.null(sel) || nrow(sel) == 0) return(list(table = NULL, error = "Select an area on the map first."))
    geometry <- sf::st_union(sf::st_geometry(sel))
    trange <- catalogue_trange()
    key <- cache_key("catalogue", sf::st_as_text(geometry, digits = 6), paste(trange, collapse = "/"))
    table <- cache_get(key)
    if (is.null(table)) {
      table <- tryCatch(
        withProgress(message = "Checking the datasets...", value = 0.5, rNDC::ndc_available(geometry, trange, token = mytoken)),
        error = function(e) e
      )
      if (inherits(table, "error")) {
        return(list(table = NULL, error = paste0("The datasets could not be listed: ", conditionMessage(table))))
      }
      cache_set(key, table)
    }
    list(table = table, error = NULL)
  })

  output$catalogue_note <- renderUI({
    data <- catalogue_data()
    if (is.null(data$table)) return(tags$p(data$error, style = "color:#b26a00;"))
    tags$p(catalogue_note(data$table, catalogue_trange()), style = "font-size:13px;")
  })

  # the text of the datasets comes from services: it is escaped, not read as HTML
  output$catalogue_table <- renderTable({
    catalogue_display(catalogue_data()$table)
  }, striped = TRUE, na = "", sanitize.text.function = htmltools::htmlEscape)

  invisible(NULL)
}
