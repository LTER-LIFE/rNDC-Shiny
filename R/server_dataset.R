# Choosing a dataset and its settings, and adding it to the overview. Returns `build_controls_for()`.
server_dataset <- function(input, output, session, state, helpers) {
  drawn_features <- state$drawn_features
  selected_polygons <- state$selected_polygons
  uploaded_polys <- state$uploaded_polys
  overview <- state$overview
  adc_available <- state$adc_available

  shinyjs::disable(selector = "a[data-value='Statistics']")

  lapply(names(dataset_info), function(ds_name) {
    btn_id <- paste0("info_ds_", gsub("[^A-Za-z0-9]", "_", ds_name))
    observeEvent(input[[btn_id]], {
      info <- dataset_info[[ds_name]]
      showModal(modalDialog(
        title = info$title,
        HTML(paste0("<p>", info$description, "</p><p><b>Notes:</b> ", info$notes, "</p>")),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    }, ignoreInit = TRUE)
  })

  session$sendCustomMessage("ndc_toggle_add_button", FALSE)

  if (!adc_available) {
    showNotification(
      "No ADC_TOKEN set: Weather, Soil map, AHN and Agricultural fields are unavailable.",
      type = "warning", duration = 10
    )
  }

  observe({
    ds <- NULL
    try({ ds <- input$selected_dataset }, silent = TRUE)

    sel <- selected_polygons()
    enabled <- FALSE

    if (!is.null(ds) && nzchar(ds) && !is.null(sel) && nrow(sel) > 0) {
      date_ok <- TRUE

      if (ds == "Weather") {
        try({
          mode <- input$weather_mode
          if (is.null(mode) || mode == "single") {
            date_ok <- !is.null(input$weather_date) && !is.na(as.Date(input$weather_date))
          } else {
            pr <- input$weather_period
            date_ok <- !is.null(pr) && length(pr) == 2 && !is.na(as.Date(pr[1])) && !is.na(as.Date(pr[2]))
          }
        }, silent = TRUE)
      } else if (ds == "NDVI") {
        try({
          mode <- input$ndvi_mode
          if (is.null(mode) || mode == "single") {
            date_ok <- !is.null(input$ndvi_year) && !is.null(input$ndvi_month)
          } else {
            date_ok <- !is.null(input$ndvi_from_year) && !is.null(input$ndvi_from_month) &&
              !is.null(input$ndvi_to_year) && !is.null(input$ndvi_to_month)
          }
        }, silent = TRUE)
      } else if (ds == "Nitrogen") {
        try({
          date_ok <- !is.null(input$nitrogen_year) && nzchar(as.character(input$nitrogen_year))
        }, silent = TRUE)
      } else if (ds == "Agricultural fields") {
        try({ date_ok <- !is.null(input$selected_year) }, silent = TRUE)
      }

      enabled <- isTRUE(date_ok)
      if (!adc_available && ds %in% adc_datasets) enabled <- FALSE
    }

    session$sendCustomMessage("ndc_toggle_add_button", enabled)
  })

  observeEvent(input$selected_dataset, {
    val <- if (is.null(input$selected_dataset) || identical(input$selected_dataset, "")) NULL else input$selected_dataset
    session$sendCustomMessage("ndc_select_dataset", val)
  }, ignoreNULL = FALSE)

  output$dataset_metadata <- renderUI({
    req(input$selected_dataset)
    ds <- input$selected_dataset

    make_ds_tabset <- function(ds_name) {
      tid <- make_tab_id(ds_name)
      header_text <- paste0("Configure data request - ", ds_name)
      header_tag <- tags$h4(header_text, style = "color: #1f5a8a; margin-top: 6px; margin-bottom: 8px;")
      tab_names <- tabs_for_dataset(ds_name)
      tab_panels <- lapply(tab_names, function(tn) {
        tabPanel(title = tn, value = tn, uiOutput(make_target_id(ds_name, tn)))
      })
      tagList(
        header_tag,
        do.call(tabsetPanel, c(list(id = tid, type = "tabs"), tab_panels))
      )
    }

    make_ds_tabset(ds)
  })

  build_controls_for <- function(ds, tab = NULL) {
    if (is.null(ds) || ds == "") return(NULL)
    if (!adc_available && ds %in% adc_datasets) {
      return(tagList(tags$div(class = "dataset-controls",
                              helpText("This dataset needs an AgroDataCube token: set ADC_TOKEN and restart the app."))))
    }
    this_year <- as.integer(format(Sys.Date(), "%Y"))
    last_year <- this_year - 1L  # latest complete year (AgroDataCube fields)
    ndvi_min_year <- as.integer(format(ndvi_min_month, "%Y"))

    if (ds == "Agricultural fields") {
      tagList(tags$div(class = "dataset-controls", numericInput("selected_year", "Select year:", value = last_year, min = fields_min_year, max = last_year)))

    } else if (ds == "Nitrogen") {
      nitrogen_years <- get_nitrogen_years()
      tagList(tags$div(
        class = "dataset-controls",
        selectInput("nitrogen_year", "Select year:", choices = nitrogen_years,
                    selected = nitrogen_years[1], multiple = FALSE),
        tags$div(style = "margin-top: 6px; color: #5a6472;",
                 paste0("Retrieval will return all nitrogen rasters: ",
                        paste(nitrogen_layer_choices, collapse = ", "), "."))
      ))

    } else if (ds == "Land Use") {
      years <- get_landuse_years()
      default <- as.character(landuse_default_year)
      tagList(tags$div(class = "dataset-controls",
                       selectInput("landuse_year", "Select year:", choices = years,
                                   selected = if (default %in% years) default else utils::tail(years, 1))))

    } else if (ds == "Weather") {
      tagList(
        tags$div(class = "dataset-controls",
                 radioButtons("weather_mode", label = NULL, choices = c("Single date" = "single", "Period" = "period"), selected = "single", inline = TRUE),
                 conditionalPanel(condition = "input.weather_mode == 'single'",
                                  dateInput("weather_date", "Date (single):", value = Sys.Date() - 1, min = weather_min_date, max = Sys.Date())),
                 conditionalPanel(condition = "input.weather_mode == 'period'",
                                  dateRangeInput("weather_period", "From - To:", start = Sys.Date() - 31, end = Sys.Date() - 1, min = weather_min_date, max = Sys.Date()))
        )
      )

    } else if (ds == "NDVI") {
      is_stats <- !is.null(tab) && tolower(as.character(tab)) == "statistics"
      stats_note <- if (is_stats) {
        helpText("Statistics are monthly NDVI summaries (mean/std) per polygon, available for LTER and SNL project areas only.")
      } else {
        helpText("Geodata returns monthly average NDVI raster layers.")
      }
      tagList(
        tags$div(class = "dataset-controls",
                 stats_note,
                 radioButtons("ndvi_mode", "NDVI query type:", choices = c("Single month" = "single", "Range of months" = "range"), selected = "single", inline = TRUE),
                 conditionalPanel(condition = "input.ndvi_mode == 'single'",
                                  numericInput("ndvi_year", "Year:", value = last_year, min = ndvi_min_year, max = this_year),
                                  numericInput("ndvi_month", "Month (1-12):", value = 1, min = 1, max = 12)),
                 conditionalPanel(condition = "input.ndvi_mode == 'range'",
                                  fluidRow(
                                    column(6, numericInput("ndvi_from_year", "From Year:", value = last_year, min = ndvi_min_year, max = this_year), numericInput("ndvi_from_month", "From Month (1-12):", value = 1, min = 1, max = 12)),
                                    column(6, numericInput("ndvi_to_year", "To Year:", value = last_year, min = ndvi_min_year, max = this_year), numericInput("ndvi_to_month", "To Month (1-12):", value = 12, min = 1, max = 12))
                                  ))
        )
      )

    } else {
      tagList(tags$div(class = "dataset-controls", helpText("This dataset does not require a year or date selection.")))
    }
  }

  for (ds_name in all_dataset_names) {
    for (tab_nm in tabs_for_dataset(ds_name)) {
      local({
        dsn <- ds_name
        tab_name <- tab_nm
        tgt <- make_target_id(dsn, tab_name)
        tab_input_id <- make_tab_id(dsn)
        output[[tgt]] <- renderUI({
          if (is.null(input$selected_dataset) || input$selected_dataset != dsn) return(NULL)
          current_tab <- if (!is.null(input[[tab_input_id]])) input[[tab_input_id]] else default_tab_for_dataset(dsn)
          if (is.null(current_tab) || tolower(as.character(current_tab)) != tolower(tab_name)) return(NULL)
          build_controls_for(dsn, tab_name)
        })
      })
    }
  }

  observeEvent(input$add_dataset, {
    sel <- selected_polygons()
    if (is.null(sel) || nrow(sel) == 0) {
      showNotification("\u26a0 Please select or draw at least one polygon first.", type = "error", duration = 5)
      return(NULL)
    }
    req(input$selected_dataset)
    if (!adc_available && input$selected_dataset %in% adc_datasets) {
      showNotification("This dataset needs an AgroDataCube token (ADC_TOKEN).", type = "error", duration = 5)
      return(NULL)
    }

    year_val <- NA_integer_
    date_from_val <- as.Date(NA)
    date_to_val <- as.Date(NA)

    if (input$selected_dataset == "Agricultural fields") {
      req(input$selected_year)
      year_val <- as.integer(input$selected_year)

    } else if (input$selected_dataset == "Nitrogen") {
      req(input$nitrogen_year)
      year_val <- as.integer(input$nitrogen_year)

    } else if (input$selected_dataset == "Land Use") {
      # the default until the year input exists
      year_val <- if (is.null(input$landuse_year)) as.integer(landuse_default_year) else as.integer(input$landuse_year)

    } else if (input$selected_dataset == "NDVI") {
      single <- is.null(input$ndvi_mode) || input$ndvi_mode == "single"
      ym <- if (single) {
        c(input$ndvi_year, input$ndvi_month, input$ndvi_year, input$ndvi_month)
      } else {
        c(input$ndvi_from_year, input$ndvi_from_month, input$ndvi_to_year, input$ndvi_to_month)
      }
      if (length(ym) != 4 || any(is.na(ym)) || any(ym[c(2, 4)] < 1 | ym[c(2, 4)] > 12)) {
        showNotification("Please enter valid years and months (1-12).", type = "error", duration = 5)
        return(NULL)
      }
      date_from_val <- as.Date(sprintf("%04d-%02d-01", as.integer(ym[1]), as.integer(ym[2])))
      date_to_val <- as.Date(sprintf("%04d-%02d-01", as.integer(ym[3]), as.integer(ym[4]))) + months(1) - 1
      if (date_from_val > date_to_val) {
        showNotification("The NDVI start month must not be after the end month.", type = "error", duration = 5)
        return(NULL)
      }
      current_month <- as.Date(format(Sys.Date(), "%Y-%m-01"))
      if (date_from_val > Sys.Date()) {
        showNotification("The NDVI start month is in the future.", type = "error", duration = 5)
        return(NULL)
      }
      if (date_to_val >= current_month + months(1)) {
        # no data for future months: avoid one failing download per future day
        date_to_val <- current_month + months(1) - 1
        showNotification("The NDVI end month was set to the current month.", type = "warning", duration = 5)
      }
      if (date_to_val < ndvi_min_month) {
        showNotification("NDVI data is available from May 2017.", type = "error", duration = 5)
        return(NULL)
      }
      if (date_from_val < ndvi_min_month) {
        # no data before the first observations: avoid one failing download per day
        date_from_val <- ndvi_min_month
        showNotification("The NDVI start month was set to May 2017, the first month with data.",
                         type = "warning", duration = 5)
      }

    } else if (input$selected_dataset == "Weather") {
      if (is.null(input$weather_mode) || input$weather_mode == "single") {
        date_from_val <- as.Date(input$weather_date)
        date_to_val <- as.Date(input$weather_date)
      } else {
        date_from_val <- as.Date(input$weather_period[1])
        date_to_val <- as.Date(input$weather_period[2])
      }
    }

    ds_label <- input$selected_dataset
    tab_input_id <- make_tab_id(ds_label)
    default_tab <- default_tab_for_dataset(input$selected_dataset)
    cur_tab_val <- if (!is.null(isolate(input[[tab_input_id]]))) isolate(input[[tab_input_id]]) else default_tab
    view_label <- if (tolower(as.character(cur_tab_val)) == "statistics") "Statistics" else "Geodata"

    ov <- overview()
    new_rows <- list()

    for (i in seq_len(nrow(sel))) {
      poly <- sel[i, , drop = FALSE]
      assigned_name <- if ("source_name" %in% names(poly) && !is.na(poly$source_name[1]) && nzchar(as.character(poly$source_name[1]))) {
        as.character(poly$source_name[1])
      } else {
        "Own polygon"
      }

      is_dup <- FALSE
      if (nrow(ov) > 0) {
        same_poly <- ov$wkt == poly$wkt[1]
        same_ds <- ov$dataset == ds_label
        same_view <- ov$view == view_label
        same_year <- vapply(seq_len(nrow(ov)), function(j) same_na(ov$year[j], year_val), logical(1))
        same_from <- vapply(seq_len(nrow(ov)), function(j) same_na(ov$date_from[j], date_from_val), logical(1))
        same_to <- vapply(seq_len(nrow(ov)), function(j) same_na(ov$date_to[j], date_to_val), logical(1))
        same_name <- ov$polygon == assigned_name
        is_dup <- any(same_ds & same_view & same_poly & same_name & same_year & same_from & same_to)
      }

      if (!is_dup) {
        new_rows[[length(new_rows) + 1]] <- tibble::tibble(
          dataset = ds_label,
          view = view_label,
          year = year_val,
          polygon = assigned_name,
          wkt = poly$wkt[1],
          polygon_sf = list(poly),
          date_from = date_from_val,
          date_to = date_to_val
        )
      }
    }

    if (length(new_rows) > 0) {
      added <- dplyr::bind_rows(new_rows)
      # the whole overview is retrieved at once, so it is the total that counts
      seconds <- sum(estimate_row_seconds(ov$dataset, ov$view, ov$date_from, ov$date_to),
                     estimate_row_seconds(added$dataset, added$view, added$date_from, added$date_to))
      if (seconds > max_request_seconds()) {
        showNotification(too_long_retrieval_message(seconds), type = "error", duration = 12)
        return(NULL)
      }
      overview(dplyr::bind_rows(ov, added))
      if (seconds > warn_request_seconds) {
        showNotification(long_retrieval_warning(seconds), type = "warning", duration = 10)
      }
    } else {
      showNotification("Selected dataset(s) already present for the selected polygon(s) with the same settings.", type = "message")
    }
  })

  observe({
    disable_stats <- FALSE
    df <- drawn_features()
    up <- uploaded_polys()
    if (!is.null(df) && nrow(df) > 0) disable_stats <- TRUE
    if (!is.null(up) && nrow(up) > 0) disable_stats <- TRUE
    session$sendCustomMessage("ndc_toggle_statistics", disable_stats)
  })

  # Does the selected area have data for the selected raster dataset (Land Use, Nitrogen)? The number of
  # STAC items that intersect it, for the selected year. Debounced, so that selecting polygons one after
  # another does not make a request for each.
  availability <- reactive({
    ds <- input$selected_dataset
    sel <- selected_polygons()
    collections <- if (is.null(ds)) NULL else availability_collections()[[ds]]
    if (is.null(collections) || is.null(sel) || nrow(sel) == 0) return(NULL)

    year <- switch(ds, "Land Use" = input$landuse_year, "Nitrogen" = input$nitrogen_year)
    list(dataset = ds, year = year,
         counts = items_in_area(collections, sf::st_union(sf::st_geometry(sel)), year))
  }) %>% debounce(400)

  output$availability <- renderUI({
    a <- availability()
    if (is.null(a) || all(is.na(a$counts))) return(NULL)

    what <- if (!is.null(a$year) && nzchar(a$year)) paste0(a$dataset, " data for ", a$year) else paste0(a$dataset, " data")
    style <- "margin-top:8px; font-size:13px; color:"
    missing <- names(a$counts)[!is.na(a$counts) & a$counts == 0]
    if (length(missing) > 0) {
      layers <- if (length(a$counts) > 1) paste0(" (", paste(missing, collapse = ", "), ")") else ""
      tags$div(style = paste0(style, "#b26a00;"),
               paste0("No ", what, " found for the selected area", layers, "."))
    } else {
      tags$div(style = paste0(style, "#2e7d32;"), paste0(what, " is available for the selected area."))
    }
  })

  invisible(list(build_controls_for = build_controls_for))
}
