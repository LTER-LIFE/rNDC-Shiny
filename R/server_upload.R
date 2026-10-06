# Uploading polygons: files, the choice of a layer in a multi-layer GeoPackage, and removing uploads.
server_upload <- function(input, output, session, state, helpers) {
  drawn_features <- state$drawn_features
  selected_polygons <- state$selected_polygons
  fixed_polys <- state$fixed_polys
  uploaded_polys <- state$uploaded_polys
  pending_gpkg <- state$pending_gpkg
  overview <- state$overview
  update_selected_highlights <- helpers$update_selected_highlights
  clear_map_polygons <- helpers$clear_map_polygons
  clear_project_selection_state <- helpers$clear_project_selection_state

  created_observers <- reactiveVal(character(0))  # the pending GeoPackages that have import observers

  assign_uploaded_colors_server <- function(up_sf) {
    assign_uploaded_colors(up_sf, drawn_features_val = drawn_features(), fixed_polys_val = fixed_polys())
  }

  observeEvent(input$upload, {
    files <- input$upload
    req(files)

    cur_fixed_max <- if (!is.null(fixed_polys())) max(as.integer(fixed_polys()$layer_id), na.rm = TRUE) else 0
    cur_uploaded_max <- if (!is.null(uploaded_polys())) max(as.integer(uploaded_polys()$layer_id), na.rm = TRUE) else 0
    cur_drawn_max <- if (!is.null(drawn_features())) max(as.integer(drawn_features()$layer_id), na.rm = TRUE) else 0
    start_id <- max(cur_fixed_max, cur_uploaded_max, cur_drawn_max, 0) + 1

    clear_project_selection_state()
    session$sendCustomMessage("ndc_force_clear_fixed_sidebar", NULL)
    clear_map_polygons(except = c("uploaded"))
    session$sendCustomMessage("ndc_select_fixed", NULL)

    res <- process_uploaded_files(files, start_layer_id = start_id)
    imported <- res$imported
    pending <- res$pending_gpkg

    if (!is.null(imported) && nrow(imported) > 0) {
      existing_max <- if (!is.null(uploaded_polys())) max(as.integer(uploaded_polys()$layer_id), na.rm = TRUE) else 0
      new_layer_id <- as.integer(existing_max + 1)
      imported$layer_id <- new_layer_id
      imported$wkt <- sf::st_as_text(sf::st_geometry(imported))
      imported$source <- "uploaded"
      imported$pending_pid <- NA_character_
      imported$pending_layer <- NA_character_
      base_name <- if (!is.null(files$name) && length(files$name) >= 1) files$name[1] else "Uploaded"
      imported <- assign_sequential_source_names(imported, base_name = base_name, overview(), fixed_polys(), uploaded_polys(), drawn_features())

      tmp_up <- if (is.null(uploaded_polys())) imported else dplyr::bind_rows(uploaded_polys(), imported)
      tmp_up <- assign_uploaded_colors_server(tmp_up)
      uploaded_polys(tmp_up)

      zoom_to_sf(imported)
    }

    if (length(pending) > 0) {
      cur_pending <- pending_gpkg()
      for (p in pending) {
        pid_hash <- tryCatch({ as.character(tools::md5sum(p$datapath)[[1]]) }, error = function(e) { paste0("tmp_", as.integer(stats::runif(1, 1e5, 1e6))) })
        pid <- paste0("pg_", pid_hash)

        if (!is.null(cur_pending) && pid %in% names(cur_pending)) {
          cur_pending[[pid]]$name <- p$name
          cur_pending[[pid]]$datapath <- p$datapath
          cur_pending[[pid]]$all_layers <- p$layers
          prev_imported <- cur_pending[[pid]]$imported
          if (is.null(prev_imported)) prev_imported <- character(0)
          cur_pending[[pid]]$layers <- setdiff(p$layers, prev_imported)
          cur_pending[[pid]]$original_name <- p$original_name
        } else {
          cur_pending[[pid]] <- list(
            id = pid,
            name = p$name,
            datapath = p$datapath,
            all_layers = p$layers,
            layers = p$layers,
            imported = character(0),
            original_name = p$original_name
          )
        }
      }
      pending_gpkg(cur_pending)
    }

    if (!is.null(uploaded_polys())) {
      leaflet::leafletProxy("map") %>% leaflet::clearGroup("uploaded") %>%
        leaflet::addPolygons(
          data = uploaded_polys(),
          group = "uploaded",
          color = ~color,
          fillColor = ~color,
          fillOpacity = 0.25,
          weight = 2,
          layerId = ~layer_id,
          label = ~source_name,
          labelOptions = leaflet::labelOptions(direction = "auto")
        )
    }

    n_imp <- if (!is.null(imported)) nrow(imported) else 0
    n_pending <- length(pending)
    msg_parts <- c()
    if (n_imp > 0) msg_parts <- c(msg_parts, paste0("Imported ", n_imp, " polygon(s)."))
    if (n_pending > 0) msg_parts <- c(msg_parts, paste0(n_pending, " geopackage(s) require layer selection. See upload panel below."))
    if (length(msg_parts) > 0) showNotification(paste(msg_parts, collapse = " "), type = "message", duration = 6)
  })

  output$upload_panel <- renderUI({
    pending <- pending_gpkg()
    uploaded <- uploaded_polys()

    tagList(
      if (length(pending) > 0) {
        wellPanel(
          h5("GeoPackage layer selection"),
          p("One or more uploaded GeoPackage files have multiple layers. Choose which layer to import and click Import."),
          lapply(names(pending), function(pid) {
            pinfo <- pending[[pid]]
            ns_import_id <- paste0("import_gpkg_", pid)
            select_id <- paste0("select_gpkg_", pid)
            tagList(
              tags$div(style = "margin-bottom:8px;",
                       strong(pinfo$name),
                       br(),
                       selectInput(select_id, "Choose layer:", choices = pinfo$layers, selected = ifelse(length(pinfo$layers) > 0, pinfo$layers[1], "")),
                       actionButton(ns_import_id, "Import selected layer", class = "btn-custom"))
            )
          })
        )
      } else NULL,

      wellPanel(
        h5("Uploaded layers"),
        if (is.null(uploaded) || nrow(uploaded) == 0) {
          p("No uploaded/imported polygon layers yet.", style = "color: grey; font-style: italic;")
        } else {
          uniq <- dplyr::distinct(uploaded, layer_id, .keep_all = TRUE)
          tags$table(class = "table table-condensed",
                     tags$thead(tags$tr(tags$th("Source"), tags$th("Features"), tags$th("Remove"))),
                     tags$tbody(
                       lapply(seq_len(nrow(uniq)), function(i) {
                         row <- uniq[i, ]
                         tags$tr(
                           tags$td(row$source_name),
                           tags$td("1"),
                           tags$td(actionButton(
                             paste0("remove_uploaded_", row$layer_id),
                             "Remove",
                             onclick = sprintf("Shiny.setInputValue('remove_uploaded', %d, {priority: 'event'})", row$layer_id),
                             class = "btn-delete"
                           ))
                         )
                       })
                     ))
        }
      )
    )
  })

  observe({
    pending <- pending_gpkg()
    if (length(pending) == 0) {
      created_observers(character(0))
      return(NULL)
    }

    existing_created <- created_observers()
    pids <- names(pending)
    for (pid in pids) {
      if (pid %in% existing_created) next

      local({
        my_pid <- pid
        import_btn_id <- paste0("import_gpkg_", my_pid)
        select_id <- paste0("select_gpkg_", my_pid)

        observeEvent(input[[import_btn_id]], {
          cur_pending_all <- pending_gpkg()
          p <- cur_pending_all[[my_pid]]
          if (is.null(p)) {
            showNotification("Pending file not found (it may have been imported already).", type = "error")
            return(NULL)
          }

          chosen_layer <- input[[select_id]]
          if (is.null(chosen_layer) || chosen_layer == "") {
            showNotification("Please select a layer before importing.", type = "error")
            return(NULL)
          }

          sf_obj <- tryCatch(sf::st_read(p$datapath, layer = chosen_layer, quiet = TRUE), error = function(e) NULL)
          if (is.null(sf_obj)) {
            showNotification(paste0("Failed to read layer ", chosen_layer, " from ", p$name), type = "error")
            return(NULL)
          }

          poly_only <- sf_obj[sf::st_is(sf_obj, c("POLYGON", "MULTIPOLYGON")), , drop = FALSE]
          if (nrow(poly_only) == 0) {
            showNotification(paste0("Layer ", chosen_layer, " does not contain polygon features."), type = "error")
            return(NULL)
          }

          poly_only <- sf::st_transform(poly_only, 4326)
          poly_only$source <- "uploaded"
          poly_only$pending_layer <- chosen_layer
          base_name <- paste0(p$name, " :: ", chosen_layer)
          existing_max <- if (!is.null(uploaded_polys())) max(as.integer(uploaded_polys()$layer_id), na.rm = TRUE) else 0
          new_layer_id <- as.integer(existing_max + 1)
          poly_only$layer_id <- new_layer_id
          poly_only$wkt <- sf::st_as_text(sf::st_geometry(poly_only))
          poly_only$pending_pid <- my_pid
          poly_only <- assign_sequential_source_names(poly_only, base_name = base_name, overview(), fixed_polys(), uploaded_polys(), drawn_features())

          tmp_up <- if (is.null(uploaded_polys())) poly_only else dplyr::bind_rows(uploaded_polys(), poly_only)
          tmp_up <- assign_uploaded_colors_server(tmp_up)
          uploaded_polys(tmp_up)

          prev_imported <- cur_pending_all[[my_pid]]$imported
          prev_imported <- if (is.null(prev_imported)) character(0) else prev_imported
          new_imported <- unique(c(prev_imported, chosen_layer))
          cur_pending_all[[my_pid]]$imported <- new_imported
          all_layers_list <- cur_pending_all[[my_pid]]$all_layers
          remaining_layers <- setdiff(all_layers_list, new_imported)
          cur_pending_all[[my_pid]]$layers <- remaining_layers
          pending_gpkg(cur_pending_all)

          leaflet::leafletProxy("map") %>%
            leaflet::clearGroup("uploaded") %>%
            {
              if (!is.null(uploaded_polys())) leaflet::addPolygons(., data = uploaded_polys(), group = "uploaded", color = ~color, fillColor = ~color, fillOpacity = 0.25, weight = 2, layerId = ~layer_id, label = ~source_name, labelOptions = leaflet::labelOptions(direction = "auto")) else .
            }

          zoom_to_sf(poly_only)
          showNotification(paste0("Imported layer '", chosen_layer, "' from ", p$name), type = "message", duration = 5)
        }, ignoreNULL = TRUE)
      })

      created_observers(unique(c(created_observers(), pid)))
    }
  })

  observeEvent(input$remove_uploaded, {
    lid <- as.integer(input$remove_uploaded)
    if (is.na(lid)) return(NULL)
    cur <- uploaded_polys()
    if (is.null(cur) || nrow(cur) == 0) return(NULL)
    lip <- which(as.integer(cur$layer_id) == lid)
    if (length(lip) > 0) {
      removed_source_name <- cur$source_name[lip[1]]
      removed_pid <- if ("pending_pid" %in% names(cur)) cur$pending_pid[lip[1]] else NA_character_
      removed_pending_layer <- if ("pending_layer" %in% names(cur)) cur$pending_layer[lip[1]] else NA_character_

      new <- cur[-lip, , drop = FALSE]
      if (nrow(new) == 0) new <- NULL
      uploaded_polys(new)
      leaflet::leafletProxy("map") %>% leaflet::clearGroup("uploaded")
      if (!is.null(new)) leaflet::leafletProxy("map") %>% leaflet::addPolygons(data = new, group = "uploaded", color = ~color, fillColor = ~color, fillOpacity = 0.25, weight = 2, layerId = ~layer_id, label = ~source_name, labelOptions = leaflet::labelOptions(direction = "auto"))

      sel <- selected_polygons()
      if (!is.null(sel) && nrow(sel) > 0 && "source" %in% names(sel)) {
        keep_idx <- !(sel$source == "uploaded" & as.integer(sel$layer_id) == lid)
        new_sel <- sel[keep_idx, , drop = FALSE]
        if (nrow(new_sel) == 0) new_sel <- NULL
        selected_polygons(new_sel)
        update_selected_highlights()
      }

      if (!is.na(removed_pid) && nzchar(removed_pid)) {
        cur_pending <- pending_gpkg()
        if (!is.null(cur_pending) && removed_pid %in% names(cur_pending)) {
          chosen_layer <- removed_pending_layer
          if (is.na(chosen_layer) || !nzchar(chosen_layer)) chosen_layer <- sub("^.*::\\s*", "", removed_source_name)
          cur_imported <- cur_pending[[removed_pid]]$imported
          if (is.null(cur_imported)) cur_imported <- character(0)
          cur_pending[[removed_pid]]$imported <- setdiff(cur_imported, chosen_layer)
          all_layers_list <- cur_pending[[removed_pid]]$all_layers
          if (is.null(all_layers_list)) all_layers_list <- cur_pending[[removed_pid]]$layers
          cur_pending[[removed_pid]]$layers <- setdiff(all_layers_list, cur_pending[[removed_pid]]$imported)
          pending_gpkg(cur_pending)
        }
      }

      showNotification("Uploaded layer removed.", type = "message", duration = 4)
    }
  }, ignoreInit = TRUE)
}
