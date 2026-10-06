# Project layers (LTER classes and SNL parcels) on the map.
server_projects <- function(input, output, session, state, helpers) {
  drawn_features <- state$drawn_features
  selected_polygons <- state$selected_polygons
  fixed_polys <- state$fixed_polys
  uploaded_polys <- state$uploaded_polys
  active_project <- state$active_project
  snl_last_bbox <- state$snl_last_bbox
  snl_status_msg <- state$snl_status_msg
  overview <- state$overview
  clear_map_polygons <- helpers$clear_map_polygons

  observeEvent(active_project(), {
    val <- active_project()
    session$sendCustomMessage("ndc_select_fixed", if (is.null(val)) NULL else val)
  }, ignoreNULL = FALSE)

  # ---- Helper: render a set of "fixed" project polygons on the map ----
  render_fixed_polys <- function(poly, fit = FALSE, show_hint = TRUE) {
    fixed_polys(poly)
    proxy <- leaflet::leafletProxy("map") %>%
      leaflet::clearGroup("fixed") %>%
      leaflet::clearGroup("highlight_fixed") %>%
      leaflet::clearPopups() %>%
      leaflet::addPolygons(
        data = poly,
        group = "fixed",
        color = "black",
        fillOpacity = 0.3,
        weight = 2,
        layerId = ~layer_id,
        label = ~source_name,
        labelOptions = leaflet::labelOptions(direction = "auto")
      )

    if (fit) {
      bb <- sf::st_bbox(poly)
      proxy <- proxy %>% leaflet::fitBounds(
        as.numeric(bb["xmin"]), as.numeric(bb["ymin"]),
        as.numeric(bb["xmax"]), as.numeric(bb["ymax"])
      )
    }

    if (show_hint && (is.null(selected_polygons()) || nrow(selected_polygons()) == 0)) {
      centroid <- sf::st_centroid(sf::st_geometry(poly[nrow(poly), ]))
      coords <- sf::st_coordinates(centroid)
      proxy %>% leaflet::addPopups(
        lng = coords[1], lat = coords[2],
        popup = "\u26A0 You have selected a project but still need to select a project area.",
        options = leaflet::popupOptions(closeButton = TRUE)
      )
    }
    invisible(NULL)
  }

  # ---- Main project selection handler (LTER classes + SNL) ----
  observeEvent(input$ndc_project, {
    key <- input$ndc_project
    active_project(if (is.null(key) || identical(key, "")) NULL else key)
    snl_last_bbox(NULL)   # reset SNL viewport cache on any project change
    snl_status_msg(NULL)  # clear any stale SNL status line

    # Deselected: clear everything
    if (is.null(key) || key == "") {
      clear_map_polygons(except = character(0))
      leaflet::leafletProxy("map") %>%
        leaflet::clearGroup("fixed") %>%
        leaflet::clearGroup("highlight_fixed") %>%
        leaflet::clearPopups()
      session$sendCustomMessage("ndc_select_fixed", NULL)
      return(NULL)
    }

    # Any project switch starts from a fully clean map: clear previous project
    # polygons (e.g. LTER sites when switching to SNL, or vice versa), their
    # highlights, selection and popups.
    clear_map_polygons(except = character(0))
    fixed_polys(NULL)
    leaflet::leafletProxy("map") %>%
      leaflet::clearGroup("fixed") %>%
      leaflet::clearGroup("highlight_fixed") %>%
      leaflet::clearPopups()

    # ---- SNL: don't load now; parcels stream in by viewport (see observer below) ----
    if (identical(key, "snl")) {
      # Trigger an immediate viewport fetch attempt (it will no-op if zoomed out)
      snl_update_viewport(force = TRUE)
      return(NULL)
    }

    # ---- LTER: pull the chosen class from the cached, classified collection ----
    if (startsWith(key, "lter:")) {
      this_class <- sub("^lter:", "", key)
      lter_res <- get_lter_data()
      lter <- lter_res$data
      if (is.null(lter)) {
        showNotification(
          paste0("Could not load LTER data from the STAC endpoint",
                 if (!is.null(lter_res$error)) paste0(": ", lter_res$error) else " (empty collection)", "."),
          type = "error", duration = 10
        )
        return(NULL)
      }
      poly <- lter[!is.na(lter$project_class) & lter$project_class == this_class, , drop = FALSE]
      if (nrow(poly) == 0) {
        showNotification(paste0("No LTER features found for '", this_class, "'."), type = "warning")
        return(NULL)
      }
      poly <- dplyr::mutate(poly, layer_id = as.integer(dplyr::row_number()))
      # Unique sequential names per class, e.g. "Light on Nature_1", "Light on Nature_2", ...
      poly <- assign_sequential_source_names(poly, base_name = this_class,
                                             overview(), fixed_polys(), uploaded_polys(), drawn_features())
      # Add the WKT geometry text column used by downstream retrieval (Weather,
      # Soil map, AHN, Agricultural fields all read the polygon as WKT).
      poly <- add_wkt_column(poly)
      render_fixed_polys(poly, fit = TRUE, show_hint = TRUE)
    }
  }, ignoreNULL = FALSE)

  # ---- SNL viewport streaming ----
  # Fetch only the parcels intersecting the current map view, and only once the
  # user has zoomed in past snl_min_zoom. Debounced + bbox-deduped to keep the
  # API load (and render count) low.
  snl_update_viewport <- function(force = FALSE) {
    if (!identical(active_project(), "snl")) return(invisible(NULL))

    zoom <- input$map_zoom
    bounds <- input$map_bounds
    if (is.null(zoom) || is.null(bounds)) return(invisible(NULL))

    # Too far out: clear parcels and show a hint instead of fetching thousands.
    if (zoom < snl_min_zoom) {
      fixed_polys(NULL)
      snl_last_bbox(NULL)
      snl_status_msg(list(type = "hint", text = "Zoom in to load SNL parcels."))
      # No on-map popup here: re-anchoring a popup to the viewport centre on
      # every pan/zoom made it "travel" and flicker. The status line under the
      # map conveys the same thing and stays put.
      leaflet::leafletProxy("map") %>%
        leaflet::clearGroup("fixed") %>%
        leaflet::removePopup("snl_zoom_hint")
      return(invisible(NULL))
    }

    bbox <- c(bounds$west, bounds$south, bounds$east, bounds$north)

    # Skip if this viewport is essentially the same as the last fetched one.
    if (!force && !is.null(snl_last_bbox())) {
      prev <- snl_last_bbox()
      if (max(abs(bbox - prev)) < 1e-6) return(invisible(NULL))
    }
    snl_last_bbox(bbox)

    leaflet::leafletProxy("map") %>% leaflet::removePopup("snl_zoom_hint")

    # The fetch is a blocking network call. withProgress() gives the user a
    # real progress indicator while it runs (a plain reactiveVal "loading"
    # message would not render until the observer returns, i.e. too late).
    res <- withProgress(
      expr = fetch_snl_bbox(bbox),
      message = "Loading SNL parcels for this view\u2026",
      value = 0.5
    )

    if (identical(res$status, "error")) {
      fixed_polys(NULL)
      leaflet::leafletProxy("map") %>% leaflet::clearGroup("fixed")
      snl_status_msg(list(type = "error",
                          text = paste0("Could not load SNL parcels: ",
                                        if (is.null(res$error)) "request failed." else res$error)))
      return(invisible(NULL))
    }

    if (identical(res$status, "empty")) {
      fixed_polys(NULL)
      leaflet::leafletProxy("map") %>% leaflet::clearGroup("fixed")
      snl_status_msg(list(type = "empty", text = "No SNL parcels in this area. Try panning or zooming."))
      return(invisible(NULL))
    }

    parcels <- res$data
    n <- nrow(parcels)
    parcels <- dplyr::mutate(parcels, layer_id = as.integer(dplyr::row_number()))
    parcels <- assign_sequential_source_names(parcels, base_name = "SNL parcel",
                                              overview(), fixed_polys(), uploaded_polys(), drawn_features())
    # Add the WKT geometry text column used by downstream retrieval.
    parcels <- add_wkt_column(parcels)
    fixed_polys(parcels)

    leaflet::leafletProxy("map") %>%
      leaflet::clearGroup("fixed") %>%
      leaflet::addPolygons(
        data = parcels,
        group = "fixed",
        color = "black",
        fillOpacity = 0.2,
        weight = 1,
        layerId = ~layer_id,
        label = ~source_name,
        labelOptions = leaflet::labelOptions(direction = "auto")
      )

    # If we hit the cap, the view is almost certainly showing only a subset.
    capped <- n >= snl_fetch_limit
    snl_status_msg(list(
      type = if (capped) "capped" else "ok",
      text = if (capped)
        paste0("Showing ", n, " SNL parcels (zoom in further to see all parcels in this area).")
      else
        paste0("Showing ", n, " SNL parcel", if (n == 1) "" else "s", " in view. Click on a parcel to select it.")
    ))
    invisible(NULL)
  }

  # React to pan/zoom while SNL is active. Debounced so dragging doesn't spam
  # the API; only the settled viewport triggers a fetch.
  snl_viewport_trigger <- reactive({
    list(zoom = input$map_zoom, bounds = input$map_bounds)
  }) %>% debounce(500)

  observeEvent(snl_viewport_trigger(), {
    if (identical(active_project(), "snl")) snl_update_viewport(force = FALSE)
  }, ignoreInit = TRUE)

  # Status line under the map for the SNL parcel layer. Only shown while SNL
  # is the active project; cleared otherwise.
  output$snl_status <- renderUI({
    if (!identical(active_project(), "snl")) return(NULL)
    msg <- snl_status_msg()
    if (is.null(msg)) return(NULL)
    colour <- switch(msg$type,
                     loading = "#1f5a8a",
                     ok      = "#2e7d32",
                     capped  = "#b26a00",
                     empty   = "#b26a00",
                     error   = "#c62828",
                     hint    = "#555555",
                     "#555555")
    tags$div(
      style = paste0("margin-top:6px; padding:6px 10px; border-radius:5px; font-size:13px; ",
                     "background:#f5f7fb; border-left:4px solid ", colour, "; color:", colour, ";"),
      if (identical(msg$type, "loading"))
        tags$span(tags$span(class = "snl-spinner"), msg$text)
      else
        msg$text
    )
  })
}
