# Selecting polygons on the map: the map itself, clicks, "Deselect all" and drawing.
server_map <- function(input, output, session, state, helpers) {
  drawn_features <- state$drawn_features
  selected_polygons <- state$selected_polygons
  fixed_polys <- state$fixed_polys
  uploaded_polys <- state$uploaded_polys
  overview <- state$overview
  update_selected_highlights <- helpers$update_selected_highlights
  clear_map_polygons <- helpers$clear_map_polygons
  clear_project_selection_state <- helpers$clear_project_selection_state

  output$map <- renderLeaflet({
    leaflet() %>% addTiles() %>%
      setView(lng = 5.3, lat = 52.1, zoom = 7) %>%
      addDrawToolbar(
        targetGroup = "drawn",
        polylineOptions = FALSE,
        circleOptions = FALSE,
        rectangleOptions = FALSE,
        markerOptions = FALSE,
        circleMarkerOptions = FALSE,
        # Drawing your own polygon on the map is temporarily disabled.
        # To re-enable, uncomment the line below and remove/comment out polygonOptions = FALSE.
        # polygonOptions = drawPolygonOptions(showArea = TRUE, repeatMode = FALSE),
        polygonOptions = FALSE,
        # Remove/trash-bin button is temporarily disabled.
        # To re-enable, uncomment the line below and remove/comment out the line after it.
        # editOptions = editToolbarOptions(edit = FALSE, remove = TRUE)
        editOptions = editToolbarOptions(edit = FALSE, remove = FALSE)
      )
  })

  # Clear the user's selected polygons when the map's "Deselect all" button is
  # clicked. The loaded project layer (the "fixed" group) is intentionally left
  # in place; only the selection + its highlight are removed.
  observeEvent(input$ndc_deselect_all, {
    selected_polygons(NULL)
    update_selected_highlights()
  })

  # Show the "Deselect all" map control only while at least one polygon is
  # selected; remove it otherwise. Driven reactively by the selection state.
  observe({
    sel <- selected_polygons()
    has_selection <- !is.null(sel) && nrow(sel) > 0
    proxy <- leaflet::leafletProxy("map")
    leaflet::removeControl(proxy, layerId = "ndc_deselect_ctrl")
    if (has_selection) {
      leaflet::addControl(
        proxy,
        position = "topright",
        layerId = "ndc_deselect_ctrl",
        html = HTML(paste0(
          "<button id='ndc_deselect_btn' type='button' title='Deselect all selected polygons' ",
          "style='background:#fff; border:2px solid rgba(0,0,0,0.2); border-radius:4px; ",
          "padding:5px 10px; font-size:13px; font-weight:600; color:#1f5a8a; cursor:pointer; ",
          "box-shadow:0 1px 4px rgba(0,0,0,0.15);' ",
          "onclick=\"Shiny.setInputValue('ndc_deselect_all', Math.random(), {priority:'event'});\">",
          "Deselect all</button>"
        ))
      )
    }
  })

  observeEvent(input$map_shape_click, {
    click <- input$map_shape_click
    req(!is.null(click$lng), !is.null(click$lat))

    found_poly <- NULL
    found_source <- NULL

    if (!is.null(click$id) && click$id != "") {
      fp <- fixed_polys()
      if (!is.null(fp) && "layer_id" %in% names(fp)) {
        match_idx <- which(as.integer(fp$layer_id) == as.integer(click$id))
        if (length(match_idx) > 0) { found_poly <- fp[match_idx, , drop = FALSE]; found_source <- "fixed" }
      }
      if (is.null(found_poly)) {
        up <- uploaded_polys()
        if (!is.null(up) && "layer_id" %in% names(up)) {
          match_idx <- which(as.integer(up$layer_id) == as.integer(click$id))
          if (length(match_idx) > 0) { found_poly <- up[match_idx, , drop = FALSE]; found_source <- "uploaded" }
        }
      }
    }

    if (is.null(found_poly)) {
      pt_sf <- sf::st_as_sf(data.frame(id = 1, x = click$lng, y = click$lat), coords = c("x", "y"), crs = 4326)

      fp <- fixed_polys()
      if (!is.null(fp) && nrow(fp) > 0) {
        ints <- sf::st_intersects(fp, pt_sf, sparse = FALSE)
        if (any(ints)) { idx <- which(ints)[1]; found_poly <- fp[idx, , drop = FALSE]; found_source <- "fixed" }
      }

      if (is.null(found_poly)) {
        up <- uploaded_polys()
        if (!is.null(up) && nrow(up) > 0) {
          ints <- sf::st_intersects(up, pt_sf, sparse = FALSE)
          if (any(ints)) { idx <- which(ints)[1]; found_poly <- up[idx, , drop = FALSE]; found_source <- "uploaded" }
        }
      }

      if (is.null(found_poly)) {
        df <- drawn_features()
        if (!is.null(df) && nrow(df) > 0) {
          ints <- sf::st_intersects(df, pt_sf, sparse = FALSE)
          if (any(ints)) { idx <- which(ints)[1]; found_poly <- df[idx, , drop = FALSE]; found_source <- "drawn" }
        }
      }
    }

    if (is.null(found_poly) || nrow(found_poly) == 0) return(NULL)

    cur_sel <- selected_polygons()
    if (!is.null(cur_sel) && nrow(cur_sel) > 0) {
      eq_matrix <- sf::st_equals(cur_sel, found_poly, sparse = FALSE)
      if (is.matrix(eq_matrix) && any(as.logical(eq_matrix))) {
        rem_idx <- which(as.logical(eq_matrix), arr.ind = FALSE)
        if (length(rem_idx) > 0) {
          new_sel <- cur_sel[-rem_idx, , drop = FALSE]
          if (nrow(new_sel) == 0) new_sel <- NULL
          selected_polygons(new_sel)
          update_selected_highlights()
          return(NULL)
        }
      }
    }

    found_poly$source <- found_source
    if (is.null(cur_sel) || nrow(cur_sel) == 0) {
      selected_polygons(found_poly)
    } else {
      if (!("source" %in% names(cur_sel))) cur_sel$source <- "drawn"
      selected_polygons(dplyr::bind_rows(cur_sel, found_poly))
    }

    update_selected_highlights()
  })

  observeEvent(input$map_draw_new_feature, {
    feat <- input$map_draw_new_feature
    start_id <- next_layer_id(fixed_polys(), uploaded_polys(), drawn_features())

    clear_project_selection_state()
    clear_map_polygons(except = c("drawn"))

    poly_sf <- convert_drawn_to_sf(feat, start_layer_id = start_id)
    req(poly_sf)

    poly_sf <- assign_sequential_source_names(poly_sf, base_name = "Own polygon", overview(), fixed_polys(), uploaded_polys(), drawn_features())

    if (is.null(drawn_features())) drawn_features(poly_sf) else drawn_features(dplyr::bind_rows(drawn_features(), poly_sf))

    leaflet::leafletProxy("map") %>% leaflet::clearGroup("drawn")
    leaflet::leafletProxy("map") %>%
      leaflet::addPolygons(
        data = drawn_features(),
        group = "drawn",
        layerId = ~layer_id,
        color = "#444444",
        weight = 2,
        fillOpacity = 0.3,
        label = ~source_name,
        labelOptions = leaflet::labelOptions(direction = "auto")
      )

    poly_sf$source <- "drawn"
    cur_sel <- selected_polygons()
    if (is.null(cur_sel) || nrow(cur_sel) == 0) {
      selected_polygons(poly_sf)
    } else {
      if (!("source" %in% names(cur_sel))) cur_sel$source <- "drawn"
      selected_polygons(dplyr::bind_rows(cur_sel, poly_sf))
    }

    update_selected_highlights()
  })

  observeEvent(input$map_draw_deleted_features, {
    deleted <- input$map_draw_deleted_features
    df <- drawn_features()
    if (is.null(df) || nrow(df) == 0) {
      selected_polygons(NULL)
      leaflet::leafletProxy("map") %>% leaflet::clearGroup("highlight_drawn")
      return(NULL)
    }

    deleted_feats <- list()
    if (!is.null(deleted$features) && length(deleted$features) > 0) {
      for (f in deleted$features) {
        sf_f <- convert_geojson_feature_to_sf(f)
        if (!is.null(sf_f)) deleted_feats[[length(deleted_feats) + 1]] <- sf_f
      }
    }

    if (length(deleted_feats) == 0) {
      drawn_features(NULL)
      sel <- selected_polygons()
      if (!is.null(sel) && nrow(sel) > 0 && "source" %in% names(sel)) sel <- sel[sel$source != "drawn", , drop = FALSE]
      if (is.null(sel) || nrow(sel) == 0) sel <- NULL
      selected_polygons(sel)
      update_selected_highlights()
      return(NULL)
    }

    remaining <- df
    for (del in deleted_feats) {
      eq_idx <- integer(0)
      if (nrow(remaining) > 0) {
        eq_matrix <- sf::st_equals(remaining, del, sparse = FALSE)
        if (is.matrix(eq_matrix)) eq_idx <- which(as.logical(eq_matrix))
        if (length(eq_idx) == 0) {
          inters <- sf::st_intersects(remaining, del, sparse = FALSE)
          if (is.matrix(inters)) eq_idx <- which(as.logical(inters))
        }
      }
      if (length(eq_idx) > 0) remaining <- remaining[-eq_idx, , drop = FALSE]
    }

    if (nrow(remaining) == 0) drawn_features(NULL) else drawn_features(remaining)

    sel <- selected_polygons()
    if (!is.null(sel) && nrow(sel) > 0 && "source" %in% names(sel)) {
      keep_idx <- rep(TRUE, nrow(sel))
      for (i in seq_len(nrow(sel))) {
        if (sel$source[i] == "drawn") {
          present <- FALSE
          if (!is.null(remaining) && nrow(remaining) > 0) {
            eq_mat <- sf::st_equals(sel[i, , drop = FALSE], remaining, sparse = FALSE)
            if (is.matrix(eq_mat) && any(as.logical(eq_mat))) present <- TRUE
          }
          if (!present) keep_idx[i] <- FALSE
        }
      }
      new_sel <- sel[keep_idx, , drop = FALSE]
      if (nrow(new_sel) == 0) new_sel <- NULL
      selected_polygons(new_sel)
    }

    update_selected_highlights()
  })
}
