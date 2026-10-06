# Functions shared by the server components that work on the map and the selection. They are created for a
# session, from the shared `state`, and returned as a list.
create_map_helpers <- function(session, state) {
  drawn_features <- state$drawn_features
  selected_polygons <- state$selected_polygons
  fixed_polys <- state$fixed_polys
  uploaded_polys <- state$uploaded_polys
  pending_gpkg <- state$pending_gpkg
  active_project <- state$active_project
  snl_last_bbox <- state$snl_last_bbox

  restore_pending_from_uploaded <- function(up_sf) {
    if (is.null(up_sf) || nrow(up_sf) == 0) return(invisible(NULL))
    if (!all(c("pending_pid", "pending_layer") %in% names(up_sf))) return(invisible(NULL))

    cur_pending <- pending_gpkg()
    if (is.null(cur_pending)) cur_pending <- list()

    restore_df <- up_sf[
      !is.na(up_sf$pending_pid) & nzchar(as.character(up_sf$pending_pid)) &
        !is.na(up_sf$pending_layer) & nzchar(as.character(up_sf$pending_layer)),
      , drop = FALSE
    ]
    if (nrow(restore_df) == 0) return(invisible(NULL))

    restore_key <- paste(as.character(restore_df$pending_pid), as.character(restore_df$pending_layer), sep = "::")
    restore_df <- restore_df[!duplicated(restore_key), , drop = FALSE]

    for (i in seq_len(nrow(restore_df))) {
      pid <- as.character(restore_df$pending_pid[i])
      layer_nm <- as.character(restore_df$pending_layer[i])
      if (!nzchar(pid) || !nzchar(layer_nm)) next
      if (is.null(cur_pending[[pid]])) next

      prev_imported <- cur_pending[[pid]]$imported
      if (is.null(prev_imported)) prev_imported <- character(0)
      cur_pending[[pid]]$imported <- setdiff(unique(prev_imported), layer_nm)

      all_layers_list <- cur_pending[[pid]]$all_layers
      if (is.null(all_layers_list)) all_layers_list <- cur_pending[[pid]]$layers
      cur_pending[[pid]]$layers <- setdiff(all_layers_list, cur_pending[[pid]]$imported)
    }

    pending_gpkg(cur_pending)
    invisible(NULL)
  }

  clear_map_polygons <- function(except = character(0)) {
    groups_all <- c("fixed", "uploaded", "drawn", "highlight_fixed", "highlight_uploaded", "highlight_drawn")
    to_clear <- setdiff(groups_all, except)
    proxy <- leaflet::leafletProxy("map")
    for (g in to_clear) proxy <- suppressWarnings(leaflet::clearGroup(proxy, g))
    proxy <- suppressWarnings(leaflet::clearPopups(proxy))

    if (!("uploaded" %in% except)) restore_pending_from_uploaded(uploaded_polys())
    if (!("fixed" %in% except)) fixed_polys(NULL)
    if (!("uploaded" %in% except)) uploaded_polys(NULL)
    if (!("drawn" %in% except)) drawn_features(NULL)
    selected_polygons(NULL)
    invisible(NULL)
  }

  clear_project_selection_state <- function() {
    session$sendCustomMessage("ndc_select_fixed", NULL)
    session$sendCustomMessage("ndc_force_clear_fixed_sidebar", NULL)
    active_project(NULL)
    snl_last_bbox(NULL)
    sel <- selected_polygons()
    if (is.null(sel) || nrow(sel) == 0) {
      selected_polygons(NULL)
    } else if ("source" %in% names(sel)) {
      sel <- sel[sel$source != "fixed", , drop = FALSE]
      if (nrow(sel) == 0) sel <- NULL
      selected_polygons(sel)
    } else {
      selected_polygons(NULL)
    }
    update_selected_highlights() # TODO: check this function
    invisible(NULL)
  }

  update_selected_highlights <- function() {
    sel <- selected_polygons()
    leaflet::leafletProxy("map") %>%
      leaflet::clearGroup("highlight_fixed") %>%
      leaflet::clearGroup("highlight_uploaded") %>%
      leaflet::clearGroup("highlight_drawn")

    if (is.null(sel) || nrow(sel) == 0) return(invisible(NULL))
    if (!("source" %in% names(sel))) sel$source <- "drawn"

    if (any(sel$source == "fixed")) {
      toadd <- sel[sel$source == "fixed", , drop = FALSE]
      leaflet::leafletProxy("map") %>% leaflet::addPolygons(
        data = toadd, group = "highlight_fixed", color = "#007bff", weight = 4, fillOpacity = 0.5,
        layerId = ~layer_id, label = ~source_name, labelOptions = leaflet::labelOptions(direction = "auto")
      )
    }
    if (any(sel$source == "uploaded")) {
      toadd <- sel[sel$source == "uploaded", , drop = FALSE]
      if ("color" %in% names(toadd)) {
        leaflet::leafletProxy("map") %>% leaflet::addPolygons(
          data = toadd, group = "highlight_uploaded", color = ~color, fillColor = ~color, weight = 4, fillOpacity = 0.5,
          layerId = ~layer_id, label = ~source_name, labelOptions = leaflet::labelOptions(direction = "auto")
        )
      } else {
        leaflet::leafletProxy("map") %>% leaflet::addPolygons(
          data = toadd, group = "highlight_uploaded", color = "#007bff", fillColor = "#007bff", weight = 4, fillOpacity = 0.5,
          layerId = ~layer_id, label = ~source_name, labelOptions = leaflet::labelOptions(direction = "auto")
        )
      }
    }
    if (any(sel$source == "drawn")) {
      toadd <- sel[sel$source == "drawn", , drop = FALSE]
      leaflet::leafletProxy("map") %>% leaflet::addPolygons(
        data = toadd, group = "highlight_drawn", color = "#007bff", weight = 4, fillOpacity = 0.5,
        layerId = ~layer_id, label = ~source_name, labelOptions = leaflet::labelOptions(direction = "auto")
      )
    }
    invisible(NULL)
  }

  list(
    update_selected_highlights = update_selected_highlights,
    clear_map_polygons = clear_map_polygons,
    clear_project_selection_state = clear_project_selection_state,
    restore_pending_from_uploaded = restore_pending_from_uploaded
  )
}
