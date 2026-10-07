# The overview ("shopping cart") table.
server_overview <- function(input, output, session, state, helpers) {
  selected_polygons <- state$selected_polygons
  overview <- state$overview
  update_selected_highlights <- helpers$update_selected_highlights

  output$overview_table <- renderUI({
    ov <- overview()
    if (is.null(ov) || nrow(ov) == 0) {
      return(p("No datasets added yet.", style = "color: grey; font-style: italic;"))
    }

    display_dates <- vapply(seq_len(nrow(ov)), function(i) {
      format_date_label(ov$year[i], ov$date_from[i], ov$date_to[i])
    }, FUN.VALUE = character(1), USE.NAMES = FALSE)

    tags$table(
      class = "table table-striped table-bordered",
      tags$thead(tags$tr(
        tags$th("Dataset"),
        tags$th("Type"),
        tags$th("Date"),
        tags$th("Polygon"),
        tags$th("Delete")
      )),
      tags$tbody(
        lapply(seq_len(nrow(ov)), function(i) {
          row <- ov[i, ]
          tags$tr(
            tags$td(row$dataset),
            tags$td(row$view),
            tags$td(display_dates[i]),
            tags$td(row$polygon),
            tags$td(actionButton(
              paste0("delete_row_", i),
              "x",
              onclick = sprintf("Shiny.setInputValue('delete_row', %d, {priority: 'event'})", i),
              class = "btn-delete"
            ))
          )
        })
      )
    )
  })

  observeEvent(input$delete_row, {
    idx <- as.integer(input$delete_row)
    if (is.na(idx)) return(NULL)

    ov <- overview()
    if (is.null(ov) || nrow(ov) == 0) {
      showNotification("Nothing to delete.", type = "warning")
      return(NULL)
    }
    if (idx < 1 || idx > nrow(ov)) {
      showNotification("That row no longer exists (index out of range).", type = "error")
      return(NULL)
    }

    new_ov <- ov[-idx, , drop = FALSE]
    if (nrow(new_ov) == 0) overview(empty_overview()) else overview(new_ov)
    update_selected_highlights()
    showNotification("Row removed from overview.", type = "message")
  }, ignoreInit = TRUE)

  observeEvent(input$clear_overview, {
    cur_sel <- selected_polygons()
    overview(empty_overview())
    if (!is.null(cur_sel) && nrow(cur_sel) > 0) selected_polygons(cur_sel)
    update_selected_highlights()
    showNotification("Overview cleared", type = "message")
  }, ignoreInit = TRUE)
}
