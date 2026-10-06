# The quick guide.
server_help <- function(input, output, session) {
  observeEvent(input$open_guide, {
    read_readme <- function() {
      p <- system.file("app", "NatureDataCube_README.txt", package = "rNDC.Shiny")
      if (nzchar(p) && file.exists(p)) {
        txt <- tryCatch(readLines(p, warn = FALSE), error = function(e) NULL)
        if (!is.null(txt)) return(paste(txt, collapse = "<br>"))
      }
      return("Help file not found. Try re-installing the package.")
    }
    content <- read_readme()
    showModal(modalDialog(
      title = "How to use this app \u2014 Quick guide",
      HTML(content),
      easyClose = TRUE,
      footer = modalButton("Close"),
      size = "l"
    ))
  }, ignoreInit = TRUE)
}
