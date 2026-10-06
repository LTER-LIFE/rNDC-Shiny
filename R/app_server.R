# The server logic of the app: the shared state, and the parts that work on it (see R/server_*.R).

app_server <- function(input, output, session) {
  # Tokens are read when a session starts (ndc_setup() has already checked them).
  state <- create_state(Sys.getenv("NDC_TOKEN"), Sys.getenv("ADC_TOKEN"))
  # The state is also available by name (e.g. `overview()`): the tests reach it that way.
  list2env(state, environment())
  helpers <- create_map_helpers(session, state)

  dataset <- server_dataset(input, output, session, state, helpers)
  server_projects(input, output, session, state, helpers)
  server_map(input, output, session, state, helpers)
  server_upload(input, output, session, state, helpers)
  server_overview(input, output, session, state, helpers)
  download <- server_download(input, output, session, state, helpers)
  server_help(input, output, session)

  # Kept by name, like the state, for the tests
  build_controls_for <- dataset$build_controls_for
  retrieve_and_save <- download$retrieve_and_save

  invisible(NULL)
}
