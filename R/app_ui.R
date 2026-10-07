# The user interface of the app.

app_ui <- function() {
  fluidPage(
    useShinyjs(),
    tags$head(
      tags$style(HTML("
        body {
          background-color: #f5f7fb;
          font-family: 'Segoe UI', 'Helvetica Neue', Arial, sans-serif;
        }
        .app-header {
          background: linear-gradient(135deg, #1f5a8a, #2d7da6);
          color: white;
          padding: 20px 30px;
          margin-bottom: 20px;
          display: flex;
          align-items: center;
          gap: 25px;
        }
        .app-header img {
          height: 60px;
        }
        .app-title {
          font-size: 30px;
          font-weight: 600;
        }
        .header-link {
          margin-left: auto;
          display: inline-flex;
          align-items: center;
          gap: 8px;
          background: rgba(255,255,255,0.15);
          color: #ffffff !important;
          text-decoration: none !important;
          padding: 8px 16px;
          border-radius: 20px;
          font-size: 14px;
          font-weight: 500;
          border: 1px solid rgba(255,255,255,0.4);
          transition: background 0.2s ease;
          white-space: nowrap;
        }
        .header-link:hover {
          background: rgba(255,255,255,0.3);
        }
        .header-link svg {
          height: 14px;
          width: 14px;
          flex-shrink: 0;
        }
        .well {
          background-color: white;
          border-radius: 6px;
          border: none;
          box-shadow: 0 2px 6px rgba(0,0,0,0.08);
        }
        .btn-custom {
          background-color: #1f5a8a !important;
          color: white !important;
          border: none !important;
          border-radius: 5px;
          padding: 8px 14px;
          font-weight: 500;
          transition: all 0.2s ease;
        }
        .btn-custom:hover {
          background-color: #17476c !important;
          transform: translateY(-1px);
        }
        .btn-download {
          background-color: #1f5a8a !important;
          color: white !important;
          border: none !important;
        }
        .small-btn {
          padding: 6px 10px;
          font-size: 13px;
        }
        .radio label {
          display: block;
          background: white;
          border: 1px solid #d8e1ec;
          padding: 8px 12px;
          border-radius: 5px;
          margin-bottom: 6px;
          cursor: pointer;
          transition: all 0.2s ease;
        }
        .radio label:hover {
          background: #eef4fb;
          border-color: #1f5a8a;
        }
        .table thead {
          background-color: #1f5a8a;
          color: white;
        }
        .btn-delete {
          background-color: #c94c4c;
          color: white;
          border: none;
          border-radius: 4px;
          padding: 2px 6px;
        }
        .btn-delete:hover {
          background-color: #a83d3d;
        }
        h4 {
          color: #1f5a8a;
          font-weight: 600;
        }
        .ndc-category {
          margin-bottom: 8px;
          background: white;
          border: 1px solid #e6eef6;
          border-radius: 6px;
          padding: 6px 8px;
          box-shadow: 0 1px 3px rgba(0,0,0,0.03);
        }
        .ndc-category summary {
          font-weight: 600;
          color: #1f5a8a;
          list-style: none;
          cursor: pointer;
          outline: none;
          padding: 6px;
        }
        .ndc-ds {
          display: block;
          padding: 6px 10px;
          margin: 4px 0;
          border-radius: 5px;
          color: #233043;
          text-decoration: none;
        }
        .ndc-ds:hover {
          background: #eef4fb;
          border-color: #1f5a8a;
        }
        .ndc-ds.selected { 
          background: linear-gradient(90deg, rgba(29,92,140,0.08), rgba(29,92,140,0.04));
          border-left: 4px solid #1f5a8a;
          padding-left: 6px;
          font-weight: 600;
        }
        details[open] > summary::after {
          content: \" \\25BC\";
          float: right;
        }
        details > summary::after {
          content: \" \\25B6\";
          float: right;
        }
        .fixed-item {
          display: block;
          padding: 6px 10px;
          margin: 4px 0;
          border-radius: 5px;
          color: #233043;
          text-decoration: none;
        }
        .fixed-item:hover {
          background: #eef4fb;
          border-color: #1f5a8a;
        }
        .fixed-item.selected {
          background: linear-gradient(90deg, rgba(29,92,140,0.08), rgba(29,92,140,0.04));
          border-left: 4px solid #1f5a8a;
          padding-left: 6px;
          font-weight: 600;
        }
        .ndc-subcategory { margin: 6px 0 6px 10px; padding: 4px 8px; border-left: 2px solid #e6eef6; }
        .ndc-subcategory > summary { font-weight: 600; color: #2d7da6; list-style: none; cursor: pointer; outline: none; padding: 4px; font-size: 14px; }
        .ndc-leaf { margin: 6px 0 6px 10px; padding: 4px 8px 4px 12px; border-left: 2px solid #e6eef6; font-weight: 600; color: #2d7da6; font-size: 14px; }
        .ndc-leaf:hover { background: #eef4fb; }
        .ndc-leaf.selected { color: #1f5a8a; }
        .nav-tabs a.disabled {
          color: #999 !important;
          pointer-events: none;
          cursor: default;
          opacity: 0.6;
        }
        .btn-custom[disabled] {
          opacity: 0.55 !important;
          cursor: not-allowed !important;
          box-shadow: none !important;
        }
        .btn-info-circle {
          background-color: #FFFFFF !important;
          color: #2C7BE5 !important;
          border: 1px solid #2C7BE5;
          width: 24px;
          height: 24px;
          padding: 0 !important;
          font-size: 1.1rem;
          font-weight: bold;
          border-radius: 50% !important;
          display: inline-flex;
          align-items: center;
          justify-content: center;
          line-height: 1;
          transition: all 0.2s;
        }
        .btn-info-circle:hover, .btn-info-circle:focus {
          background-color: #2C7BE5 !important;
          color: #FFFFFF !important;
          border-color: #2C7BE5 !important;
        }
        .dataset-row {
          display:flex;
          align-items:center;
          justify-content:space-between;
          gap:8px;
        }
        .dataset-row .ndc-ds {
          flex:1;
          margin:0;
        }
        .help-button-container {
          position: fixed;
          bottom: 20px;
          left: 20px;
          z-index: 9999;
        }
        .btn-help-circle {
          background-color: #FFFFFF !important;
          color: #2C7BE5 !important;
          border: 1px solid #2C7BE5;
          width: 40px;
          height: 40px;
          font-size: 1.2rem;
          font-weight: bold;
          border-radius: 50% !important;
          display: flex;
          align-items: center;
          justify-content: center;
          padding: 0 !important;
          box-shadow: 0 2px 6px rgba(0,0,0,0.15);
          transition: all 0.2s;
        }
        .btn-help-circle:hover {
          background-color: #2C7BE5 !important;
          color: #FFFFFF !important;
        }
        .nav-tabs > li > a {
          color: #233043 !important;
          font-weight: 600;
          background: white;
          border: 1px solid #e6eef6;
          border-radius: 6px;
          margin-right: 8px;
          padding: 8px 12px;
        }
        .nav-tabs > li.active > a, .nav-tabs > li > a:hover {
          background: linear-gradient(90deg, rgba(29,92,140,0.08), rgba(29,92,140,0.04));
          color: #1f5a8a !important;
          border-left: 4px solid #1f5a8a;
          padding-left: 10px;
        }
        .ndc-ds.disabled-ds {
          color: #9aa6b2;
          cursor: not-allowed;
          pointer-events: none;
          opacity: 0.6;
        }
        .ndc-category.disabled-category {
          opacity: 0.55;
          pointer-events: none;
          cursor: not-allowed;
        }
        .ndc-category.disabled-category summary {
          color: #9aa6b2;
          cursor: not-allowed;
        }
        .nav-tabs {
          margin-bottom: 18px;
        }
        .dataset-controls label, .dataset-controls .control-label {
          color: #1f5a8a; font-weight: 600;
        }
        .dataset-controls .form-group {
          margin-bottom: 16px;
        }
        .dataset-controls .radio {
          margin-bottom: 14px;
        }
        .dataset-controls .shiny-date-input, .dataset-controls .shiny-date-range-input {
          margin-bottom: 16px;
        }
        .btn-disabled {
          background-color: #e9ecef !important;
          color: #6c757d !important;
          border: none !important;
          cursor: not-allowed !important;
          box-shadow: none !important;
        }
      ")),
      tags$script(HTML("
        function ndcSelectProject(el, key) {
          var was = el.classList.contains('selected');
          document.querySelectorAll('.fixed-item').forEach(function(e){ e.classList.remove('selected'); });
          if (!was) {
            el.classList.add('selected');
            Shiny.setInputValue('ndc_project', key, {priority: 'event'});
          } else {
            Shiny.setInputValue('ndc_project', '', {priority: 'event'});
          }
        }
        Shiny.addCustomMessageHandler('ndc_select_fixed', function(value) {
          document.querySelectorAll('.fixed-item').forEach(function(el){ el.classList.remove('selected'); });
        });
        Shiny.addCustomMessageHandler('ndc_trigger_download', function(value) {
          // The Shiny downloadButton renders as an anchor with id download_data.
          // Click that anchor so the browser follows its download href; a tiny
          // delay ensures Shiny has set the href before we click.
          setTimeout(function() {
            var el = document.getElementById('download_data');
            if (!el) return;
            var anchor = (el.tagName === 'A') ? el : el.querySelector('a');
            if (anchor && anchor.href) {
              anchor.click();
            } else if (el.click) {
              el.click();
            }
          }, 150);
        });
        Shiny.addCustomMessageHandler('ndc_select_dataset', function(value) {
          document.querySelectorAll('.ndc-ds:not(.fixed-item)').forEach(function(el){ el.classList.remove('selected');});
          if (!value) return;
          var el = document.getElementById('ndc-ds-' + String(value).split(' ').join('_'));
          if (el) el.classList.add('selected');
        });
        Shiny.addCustomMessageHandler('ndc_toggle_statistics', function(disable) {
          var anchors = Array.from(document.querySelectorAll('ul.nav-tabs a[data-value]')).filter(function(a){
            return a.getAttribute('data-value') === 'Statistics';
          });
          anchors.forEach(function(a){
            if (disable) {
              a.classList.add('disabled');
              a.setAttribute('aria-disabled', 'true');
              if (a.parentElement.classList.contains('active')) {
                var ul = a.closest('ul.nav-tabs');
                if (ul) {
                  var rasterAnchor = Array.from(ul.querySelectorAll('a[data-value]')).find(function(x){ return x.getAttribute('data-value') === 'Geodata'; });
                  if (rasterAnchor) { rasterAnchor.click(); }
                }
              }
            } else {
              a.classList.remove('disabled');
              a.removeAttribute('aria-disabled');
            }
          });
        });
        Shiny.addCustomMessageHandler('ndc_toggle_add_button', function(enabled) {
          var btn = document.getElementById('add_dataset');
          if (!btn) return;
          btn.disabled = !enabled;
        });
      "))
    ),
    tags$div(
      class = "app-header",
      tags$img(src = "ndc-www/LTER-LIFE-logo.png", height = "70px"),
      tags$div(tags$div(class = "app-title", "Nature Data Cube \u2014 Demo")),
      tags$a(
        href = "https://ndc-test.containers.wur.nl", target = "_blank", rel = "noopener noreferrer",
        class = "header-link",
        tags$svg(
          xmlns = "http://www.w3.org/2000/svg", viewBox = "0 0 24 24", fill = "none",
          stroke = "currentColor", `stroke-width` = "2", `stroke-linecap` = "round", `stroke-linejoin` = "round",
          tags$path(d = "M18 13v6a2 2 0 0 1-2 2H5a2 2 0 0 1-2-2V8a2 2 0 0 1 2-2h6"),
          tags$polyline(points = "15 3 21 3 21 9"),
          tags$line(x1 = "10", y1 = "14", x2 = "21", y2 = "3")
        ),
        "NatureDataCube API"
      )
    ),
    sidebarLayout(
      position = "left",
      sidebarPanel(
        tags$div(id = "fixed_selector"),
        tags$h4("Define area of interest", style = "color: #1f5a8a; font-weight: 600; margin-top: 6px; margin-bottom: 6px;"),
        tags$h5("Select a project, upload a polygon, or draw your own polygon", style = "color: #1f5a8a; font-weight: 600; margin-top: 4px; margin-bottom: 8px;"),
        tags$details(class = "ndc-category",
                     tags$summary("Projects"),
                     tags$details(class = "ndc-subcategory",
                                  tags$summary("LTER sites"),
                                  lapply(lter_class_levels, function(cls) {
                                    tags$a(id = paste0("proj-lter-", gsub(" ", "_", cls)), class = "fixed-item ndc-ds", href = "#",
                                           onclick = HTML(sprintf("ndcSelectProject(this, 'lter:%s'); return false;", cls)), cls)
                                  })
                     ),
                     tags$a(id = "proj-snl", class = "fixed-item ndc-ds ndc-leaf", href = "#",
                            onclick = HTML("ndcSelectProject(this, 'snl'); return false;"), "SNL sites")
        ),
        tags$details(class = "ndc-category disabled-category",
                     tags$summary("Upload your own polygon(s)"),
                     tags$div(style = "margin-top:8px;",
                              helpText(paste0("Supported file formats: .gpkg, .shp. For shapefiles, upload all layers: .shp, .shx, .dbf, and preferably .prj. ",
                                              "The files together can be up to ", format(round(max_upload_mb()), trim = TRUE), " MB.")),
                              fileInput("upload", "Upload polygons", multiple = TRUE, accept = c(".gpkg", ".shp", ".shx", ".dbf", ".prj", ".zip", ".geojson", ".json", ".kml")),
                              uiOutput("upload_panel"),
                              tags$div(style = "margin-top:6px;"))
        ),
        tags$details(class = "ndc-category disabled-category",
                     tags$summary("Draw your own polygon"),
                     tags$div(style = "margin-top:8px;", helpText("Use the draw toolbar on the map below to create polygon(s). Click a polygon to select or deselect it. Use the edit/remove tools on the map toolbar to delete drawn shapes."), tags$div(style = "margin-top:6px;"))
        ),
        leafletOutput("map", height = "400px"),
        uiOutput("snl_status"),
        br(),
        h4("Choose dataset(s)"),
        tags$div(
          tags$details(class = "ndc-category",
                       tags$summary("Atmosphere"),
                       tags$div(class = "dataset-row", tags$a(id = "ndc-ds-Weather", class = "ndc-ds", href = "#", onclick = HTML("document.querySelectorAll('.ndc-ds:not(.fixed-item)').forEach(e=>e.classList.remove('selected')); this.classList.add('selected'); Shiny.setInputValue('selected_dataset', 'Weather', {priority: 'event'}); return false;"), "Weather"), actionButton("info_ds_Weather", "i", class = "btn-info-circle")),
                       tags$div(class = "dataset-row", tags$a(id = "ndc-ds-Nitrogen", class = "ndc-ds", href = "#", onclick = HTML("document.querySelectorAll('.ndc-ds:not(.fixed-item)').forEach(e=>e.classList.remove('selected')); this.classList.add('selected'); Shiny.setInputValue('selected_dataset', 'Nitrogen', {priority: 'event'}); return false;"), "Nitrogen"), actionButton("info_ds_Nitrogen", "i", class = "btn-info-circle"))
          ),
          tags$details(
            class = "ndc-category",
            tags$summary("Biosphere"),

            tags$div(
              class = "dataset-row",
              tags$a(
                id = "ndc-ds-NDVI",
                class = "ndc-ds",
                href = "#",
                onclick = HTML("document.querySelectorAll('.ndc-ds:not(.fixed-item)').forEach(e=>e.classList.remove('selected')); this.classList.add('selected'); Shiny.setInputValue('selected_dataset', 'NDVI', {priority: 'event'}); return false;"),
                "NDVI (greenness)"
              ),
              actionButton("info_ds_NDVI", "i", class = "btn-info-circle")
            ),

            tags$div(
              class = "dataset-row",
              tags$span(
                id = "ndc-ds-Vegetation_structure",
                class = "ndc-ds disabled-ds",
                "Vegetation structure (coming soon)"
              ),
              actionButton("info_ds_Vegetation_structure", "i", class = "btn-info-circle")
            )
          ),
          tags$details(class = "ndc-category",
                       tags$summary("Hydrosphere"),
                       tags$div(
                         class = "dataset-row",
                         tags$span(
                           id = "ndc-ds-Ground_water_table",
                           class = "ndc-ds disabled-ds",
                           "Ground water table (coming soon)"
                         ),
                         actionButton("info_ds_Ground_water_table", "i", class = "btn-info-circle")
                       )
          ),

          tags$details(class = "ndc-category",
                       tags$summary("Geosphere"),
                       tags$div(class = "dataset-row", tags$a(id = "ndc-ds-Soil_map", class = "ndc-ds", href = "#", onclick = HTML("document.querySelectorAll('.ndc-ds:not(.fixed-item)').forEach(e=>e.classList.remove('selected')); this.classList.add('selected'); Shiny.setInputValue('selected_dataset', 'Soil map', {priority: 'event'}); return false;"), "Soil Map"), actionButton("info_ds_Soil_map", "i", class = "btn-info-circle")),
                       tags$div(class = "dataset-row", tags$a(id = "ndc-ds-AHN", class = "ndc-ds", href = "#", onclick = HTML("document.querySelectorAll('.ndc-ds:not(.fixed-item)').forEach(e=>e.classList.remove('selected')); this.classList.add('selected'); Shiny.setInputValue('selected_dataset', 'AHN', {priority: 'event'}); return false;"), "Elevation (AHN)"), actionButton("info_ds_AHN", "i", class = "btn-info-circle"))
          ),
          tags$details(class = "ndc-category",
                       tags$summary("Anthroposphere"),
                       tags$div(class = "dataset-row", tags$a(id = "ndc-ds-Agricultural_fields", class = "ndc-ds", href = "#", onclick = HTML("document.querySelectorAll('.ndc-ds:not(.fixed-item)').forEach(e=>e.classList.remove('selected')); this.classList.add('selected'); Shiny.setInputValue('selected_dataset', 'Agricultural fields', {priority: 'event'}); return false;"), "Agricultural fields"), actionButton("info_ds_Agricultural_fields", "i", class = "btn-info-circle")),
                       tags$div(class = "dataset-row", tags$a(id = "ndc-ds-Land_Use", class = "ndc-ds", href = "#", onclick = HTML("document.querySelectorAll('.ndc-ds:not(.fixed-item)').forEach(e=>e.classList.remove('selected')); this.classList.add('selected'); Shiny.setInputValue('selected_dataset', 'Land Use', {priority: 'event'}); return false;"), "Land Use"), actionButton("info_ds_Land_Use", "i", class = "btn-info-circle"))
          )
        )
      ),
      mainPanel(
        h4("Overview of selected datasets"),
        uiOutput("overview_table"),
        actionButton("clear_overview", "Clear overview", class = "btn-custom"),
        br(),
        tags$div(style = "margin-top:12px;"),
        uiOutput("download_ui"),
        br(),
        uiOutput("download_messages"),
        hr(),
        uiOutput("dataset_metadata"),
        uiOutput("availability"),
        tags$div(style = "margin-top:18px;", actionButton("add_dataset", "Add to overview", class = "btn-custom"))
      )
    ),
    div(class = "help-button-container", actionButton("open_guide", "?", class = "btn-help-circle"))
  )
}
