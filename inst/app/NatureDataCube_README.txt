
NatureDataCube — Quick Start Guide
===================================

Overview
--------
This app helps you define an area of interest and request raster or statistics datasets (e.g. Weather, NDVI, Soil) for that area.
You can select an existing project area, draw your own polygon on the map, or upload polygon files (GeoPackage, Shapefile).

1) Define area of interest (left sidebar)
   - Click a project under "Projects" to show that project's polygons on the map or
   - Draw your own polygon using the draw toolbar on the map (use the polygon tool: click each corner and finish on the first one; the trash can removes drawn polygons) or
   - Upload polygon files using the Upload panel. Supported formats: .gpkg, .shp (with .shx, .dbf and preferably .prj), .geojson, .kml, or a .zip containing these. Every polygon of a file can be selected on its own. A file without a coordinate system is taken to be in WGS84.
   - Datasets can be added for polygons up to the area limit shown in the panels (Weather has no limit); the AgroDataCube dataset Agricultural fields receives a simplified polygon if yours has very many vertices.
   - Click polygons on the map to select / deselect them. Selected polygons are highlighted blue and will be used for dataset queries.

   - Click "Check availability" (below the map) and the datasets in the list are marked: a check mark when data was found, a cross when none was found, a question mark when the check could not be made. Point at a dataset to read why. A dataset with no data for a polygon cannot be added for that polygon.

2) Choosing a dataset (left sidebar)
   - The button "Check availability" (below the map) marks the datasets in the menu that have data for the selected polygons (check mark: data found, cross: none, question mark: could not be checked; point at a dataset for the details) and lists all datasets in a dialog, optionally for a period only. Datasets of the NatureDataCube are checked against their items; the AgroDataCube and GroenMonitor datasets are shown by the coverage that is known for them ("expected" / "outside its coverage"), without asking those services. The marks go (with a yellow warning) when you select other polygons or change a year or date, and come back when you check again. Adding a dataset also checks it: a polygon without data for it is not added.
   - Use the "Choose dataset for the selected area" menu to pick a dataset (e.g. Weather, NDVI).
   - Click the small (i) icon next to a dataset to see dataset metadata/description (information modal).
   - After choosing a dataset, pick the finer controls shown (date, year, month) in the right-hand panel that appears.

3) Adding datasets to the overview
   - After selecting polygons and choosing dataset settings, click "Add to overview".
   - Each added combination appears in the "Overview of selected datasets" table, showing dataset, type, date, polygon name, and layers.
   - The app avoids exact duplicates (same polygon + same dataset + same date/settings).

4) Downloading data
   - When you've added at least one dataset to the overview, the "Download dataset(s)" button will appear.
   - Click "Download dataset(s)" to start retrieving datasets and packaging them into a ZIP file. Progress messages appear in the "Updates on dataset retrieval" area.
   - The ZIP file has the data, the selected polygons, download_summary.csv (what happened to each dataset, with the reference and license of the datasets that the NatureDataCube describes) and CITATIONS.txt (the references to cite for those datasets, and which datasets come from another service, such as the AgroDataCube). The folder "parameters" has a small file per dataset with the parameters of its retrieval: in R, rNDC::ndc_repeat("parameters/<file>.json") repeats it. Untick "Include the parameters of each dataset" below the download button to leave it out.
   - The "Return data to R (close app)" button will run the retrieval and then exit the app returning the results to the calling R session (useful when launching app from RStudio).

5) Tips & troubleshooting
   - If an uploaded GeoPackage does not appear, check that it isn't corrupted and that it contains polygon layers.
   - For shapefiles inside a ZIP: ensure all shapefile components (.shp, .shx, .dbf, .prj) are present in the ZIP.
   - Coordinate reference systems: the app transforms polygons to EPSG:4326 (WGS84) where possible.
   - If dataset retrieval fails, check the messages area for details shown after retrieval attempts.
 

Enjoy exploring — and thanks for using the NatureDataCube app!
