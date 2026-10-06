
NatureDataCube — Quick Start Guide
===================================

Overview
--------
This app helps you define an area of interest and request raster or statistics datasets (e.g. Weather, NDVI, Soil) for that area.
You can select an existing project area, draw your own polygon on the map, or upload polygon files (GeoPackage, Shapefile).

1) Define area of interest (left sidebar)
   - Click a project under "Projects" to show that project's polygons on the map or
   - Draw your own polygon using the draw toolbar on the map (use the polygon tool) or 
   - Upload polygon files using the Upload panel. Supported formats: .zip (containing shapefile or gpkg),
   - Click polygons on the map to select / deselect them. Selected polygons are highlighted blue and will be used for dataset queries.

2) Choosing a dataset (left sidebar)
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
   - The "Return data to R (close app)" button will run the retrieval and then exit the app returning the results to the calling R session (useful when launching app from RStudio).

5) Tips & troubleshooting
   - If an uploaded GeoPackage does not appear, check that it isn't corrupted and that it contains polygon layers.
   - For shapefiles inside a ZIP: ensure all shapefile components (.shp, .shx, .dbf, .prj) are present in the ZIP.
   - Coordinate reference systems: the app transforms polygons to EPSG:4326 (WGS84) where possible.
   - If dataset retrieval fails, check the messages area for details shown after retrieval attempts.
 

Enjoy exploring — and thanks for using the NatureDataCube app!
