# Cleaning the polygons that users bring: invalid geometry, missing coordinate systems, zip files, temporary folders.

bowtie <- function(crs = 4326) {
  sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(rbind(c(5, 52), c(5.02, 52.02), c(5.02, 52), c(5, 52.02), c(5, 52)))),
                                  crs = crs))
}

square_sf <- function(x = 5.7, crs = 4326) {
  sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(rbind(c(x, 52), c(x + 0.01, 52), c(x + 0.01, 52.01), c(x, 52.01), c(x, 52)))),
                                  crs = crs))
}

# a zip with the given files (names are relative to `root`)
make_zip <- function(root, files, zipfile = withr::local_tempfile(fileext = ".zip", .local_envir = parent.frame())) {
  zip::zipr(zipfile, files = files, root = root)
  zipfile
}

temp_folders <- function() list.files(tempdir(), "^(unzip_|shp_bundle_)")

test_that("a polygon that crosses itself is repaired, with a note", {
  out <- polygon_clean(bowtie(), "bow.gpkg")
  expect_equal(nrow(out), 1)
  expect_true(all(sf::st_is_valid(out)))
  expect_gt(as.numeric(sf::st_area(sf::st_transform(out, 28992))), 100)
  expect_match(polygon_notes(out), "bow.gpkg: 1 invalid polygon\\(s\\).*repaired")

  # valid polygons come back without notes
  expect_length(polygon_notes(polygon_clean(square_sf())), 0)
})

test_that("what is not a polygon is dropped, Z is removed and other coordinate systems are transformed", {
  points <- sf::st_sf(geometry = sf::st_sfc(sf::st_point(c(5, 52)), crs = 4326))
  expect_null(polygon_clean(points))

  both <- rbind(square_sf()[, "geometry"], sf::st_sf(geometry = sf::st_sfc(sf::st_point(c(5, 52)), crs = 4326)))
  expect_equal(nrow(polygon_clean(both)), 1)

  rd <- polygon_clean(square_sf(crs = 4326) |> sf::st_transform(28992))
  expect_equal(sf::st_crs(rd), sf::st_crs(4326))
  expect_equal(sf::st_bbox(rd)[["xmin"]], 5.7, tolerance = 1e-4)
  expect_length(polygon_notes(rd), 0)

  z <- sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(rbind(c(5, 52, 1), c(5.01, 52, 1), c(5.01, 52.01, 1), c(5, 52, 1))), dim = "XYZ"),
                                       crs = 4326))
  expect_equal(as.character(sf::st_geometry_type(polygon_clean(z))), "POLYGON")
  expect_false(grepl(" Z ", sf::st_as_text(sf::st_geometry(polygon_clean(z)))))
})

test_that("a missing coordinate system is assumed to be WGS84, and the user is told", {
  none <- square_sf(crs = sf::NA_crs_)
  out <- polygon_clean(none, "plots.shp")
  expect_equal(sf::st_crs(out), sf::st_crs(4326))
  expect_match(polygon_notes(out), "plots.shp has no coordinate system: WGS84")
})

test_that("a polygon that cannot be repaired is left out, with a note", {
  local_mocked_bindings(st_make_valid = function(...) stop("no GEOS"), .package = "sf")
  out <- polygon_clean(rbind(bowtie(), square_sf()), "mix.gpkg")
  expect_equal(nrow(out), 1)
  expect_match(polygon_notes(out), "1 invalid polygon\\(s\\) could not be repaired")

  none <- polygon_clean(bowtie(), "bow.gpkg")
  expect_equal(nrow(none), 0)
  expect_match(polygon_notes(none), "could not be repaired")
})

test_that("a polygon drawn across itself is repaired", {
  feat <- list(geometry = list(type = "Polygon",
                               coordinates = list(list(list(5, 52), list(5.02, 52.02), list(5.02, 52), list(5, 52.02), list(5, 52)))))
  out <- polygon_from_drawing(feat, 3)
  expect_equal(out$layer_id, 3L)
  expect_true(all(sf::st_is_valid(out)))
  expect_match(polygon_notes(out), "The polygon: 1 invalid")
  expect_match(out$wkt, "^(MULTI)?POLYGON")
})

test_that("a zip is refused when it has too many files, unpacks too large or has paths outside the folder", {
  listing <- function(names, length = 10) data.frame(Name = names, Length = length)
  expect_null(polygon_zip_problem(listing(c("a.shp", "dir/b.shx"))))
  expect_match(polygon_zip_problem(listing(letters[1:5]), max_files = 4), "has 5 files \\(at most 4")
  expect_match(polygon_zip_problem(listing("a", length = 3 * 1024^2), max_mb = 2), "larger than 2 MB when unpacked")
  for (bad in c("../evil.gpkg", "a/../../evil.gpkg", "/etc/evil", "C:/evil", "a\\..\\evil")) {
    expect_match(polygon_zip_problem(listing(c("ok.gpkg", bad))), "outside the archive", info = bad)
  }
  expect_null(polygon_zip_problem(listing("my..file.gpkg")))  # dots in a name are fine
})

test_that("a zip with a GeoPackage is read, also from a folder, and nothing is left in the temp folder", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "data"))
  sf::st_write(square_sf(), file.path(root, "data", "a.gpkg"), quiet = TRUE)
  zipfile <- make_zip(root, "data")
  before <- temp_folders()

  res <- polygon_process_upload(data.frame(name = "plots.zip", datapath = zipfile))
  expect_equal(nrow(res$imported), 1)
  expect_equal(res$imported$source_name, "plots.zip")
  expect_length(res$kept_dirs, 0)
  expect_identical(temp_folders(), before)
})

test_that("a zip that is too large to accept, or not a zip, is reported", {
  root <- withr::local_tempdir()
  for (n in 1:3) sf::st_write(square_sf(5 + n), file.path(root, paste0("f", n, ".gpkg")), quiet = TRUE)
  zipfile <- make_zip(root, paste0("f", 1:3, ".gpkg"))
  local_mocked_bindings(max_zip_files = 2L)
  res <- polygon_process_upload(data.frame(name = "many.zip", datapath = zipfile))
  expect_null(res$imported)
  expect_match(res$notes, "many.zip was not read: it has 3 files \\(at most 2")

  junk <- withr::local_tempfile(fileext = ".zip")
  writeLines("not a zip", junk)
  res <- suppressWarnings(polygon_process_upload(data.frame(name = "junk.zip", datapath = junk)))
  expect_match(res$notes, "junk.zip was not read: it is not a readable zip file")
})

test_that("a zipped GeoPackage with several layers keeps its folder for the choice, which is returned", {
  root <- withr::local_tempdir()
  multi <- file.path(root, "multi.gpkg")
  sf::st_write(square_sf(5.7), multi, layer = "a", quiet = TRUE)
  sf::st_write(square_sf(5.9), multi, layer = "b", quiet = TRUE)
  zipfile <- make_zip(root, "multi.gpkg")
  before <- temp_folders()

  res <- polygon_process_upload(data.frame(name = "layers.zip", datapath = zipfile))
  withr::defer(unlink(res$kept_dirs, recursive = TRUE))
  expect_null(res$imported)
  expect_length(res$kept_dirs, 1)
  expect_true(file.exists(res$pending_gpkg[[1]]$datapath))
  expect_equal(setdiff(temp_folders(), before), basename(res$kept_dirs))
})

test_that("a shapefile leaves no folder behind", {
  root <- withr::local_tempdir()
  sf::st_write(square_sf(), file.path(root, "plots.shp"), quiet = TRUE)
  files <- list.files(root, full.names = TRUE)
  before <- temp_folders()

  res <- polygon_process_upload(data.frame(name = basename(files), datapath = files))
  expect_equal(nrow(res$imported), 1)
  expect_identical(temp_folders(), before)
})

# ---- in the app ----

test_that("the user is told what was assumed or repaired in an upload", {
  rec <- new.env()
  rec$notes <- character(0)
  local_mocked_bindings(showNotification = function(ui, ...) rec$notes <- c(rec$notes, ui))
  path <- withr::local_tempfile(fileext = ".gpkg")
  suppressMessages(sf::st_write(rbind(bowtie(), square_sf())[, "geometry"] |> sf::st_set_crs(sf::NA_crs_), path, quiet = TRUE))

  with_server({
    session$setInputs(upload = data.frame(name = "plots.gpkg", size = 1L, type = "", datapath = path))
    expect_equal(nrow(uploaded_polys()), 2)
    expect_true(all(sf::st_is_valid(uploaded_polys())))
  })
  expect_true(any(grepl("plots.gpkg has no coordinate system", rec$notes)))
  expect_true(any(grepl("1 invalid polygon\\(s\\)", rec$notes)))
})

test_that("a zipped multi-layer GeoPackage is unpacked once and its folder goes when the session ends", {
  root <- withr::local_tempdir()
  multi <- file.path(root, "multi.gpkg")
  sf::st_write(square_sf(5.7), multi, layer = "a", quiet = TRUE)
  sf::st_write(square_sf(5.9), multi, layer = "b", quiet = TRUE)
  zipfile <- make_zip(root, "multi.gpkg")
  rec <- new.env()

  with_server({
    session$setInputs(upload = data.frame(name = "layers.zip", size = 1L, type = "", datapath = zipfile))
    pid <- names(pending_gpkg())
    rec$folder <- dirname(pending_gpkg()[[pid]]$datapath)
    expect_true(dir.exists(rec$folder))

    import_layer(session, pid, "b")
    expect_equal(nrow(uploaded_polys()), 1)
    expect_true(dir.exists(rec$folder))  # still needed for the other layer
    session$close()
  })
  expect_false(dir.exists(rec$folder))
})

test_that("a polygon drawn across itself is repaired, and the user is told", {
  rec <- new.env()
  rec$notes <- character(0)
  local_mocked_bindings(showNotification = function(ui, ...) rec$notes <- c(rec$notes, ui))
  feat <- list(type = "Feature",
               geometry = list(type = "Polygon",
                               coordinates = list(list(list(5, 52), list(5.02, 52.02), list(5.02, 52), list(5, 52.02), list(5, 52)))))

  with_server({
    session$setInputs(map_draw_new_feature = feat)
    expect_equal(nrow(drawn_features()), 1)
    expect_true(all(sf::st_is_valid(drawn_features())))
    expect_equal(drawn_features()$source_name, "Own polygon_1")
  })
  expect_true(any(grepl("invalid polygon", rec$notes)))
})

test_that("polygon_read keeps polygons, assumes EPSG:4326 and drops other geometries", {
  skip_if_not_installed("sf")
  path <- withr::local_tempfile(fileext = ".geojson")
  sf::st_write(rbind(selected_polygon()[, "geometry"],
                     sf::st_sf(geometry = sf::st_sfc(sf::st_point(c(5, 52)), crs = 4326))),
               path, quiet = TRUE)
  out <- polygon_read(path)
  expect_equal(nrow(out), 1)
  expect_equal(sf::st_crs(out), sf::st_crs(4326))
  expect_null(polygon_read(withr::local_tempfile(fileext = ".gpkg")))  # does not exist
})

test_that("uploaded GeoPackages are imported, or queued when they have several layers", {
  single <- withr::local_tempfile(fileext = ".gpkg")
  sf::st_write(selected_polygon()[, "geometry"], single, layer = "a", quiet = TRUE)
  res <- polygon_process_upload(data.frame(name = "single.gpkg", datapath = single))
  expect_equal(nrow(res$imported), 1)
  expect_equal(res$imported$source_name, "single.gpkg")
  expect_length(res$pending_gpkg, 0)

  multi <- withr::local_tempfile(fileext = ".gpkg")
  sf::st_write(selected_polygon()[, "geometry"], multi, layer = "a", quiet = TRUE)
  sf::st_write(selected_polygon()[, "geometry"], multi, layer = "b", quiet = TRUE)
  res <- polygon_process_upload(data.frame(name = "multi.gpkg", datapath = multi))
  expect_null(res$imported)
  expect_equal(res$pending_gpkg[[1]]$layers, c("a", "b"))

  expect_equal(polygon_process_upload(NULL), list(imported = NULL, pending_gpkg = NULL))
})
