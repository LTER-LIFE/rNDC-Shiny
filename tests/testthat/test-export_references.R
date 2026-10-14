# The references and licenses of the retrieved datasets (R/export_references.R), from the STAC collections.

# A small raster (the stack of a retrieved raster dataset)
small_raster <- function() terra::rast(nrows = 2, ncols = 2, vals = 1:4)

# A download summary with the given rows (dataset, view, polygon, status)
summary_of <- function(...) {
  rows <- list(...)
  dplyr::bind_rows(lapply(rows, function(r) {
    export_manifest_row(r[[1]], r[[2]], r[[3]], "2024", "tif", "x.tif", r[[4]])
  }))
}

test_that("the datasets of the NatureDataCube have collections, the others none", {
  expect_equal(export_dataset_collections("Land Use"), "lgn")
  expect_equal(export_dataset_collections("Nitrogen"), c("ntot", "nox", "nh3"))
  expect_equal(export_dataset_collections("NDVI", "Geodata", "Loobos_1"), "ndvi")
  expect_equal(export_dataset_collections("NDVI", "Statistics", "Loobos_1"), "ndvi-lter")
  expect_equal(export_dataset_collections("NDVI", "Statistics", "SNL parcel_3"), "ndvi-snl")
  expect_equal(export_dataset_collections("NDVI", "Statistics", "Own polygon"), character(0))  # no project area
  for (ds in c("Weather", "Soil map", "AHN", "Agricultural fields", "Area", "Vegetation structure")) {
    expect_equal(export_dataset_collections(ds), character(0))
  }
})

test_that("references and licenses are added to the rows that were retrieved", {
  local_stac_api()
  s <- summary_of(list("Area", "Reference polygon", "Own polygon", "ok"),
                  list("Land Use", "Geodata", "Own polygon", "ok"),
                  list("Nitrogen", "Geodata", "Own polygon", "failed"),
                  list("Weather", "Statistics", "Own polygon", "ok"))
  out <- export_add_references(s, ndc_token = "t")

  expect_equal(out$message, character(0))
  expect_s3_class(out$metadata, "tbl_df")
  expect_equal(out$summary$reference[2], "Hazeu et al. (2026). LGN2024. https://doi.org/10.4121/abc")
  expect_equal(out$summary$license[2], "CC-BY-4.0")
  expect_true(all(is.na(out$summary$reference[c(1, 3, 4)])))  # area, failed, no metadata
  expect_true(all(is.na(out$summary$license[c(1, 3, 4)])))

  # the same text for several collections is given once
  n <- export_add_references(summary_of(list("Nitrogen", "Geodata", "Own polygon", "ok")), ndc_token = "t")
  expect_equal(n$summary$reference, "RIVM (2025). Nitrogen deposition.")
})

test_that("nothing is requested when no dataset is described by the NatureDataCube", {
  local_stac_api()
  webmockr::request_registry_clear()
  s <- summary_of(list("Soil map", "Geodata", "Own polygon", "ok"))
  out <- export_add_references(s, ndc_token = "t")
  expect_identical(out$summary, s)
  expect_null(out$metadata)
  expect_length(request_uris(), 0)
  expect_identical(export_add_references(tibble::tibble(), "t")$summary, tibble::tibble())
})

test_that("a failure to read the metadata leaves the summary as it is, and says why", {
  s <- summary_of(list("Land Use", "Geodata", "Own polygon", "ok"))
  out <- export_add_references(s, "t", read_metadata = function(...) stop("the API is down"))
  expect_identical(out$summary, s)
  expect_null(out$metadata)
  expect_equal(out$message, "References could not be read: the API is down")

  # also an rNDC without ndc_metadata(): not a reason to stop a download
  local_mocked_bindings(ndc_metadata = NULL, .package = "rNDC")
  expect_match(export_add_references(s, "t", read_metadata = rNDC::ndc_metadata)$message, "References could not be read")
})

test_that("CITATIONS.txt gives the reference of every retrieved dataset, and says which have none", {
  local_stac_api()
  s <- summary_of(list("Area", "Reference polygon", "Own polygon", "ok"),
                  list("Land Use", "Geodata", "Own polygon", "ok"),
                  list("Nitrogen", "Geodata", "Own polygon", "ok"),
                  list("Nitrogen", "Geodata", "Other polygon", "ok"),
                  list("Soil map", "Geodata", "Own polygon", "ok"),
                  list("NDVI", "Statistics", "Own polygon", "ok"),
                  list("Weather", "Statistics", "Own polygon", "failed"))
  meta <- export_add_references(s, "t")$metadata
  text <- export_citations_text(s, meta, date = as.Date("2026-10-13"))
  all <- paste(text, collapse = "\n")

  expect_match(text[2], "retrieved on 2026-10-13")
  expect_match(all, "Land Use\n--------\nCollection(s): lgn\nHazeu et al. (2026). LGN2024. https://doi.org/10.4121/abc License: CC-BY-4.0.\nRelated: A related paper.", fixed = TRUE)
  # the three nitrogen collections have one text, and the dataset is listed once
  expect_match(all, "Collection(s): ntot, nox, nh3\nRIVM (2025). Nitrogen deposition. License: CC-BY-4.0.", fixed = TRUE)
  expect_equal(sum(text == "Nitrogen"), 1)
  # without a reference: the AgroDataCube dataset and the statistics of a polygon that is no project area
  expect_match(all, "Soil map: retrieved from the AgroDataCube (https://agrodatacube.wur.nl/).", fixed = TRUE)
  expect_match(all, "NDVI: no reference is available.", fixed = TRUE)
  # not the dataset that failed, nor the reference polygon
  expect_false(grepl("Weather", all, fixed = TRUE))
  expect_false(grepl("^Area$", all))
})

test_that("CITATIONS.txt says so when the references could not be read", {
  s <- summary_of(list("Land Use", "Geodata", "Own polygon", "ok"))
  text <- export_citations_text(s, metadata = NULL)
  expect_true(any(grepl("The reference could not be read. See the metadata of the collection(s) lgn", text, fixed = TRUE)))
})

test_that("the download has CITATIONS.txt, and the references in the download summary", {
  local_stac_api()
  local_mocked_bindings(ndc_landuse_raster = function(...) list(stack = small_raster()), .package = "rNDC")
  zip <- withr::local_tempfile(fileext = ".zip")
  res <- retrieve_and_package(overview_row("Land Use", year = 2024L), zipfile = zip, ndc_token = "t", adc_token = "t")
  expect_equal(res$messages, "Retrieved: Land Use raster for year 2024")

  dir <- withr::local_tempdir()
  utils::unzip(zip, exdir = dir)
  expect_true("CITATIONS.txt" %in% list.files(dir))
  text <- readLines(file.path(dir, "CITATIONS.txt"), encoding = "UTF-8")
  expect_true(any(grepl("https://doi.org/10.4121/abc", text, fixed = TRUE)))
  summary <- utils::read.csv(file.path(dir, "download_summary.csv"))
  expect_equal(summary$license[summary$dataset == "Land Use"], "CC-BY-4.0")
  expect_match(summary$reference[summary$dataset == "Land Use"], "LGN2024")
  expect_true(is.na(summary$reference[summary$dataset == "Area"]))
})

test_that("a download goes on when the references cannot be read, and says so", {
  local_stac_api(collections = list())  # the API lists no collections: unknown ones are an error of rNDC
  local_mocked_bindings(ndc_landuse_raster = function(...) list(stack = small_raster()), .package = "rNDC")
  zip <- withr::local_tempfile(fileext = ".zip")
  res <- retrieve_and_package(overview_row("Land Use", year = 2024L), zipfile = zip, ndc_token = "t", adc_token = "t")
  expect_true(res$produced_any)
  expect_equal(res$messages[1], "Retrieved: Land Use raster for year 2024")
  expect_match(res$messages[2], "^References could not be read: Unknown collection")
  dir <- withr::local_tempdir()
  utils::unzip(zip, exdir = dir)
  expect_true(any(grepl("could not be read", readLines(file.path(dir, "CITATIONS.txt")))))
})

test_that("no CITATIONS.txt when nothing was retrieved", {
  local_mocked_bindings(ndc_landuse_raster = function(...) stop("nothing here"), .package = "rNDC")
  res <- retrieve_and_package(overview_row("Land Use", year = 2024L), save_files = TRUE, ndc_token = "t", adc_token = "t")
  withr::defer(unlink(res$out_dir, recursive = TRUE))
  expect_false(res$produced_any)
  expect_false("CITATIONS.txt" %in% list.files(res$out_dir))
})
