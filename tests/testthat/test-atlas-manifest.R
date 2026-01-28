test_that("atlas_open validates env and bucket", {
  con <- connect_duckdb()
  old_env <- Sys.getenv(c("ATLAS_R2_KEY_ID", "ATLAS_R2_SECRET", "ATLAS_R2_ACCOUNT_ID", "ATLAS_R2_BUCKET"))
  on.exit(Sys.setenv(old_env), add = TRUE)
  Sys.setenv(ATLAS_R2_KEY_ID = "", ATLAS_R2_SECRET = "", ATLAS_R2_ACCOUNT_ID = "", ATLAS_R2_BUCKET = "")

  testthat::local_mocked_bindings(ensure_httpfs = function(con) TRUE)
  expect_error(atlas_open(con = con), "Missing ATLAS_R2_")

  Sys.setenv(ATLAS_R2_KEY_ID = "k", ATLAS_R2_SECRET = "s", ATLAS_R2_ACCOUNT_ID = "a")
  expect_error(atlas_open(bucket = "", manifest_uri = "", con = con), "bucket required")
})

test_that("atlas_open uses manifest when provided", {
  con <- connect_duckdb()
  manifest <- list(assays = list(`16S` = list(counts_uri = "counts", features_uri = "feat")))
  old_env <- Sys.getenv(c("ATLAS_R2_KEY_ID", "ATLAS_R2_SECRET", "ATLAS_R2_ACCOUNT_ID", "ATLAS_R2_BUCKET"))
  on.exit(Sys.setenv(old_env), add = TRUE)
  Sys.setenv(ATLAS_R2_KEY_ID = "k", ATLAS_R2_SECRET = "s", ATLAS_R2_ACCOUNT_ID = "a", ATLAS_R2_BUCKET = "b")

  testthat::local_mocked_bindings(
    ensure_httpfs = function(con) TRUE,
    set_r2_secret = function(...) TRUE,
    read_manifest = function(...) stop("unexpected read_manifest")
  )

  atlas <- atlas_open(manifest = manifest, con = con)
  expect_true(inherits(atlas, "atlas"))
})

test_that("create_atlas_views builds views including aux", {
  con <- connect_duckdb()
  tmp <- tempdir()
  counts <- data.frame(sample_id = "s1", feature_id = "f1", x = 1, stringsAsFactors = FALSE)
  features <- data.frame(feature_id = "f1", Genus = "A", stringsAsFactors = FALSE)
  samples <- data.frame(sample_id = "s1", project_id = "P1", stringsAsFactors = FALSE)
  aux <- data.frame(feature_id = "f1", extra = 1, stringsAsFactors = FALSE)

  counts_path <- file.path(tmp, "counts_aux.parquet")
  features_path <- file.path(tmp, "features_aux.parquet")
  samples_path <- file.path(tmp, "samples_aux.parquet")
  aux_path <- file.path(tmp, "aux.parquet")
  write_parquet(con, "counts_aux_tmp", counts, counts_path)
  write_parquet(con, "features_aux_tmp", features, features_path)
  write_parquet(con, "samples_aux_tmp", samples, samples_path)
  write_parquet(con, "aux_tmp", aux, aux_path)

  manifest <- list(
    samples_uri = samples_path,
    assays = list(
      `16S` = list(
        counts_uri = counts_path,
        features_uri = features_path,
        aux = list(extra = aux_path)
      )
    )
  )

  views <- create_atlas_views(con, manifest)
  expect_true("samples" %in% names(views))
  expect_true(all(c("features", "counts", "aux_extra") %in% names(views$`16S`)))
  expect_equal(nrow(DBI::dbGetQuery(con, sprintf("SELECT * FROM %s", views$samples))), 1)
})

test_that("assay validates atlas and names", {
  atlas <- structure(list(manifest = list(assays = list(`16S` = list()))), class = "atlas")
  expect_error(assay(list(), "16S"), "atlas must be")
  expect_error(assay(atlas, "18S"), "Unknown assay")
})

test_that("read_manifest requires a single object", {
  con <- connect_duckdb()
  tmp <- tempfile(fileext = ".json")
  writeLines(c('{"a":1}', '{"b":2}'), tmp)
  expect_error(read_manifest(con, tmp), "single object")
})

test_that("normalize_manifest requires assays", {
  expect_error(normalize_manifest(list()), "Manifest missing assays")
})
