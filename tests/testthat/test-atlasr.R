library(Matrix)

make_toy_assay <- function() {
  X <- Matrix::sparseMatrix(i = c(1, 1, 2, 3), j = c(1, 3, 2, 3), x = c(10, 5, 2, 1),
                            dims = c(3, 3), dimnames = list(c("s1", "s2", "s3"), c("f1", "f2", "f3")))
  feat <- data.frame(Kingdom = c("Bacteria", "Bacteria", "Bacteria"),
                     Genus = c("A", "B", NA), stringsAsFactors = FALSE)
  rownames(feat) <- c("f1", "f2", "f3")
  samp <- data.frame(project_id = c("P1", "P1", "P2"), stringsAsFactors = FALSE)
  rownames(samp) <- c("s1", "s2", "s3")
  new_assay(X, feat = feat, samp = samp)
}

test_that("prune updates alignment", {
  assay <- make_toy_assay()
  pruned <- by_sample(assay) |> prune(min_total = 6) |> end()
  expect_equal(rownames(pruned$X), rownames(pruned$samp))
})

test_that("collapse_tax sums match", {
  assay <- make_toy_assay()
  collapsed <- collapse_tax(assay, "Genus")
  expect_true(all(Matrix::colSums(collapsed$X) == c(10, 2, 6)))
})

test_that("relab sums to 1", {
  assay <- make_toy_assay()
  relab <- by_sample(assay) |> transform("relab") |> end()
  expect_true(all(abs(Matrix::rowSums(relab$X) - 1) < 1e-8))
})

test_that("collect returns dgCMatrix from ref", {
  con <- connect_duckdb()
  tmp <- tempdir()
  counts <- data.frame(sample_id = c("s1", "s1", "s2"), feature_id = c("f1", "f2", "f1"), x = c(1, 2, 3))
  features <- data.frame(feature_id = c("f1", "f2"), Genus = c("A", "B"))
  samples <- data.frame(sample_id = c("s1", "s2"), project_id = c("P1", "P1"))
  counts_path <- file.path(tmp, "counts.parquet")
  features_path <- file.path(tmp, "features.parquet")
  samples_path <- file.path(tmp, "samples.parquet")
  DBI::dbWriteTable(con, "counts_tmp", counts, overwrite = TRUE)
  DBI::dbWriteTable(con, "features_tmp", features, overwrite = TRUE)
  DBI::dbWriteTable(con, "samples_tmp", samples, overwrite = TRUE)
  DBI::dbExecute(con, sprintf("COPY counts_tmp TO '%s' (FORMAT PARQUET)", counts_path))
  DBI::dbExecute(con, sprintf("COPY features_tmp TO '%s' (FORMAT PARQUET)", features_path))
  DBI::dbExecute(con, sprintf("COPY samples_tmp TO '%s' (FORMAT PARQUET)", samples_path))

  manifest <- list(
    version = "v1",
    bucket = "local",
    base_prefix = "atlas/v1",
    samples_uri = samples_path,
    assays = list(
      `16S` = list(
        counts_uri = counts_path,
        features_uri = features_path,
        partition_cols = c("assay", "project_id", "sample_bucket"),
        x_col = "x",
        sample_col = "sample_id",
        feature_col = "feature_id",
        taxonomy_cols = c("Genus")
      )
    )
  )
  manifest_path <- file.path(tmp, "atlas.json")
  jsonlite::write_json(manifest, manifest_path, auto_unbox = TRUE)

  Sys.setenv(ATLAS_R2_KEY_ID = "dummy", ATLAS_R2_SECRET = "dummy", ATLAS_R2_ACCOUNT_ID = "dummy", ATLAS_R2_BUCKET = "local")
  A <- atlas_open(bucket = "local", prefix = "atlas/v1", manifest_uri = manifest_path, con = con)
  ref <- assay(A, "16S")
  collected <- collect(ref, max_features = 10, max_nnz = 10)
  expect_true(inherits(collected$X, "dgCMatrix"))
})
