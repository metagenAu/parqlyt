library(Matrix)

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
  write_parquet(con, "counts_tmp", counts, counts_path)
  write_parquet(con, "features_tmp", features, features_path)
  write_parquet(con, "samples_tmp", samples, samples_path)

  manifest <- make_manifest(
    counts_path = counts_path,
    features_path = features_path,
    samples_path = samples_path,
    partition_cols = c("assay", "project_id", "sample_bucket")
  )
  views <- create_atlas_views(con, manifest)
  A <- structure(list(con = con, manifest = manifest, views = views), class = "atlas")
  ref <- assay(A, "16S")
  collected <- collect(ref, max_features = 10, max_nnz = 10)
  expect_true(inherits(collected$X, "dgCMatrix"))
})
