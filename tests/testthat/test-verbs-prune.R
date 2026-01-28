test_that("prune local supports sample and feature margins with predicate", {
  assay <- make_toy_assay()
  pruned_sample <- prune(assay, min_total = 6, margin = "sample")
  expect_true(all(Matrix::rowSums(pruned_sample$X) >= 6))

  pruned_feature <- prune(assay, min_total = 5, margin = "feature")
  expect_true(all(Matrix::colSums(pruned_feature$X) >= 5))

  pruned_pred <- prune(assay, margin = "feature", predicate = function(df) df$total_abundance > 2)
  expect_equal(rownames(pruned_pred$feat), colnames(pruned_pred$X))
})

test_that("prune lazy uses stats when available and falls back otherwise", {
  info <- make_duckdb_atlas(include_stats = TRUE)
  ref <- assay(info$atlas, "16S")
  pruned <- prune(ref, min_total = 3, margin = "sample")
  expect_true(length(pruned$sample_filter$ids) > 0)

  con <- info$atlas$con
  tmp <- tempdir()
  sample_stats <- data.frame(sample_id = c("s1", "s2"), total_abundance = c(3, 3), richness = c(2, 1))
  sample_stats_path <- file.path(tmp, "sample_stats_missing.parquet")
  write_parquet(con, "sample_stats_missing", sample_stats, sample_stats_path)
  info$atlas$manifest$assays$`16S`$stats$sample_stats_uri <- sample_stats_path

  ref2 <- assay(info$atlas, "16S")
  pruned2 <- prune(ref2, min_total = 3, min_prev = 0.1, margin = "sample")
  expect_true(length(pruned2$sample_filter$ids) > 0)
})
