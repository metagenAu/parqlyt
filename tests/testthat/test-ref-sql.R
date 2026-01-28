test_that("apply_transform_sql handles expressions and relab", {
  base <- "SELECT sample_id, feature_id, x FROM t"
  expr_sql <- apply_transform_sql(base, list(type = "expr", expr = "log1p(x)"))
  expect_true(grepl("log1p\\(x\\)", expr_sql))

  relab_sql <- apply_transform_sql(base, list(type = "relab"))
  expect_true(grepl("SUM\\(x\\)", relab_sql))

  expect_error(apply_transform_sql(base, list(type = "unknown")), "Unknown transform type")
})

test_that("ref_counts_sql composes filters, collapse, and transforms", {
  info <- make_duckdb_atlas()
  ref <- assay(info$atlas, "16S")
  ref <- keep_samples(ref, c("s1"))
  ref <- keep_features(ref, c("f1"))
  ref <- transform(ref, method = "log1p", margin = "sample")
  ref <- collapse_tax(ref, "Genus")
  sql <- ref_counts_sql(ref, include_transform = TRUE)
  expect_true(grepl("sample_id IN", sql))
  expect_true(grepl("feature_id IN", sql))
  expect_true(grepl("GROUP BY", sql))
  expect_true(grepl("log1p", sql))
})

test_that("ref_apply_keep validates empty ids", {
  info <- make_duckdb_atlas()
  ref <- assay(info$atlas, "16S")
  expect_error(ref_apply_keep(ref, character(0), "sample"), "No ids provided")
})
