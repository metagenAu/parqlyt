test_that("as_long errors on assay_ref and respects max_nnz", {
  info <- make_duckdb_atlas()
  ref <- assay(info$atlas, "16S")
  expect_error(as_long(ref), "requires collect")

  assay <- make_toy_assay()
  expect_error(as_long(assay, max_nnz = 1), "Too many nonzeros")
  long <- as_long(assay, max_nnz = 10)
  expect_true(all(c("sample_id", "feature_id", "x") %in% colnames(long)))
})

test_that("samp_tbl and feat_tbl return metadata", {
  assay <- make_toy_assay()
  samp <- samp_tbl(assay)
  feat <- feat_tbl(assay)
  expect_true("sample_id" %in% colnames(samp))
  expect_true("feature_id" %in% colnames(feat))

  info <- make_duckdb_atlas()
  ref <- assay(info$atlas, "16S")
  samp_ref <- samp_tbl(ref)
  feat_ref <- feat_tbl(ref)
  expect_true(nrow(samp_ref) > 0)
  expect_true(nrow(feat_ref) > 0)

  ref_no_samples <- ref
  ref_no_samples$atlas$views$samples <- NULL
  expect_true(is.null(samp_tbl(ref_no_samples)))
})
