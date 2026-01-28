test_that("keep/drop filters work for local assays", {
  assay <- make_toy_assay()
  kept <- keep_samples(assay, c("s2", "s3"))
  expect_equal(rownames(kept$X), c("s2", "s3"))
  expect_equal(rownames(kept$samp), c("s2", "s3"))

  dropped <- drop_features(assay, "f2")
  expect_false("f2" %in% colnames(dropped$X))
  expect_equal(rownames(dropped$feat), c("f1", "f3"))
})

test_that("select_*_where works for local assays and errors without metadata", {
  assay <- make_toy_assay()
  selected <- select_samples_where(assay, "project_id == 'P1'")
  expect_equal(rownames(selected$X), c("s1", "s2"))
  selected_feat <- select_features_where(assay, "Genus == 'A'")
  expect_equal(colnames(selected_feat$X), "f1")

  assay_no_meta <- new_assay(assay$X)
  expect_error(select_samples_where(assay_no_meta, "project_id == 'P1'"), "No samp metadata")
  expect_error(select_features_where(assay_no_meta, "Genus == 'A'"), "No feat metadata")
})

test_that("keep/drop filters set lazy filters and sample_bucket", {
  info <- make_duckdb_atlas(include_partition = TRUE)
  ref <- assay(info$atlas, "16S")
  ref <- keep_samples(ref, c("s1", "s2"))
  expect_true(grepl("sample_id IN", ref$sample_filter$where))
  expect_true(grepl("sample_bucket IN", ref$sample_filter$where))

  ref <- drop_features(ref, "f2")
  expect_true(grepl("feature_id NOT IN", ref$feature_filter$where))
})

test_that("select_*_where uses views for assay_ref", {
  info <- make_duckdb_atlas()
  ref <- assay(info$atlas, "16S")
  ref2 <- select_samples_where(ref, "project_id == 'P1'")
  expect_true(length(ref2$sample_filter$ids) > 0)
  ref3 <- select_features_where(ref, "Genus == 'A'")
  expect_true(length(ref3$feature_filter$ids) > 0)

  ref_no_samples <- ref
  ref_no_samples$atlas$views$samples <- NULL
  expect_error(select_samples_where(ref_no_samples, "project_id == 'P1'"), "No samples view")
})
