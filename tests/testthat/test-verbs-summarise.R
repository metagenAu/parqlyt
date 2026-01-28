test_that("summarise works for local assays", {
  assay <- make_toy_assay()
  samp_sum <- summarise(by_sample(assay))
  feat_sum <- summarise(by_feature(assay))
  expect_true(all(c("sample_id", "total_abundance", "richness") %in% colnames(samp_sum)))
  expect_true(all(c("feature_id", "total_abundance", "prevalence_samples") %in% colnames(feat_sum)))
})

test_that("summarise works for assay_ref", {
  info <- make_duckdb_atlas()
  ref <- assay(info$atlas, "16S")
  samp_sum <- summarise(by_sample(ref))
  feat_sum <- summarise(by_feature(ref))
  expect_true(nrow(samp_sum) > 0)
  expect_true(nrow(feat_sum) > 0)
})
