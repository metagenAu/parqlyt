test_that("transform local supports log1p/relab/hellinger", {
  assay <- make_toy_assay(include_zero_sample = TRUE)
  log1p_assay <- transform(assay, method = "log1p", margin = "sample")
  expect_true(all(log1p_assay$X@x >= 0))

  expect_warning(relab <- transform(assay, method = "relab", margin = "sample"), "Zero-sum samples")
  expect_true(all(abs(Matrix::rowSums(relab$X)[1:3] - 1) < 1e-8))

  hellinger <- transform(assay, method = "hellinger", margin = "sample")
  expect_true(all(hellinger$X@x >= 0))

  expect_error(transform(assay, method = "relab", margin = "feature"), "only supported for by_sample")
})

test_that("transform lazy appends plan steps", {
  info <- make_duckdb_atlas()
  ref <- assay(info$atlas, "16S")
  ref <- transform(ref, method = "log1p", margin = "sample")
  ref <- transform(ref, method = "hellinger", margin = "sample")
  expect_equal(length(ref$transform_plan), 3)
  expect_equal(ref$transform_plan[[1]]$type, "expr")
  expect_error(transform(ref, method = "relab", margin = "feature"), "only supported for by_sample")
})
