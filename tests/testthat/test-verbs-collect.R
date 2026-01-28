test_that("collect enforces limits unless forced", {
  info <- make_duckdb_atlas()
  ref <- assay(info$atlas, "16S")
  expect_error(collect(ref, max_features = 1), "exceeds limits")
  expect_true(inherits(collect(ref, max_features = 1, force = TRUE)$X, "dgCMatrix"))
})

test_that("collect respects metadata toggles and collapse plan", {
  info <- make_duckdb_atlas()
  ref <- assay(info$atlas, "16S")
  collected <- collect(ref, with_feat = FALSE, with_samp = FALSE, max_features = 10, max_nnz = 100)
  expect_true(is.null(collected$feat))
  expect_true(is.null(collected$samp))

  collapsed_ref <- collapse_tax(ref, "Genus")
  collapsed <- collect(collapsed_ref, max_features = 10, max_nnz = 100)
  expect_true("path" %in% colnames(collapsed$feat))
})

test_that("collect respects filter ordering", {
  info <- make_duckdb_atlas()
  ref <- assay(info$atlas, "16S")
  ref <- keep_samples(ref, c("s2", "s1"))
  ref <- keep_features(ref, c("f2", "f1"))
  collected <- collect(ref, max_features = 10, max_nnz = 100)
  expect_equal(rownames(collected$X), c("s2", "s1"))
  expect_equal(colnames(collected$X), c("f2", "f1"))
})
