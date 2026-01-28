test_that("new_assay validates input and trims metadata", {
  expect_error(new_assay(matrix(1, 1, 1)), "dgCMatrix")

  X <- Matrix::sparseMatrix(i = 1, j = 1, x = 1, dims = c(1, 1))
  expect_error(new_assay(X), "rownames")

  X <- Matrix::sparseMatrix(i = 1, j = 1, x = 1, dims = c(2, 2),
                            dimnames = list(c("s1", "s2"), c("f1", "f2")))
  feat <- data.frame(val = 1, row.names = "f1")
  samp <- data.frame(val = 1, row.names = "s2")
  assay <- new_assay(X, feat = feat, samp = samp)
  expect_equal(rownames(assay$X), "s2")
  expect_equal(colnames(assay$X), "f1")
})

test_that("new_assay_ref and new_assay_ctx initialize classes", {
  atlas <- structure(list(), class = "atlas")
  ref <- new_assay_ref(atlas, "16S")
  expect_true(inherits(ref, "assay_ref"))
  expect_true(is.null(ref$sample_filter))

  ctx <- new_assay_ctx(ref, "sample")
  expect_true(inherits(ctx, "assay_ctx"))
  expect_equal(ctx$margin, "sample")
})

test_that("by_sample/by_feature wrap and end unwraps", {
  assay <- make_toy_assay()
  ctx <- by_sample(assay)
  expect_true(inherits(ctx, "assay_ctx"))
  expect_equal(end(ctx), assay)
  expect_equal(end(assay), assay)
})
