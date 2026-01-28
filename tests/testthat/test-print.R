test_that("print methods emit summaries", {
  assay <- make_toy_assay()
  out <- testthat::capture_output(print(assay))
  expect_true(any(grepl("Samples", out)))

  info <- make_duckdb_atlas()
  ref <- assay(info$atlas, "16S")
  out_ref <- testthat::capture_output(print(ref))
  expect_true(any(grepl("Assay:", out_ref)))

  ctx <- by_sample(ref)
  out_ctx <- testthat::capture_output(print(ctx))
  expect_true(any(grepl("Margin", out_ctx)))
})
