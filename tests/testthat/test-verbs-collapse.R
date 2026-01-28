test_that("collapse_tax validates rank and handles missing values", {
  assay <- make_toy_assay()
  expect_error(collapse_tax(assay, "Species"), "rank column")

  collapsed <- collapse_tax(assay, "Genus", missing_label = "Unknown")
  expect_true(all(grepl("Unknown|A|B", rownames(collapsed$feat))))

  dropped <- collapse_tax(assay, "Genus", drop_missing_rank = TRUE)
  expect_false(any(is.na(dropped$feat$path)))
})

test_that("collapse_by validates group length", {
  assay <- make_toy_assay()
  expect_error(collapse_by(assay, group = c("g1", "g2")), "length of features")
})

test_that("collapse_tax.assay_ref validates taxonomy cols and rank filter", {
  info <- make_duckdb_atlas()
  ref <- assay(info$atlas, "16S")
  expect_error(collapse_tax(ref, "Species"), "rank not found")

  ref2 <- collapse_tax(ref, "Genus", drop_missing_rank = TRUE)
  expect_true(!is.null(ref2$collapse_plan$rank_filter))
})

test_that("collapse_by.assay_ref requires mapping data.frame", {
  info <- make_duckdb_atlas()
  ref <- assay(info$atlas, "16S")
  expect_error(collapse_by(ref, group = c("g1", "g2")), "requires data.frame")

  mapping <- data.frame(feature_id = c("f1", "f2"), group_id = c("g1", "g2"), stringsAsFactors = FALSE)
  ref2 <- collapse_by(ref, mapping)
  expect_equal(ref2$collapse_plan$group_expr, "g.group_id")
})
