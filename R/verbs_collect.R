#' Collect lazy assay into memory
#'
#' @export
collect <- function(x, ...) {
  UseMethod("collect")
}

#' @export
collect.assay_ctx <- function(x, ...) {
  collect(end(x), ...)
}

#' @export
collect.assay <- function(x, ...) {
  x
}

#' @export
collect.assay_ref <- function(x, with_feat = TRUE, with_samp = TRUE, max_features = 50000,
                              max_nnz = 2e8, max_cells = 5e8, force = FALSE, ...) {
  con <- x$atlas$con
  counts_sql <- ref_counts_sql(x, include_transform = TRUE)
  size_sql <- sprintf(
    "SELECT COUNT(*) AS nnz, COUNT(DISTINCT sample_id) AS n_samples, COUNT(DISTINCT feature_id) AS n_features FROM (%s) c",
    counts_sql
  )
  size <- DBI::dbGetQuery(con, size_sql)
  nnz <- size$nnz[1]
  n_samples <- size$n_samples[1]
  n_features <- size$n_features[1]
  if (!isTRUE(force)) {
    if (n_features > max_features || nnz > max_nnz || (n_samples * n_features) > max_cells) {
      stop("Collection exceeds limits; set force=TRUE to override.")
    }
  }
  data <- DBI::dbGetQuery(con, sprintf(
    "SELECT sample_id, feature_id, x FROM (%s) c ORDER BY sample_id, feature_id",
    counts_sql
  ))
  sample_ids <- if (!is.null(x$sample_filter$ids)) x$sample_filter$ids else sort(unique(data$sample_id))
  feature_ids <- if (!is.null(x$feature_filter$ids)) x$feature_filter$ids else sort(unique(data$feature_id))
  i <- match(data$sample_id, sample_ids)
  j <- match(data$feature_id, feature_ids)
  X <- Matrix::sparseMatrix(i = i, j = j, x = data$x,
                            dims = c(length(sample_ids), length(feature_ids)),
                            dimnames = list(sample_ids, feature_ids))
  samp <- NULL
  feat <- NULL
  assay_manifest <- x$atlas$manifest$assays[[x$assay_name]]
  if (isTRUE(with_samp) && !is.null(x$atlas$views$samples)) {
    tbl <- paste0("tmp_samp_", sample(1e8, 1))
    DBI::dbWriteTable(con, tbl, data.frame(sample_id = sample_ids), overwrite = TRUE, temporary = TRUE)
    sql <- sprintf(
      "SELECT * FROM %s s JOIN %s t ON s.%s = t.sample_id",
      x$atlas$views$samples, tbl, assay_manifest$sample_col
    )
    samp <- DBI::dbGetQuery(con, sql)
    rownames(samp) <- samp[[assay_manifest$sample_col]]
  }
  if (isTRUE(with_feat)) {
    if (!is.null(x$collapse_plan)) {
      feat <- data.frame(feature_id = feature_ids, path = feature_ids, stringsAsFactors = FALSE)
      rownames(feat) <- feature_ids
    } else {
      features_view <- x$atlas$views[[x$assay_name]]$features
      tbl <- paste0("tmp_feat_", sample(1e8, 1))
      DBI::dbWriteTable(con, tbl, data.frame(feature_id = feature_ids), overwrite = TRUE, temporary = TRUE)
      sql <- sprintf(
        "SELECT * FROM %s f JOIN %s t ON f.%s = t.feature_id",
        features_view, tbl, assay_manifest$feature_col
      )
      feat <- DBI::dbGetQuery(con, sql)
      rownames(feat) <- feat[[assay_manifest$feature_col]]
    }
  }
  new_assay(X, feat = feat, samp = samp, links = list())
}
