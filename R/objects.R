#' Construct a local assay object
#'
#' @param X sparse dgCMatrix.
#' @param feat feature metadata with rownames as feature_id.
#' @param samp sample metadata with rownames as sample_id.
#' @param links named list for extra data.
#' @export
new_assay <- function(X, feat = NULL, samp = NULL, links = list()) {
  if (!inherits(X, "dgCMatrix")) {
    stop("X must be a Matrix::dgCMatrix.")
  }
  if (is.null(rownames(X)) || is.null(colnames(X))) {
    stop("X must have rownames (sample_id) and colnames (feature_id).")
  }
  ids_sample <- rownames(X)
  ids_feature <- colnames(X)
  if (!is.null(samp)) {
    if (is.null(rownames(samp))) {
      stop("samp must have rownames as sample_id.")
    }
    keep_samples <- intersect(ids_sample, rownames(samp))
    X <- X[keep_samples, , drop = FALSE]
    samp <- samp[keep_samples, , drop = FALSE]
    ids_sample <- rownames(X)
  }
  if (!is.null(feat)) {
    if (is.null(rownames(feat))) {
      stop("feat must have rownames as feature_id.")
    }
    keep_features <- intersect(ids_feature, rownames(feat))
    X <- X[, keep_features, drop = FALSE]
    feat <- feat[keep_features, , drop = FALSE]
  }
  structure(
    list(X = X, feat = feat, samp = samp, links = links),
    class = "assay"
  )
}

new_assay_ref <- function(atlas, assay_name) {
  structure(
    list(
      atlas = atlas,
      assay_name = assay_name,
      active_margin = NULL,
      sample_filter = NULL,
      feature_filter = NULL,
      transform_plan = list(),
      collapse_plan = NULL
    ),
    class = "assay_ref"
  )
}

new_assay_ctx <- function(obj, margin) {
  structure(list(obj = obj, margin = margin), class = "assay_ctx")
}

#' @export
by_sample <- function(x) {
  new_assay_ctx(x, "sample")
}

#' @export
by_feature <- function(x) {
  new_assay_ctx(x, "feature")
}

#' @export
end <- function(x) {
  if (inherits(x, "assay_ctx")) {
    return(x$obj)
  }
  x
}
