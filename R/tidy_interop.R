#' Convert assay to long table
#'
#' @export
as_long <- function(x, max_nnz = 1e6) {
  if (inherits(x, "assay_ref")) {
    stop("as_long for assay_ref requires collect() or use collect() first.")
  }
  if (!inherits(x, "assay")) {
    stop("Unsupported object.")
  }
  nnz <- length(x$X@x)
  if (nnz > max_nnz) {
    stop("Too many nonzeros for as_long; use collect() with limits or raise max_nnz.")
  }
  sm <- Matrix::summary(x$X)
  data.frame(
    sample_id = rownames(x$X)[sm$i],
    feature_id = colnames(x$X)[sm$j],
    x = sm$x,
    stringsAsFactors = FALSE
  )
}

#' Sample metadata as table
#'
#' @export
samp_tbl <- function(x) {
  if (inherits(x, "assay")) {
    samp <- x$samp
    if (is.null(samp)) return(NULL)
    samp$sample_id <- rownames(samp)
    return(samp)
  }
  if (inherits(x, "assay_ref")) {
    view <- x$atlas$views$samples
    if (is.null(view)) return(NULL)
    return(DBI::dbGetQuery(x$atlas$con, sprintf("SELECT * FROM %s", view)))
  }
  stop("Unsupported object.")
}

#' Feature metadata as table
#'
#' @export
feat_tbl <- function(x) {
  if (inherits(x, "assay")) {
    feat <- x$feat
    if (is.null(feat)) return(NULL)
    feat$feature_id <- rownames(feat)
    return(feat)
  }
  if (inherits(x, "assay_ref")) {
    view <- x$atlas$views[[x$assay_name]]$features
    return(DBI::dbGetQuery(x$atlas$con, sprintf("SELECT * FROM %s", view)))
  }
  stop("Unsupported object.")
}
