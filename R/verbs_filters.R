#' Filter samples/features by ids
#'
#' @export
keep_samples <- function(x, ids) {
  if (inherits(x, "assay_ref")) {
    return(ref_apply_keep(x, ids, "sample"))
  }
  if (inherits(x, "assay")) {
    ids <- intersect(ids, rownames(x$X))
    X <- x$X[ids, , drop = FALSE]
    samp <- if (!is.null(x$samp)) x$samp[ids, , drop = FALSE] else NULL
    return(new_assay(X, feat = x$feat, samp = samp, links = x$links))
  }
  stop("Unsupported object.")
}

#' @export
keep_features <- function(x, ids) {
  if (inherits(x, "assay_ref")) {
    return(ref_apply_keep(x, ids, "feature"))
  }
  if (inherits(x, "assay")) {
    ids <- intersect(ids, colnames(x$X))
    X <- x$X[, ids, drop = FALSE]
    feat <- if (!is.null(x$feat)) x$feat[ids, , drop = FALSE] else NULL
    return(new_assay(X, feat = feat, samp = x$samp, links = x$links))
  }
  stop("Unsupported object.")
}

#' @export
drop_samples <- function(x, ids) {
  if (inherits(x, "assay_ref")) {
    con <- x$atlas$con
    tbl <- paste0("tmp_drop_sample_", sample(1e8, 1))
    DBI::dbWriteTable(con, tbl, data.frame(id = ids), overwrite = TRUE, temporary = TRUE)
    where <- sprintf("sample_id NOT IN (SELECT id FROM %s)", tbl)
    if (!is.null(x$sample_filter)) {
      where <- paste0("(", x$sample_filter$where, ") AND (", where, ")")
    }
    x$sample_filter <- list(table = tbl, where = where)
    return(x)
  }
  if (inherits(x, "assay")) {
    keep <- !rownames(x$X) %in% ids
    X <- x$X[keep, , drop = FALSE]
    samp <- if (!is.null(x$samp)) x$samp[keep, , drop = FALSE] else NULL
    return(new_assay(X, feat = x$feat, samp = samp, links = x$links))
  }
  stop("Unsupported object.")
}

#' @export
drop_features <- function(x, ids) {
  if (inherits(x, "assay_ref")) {
    con <- x$atlas$con
    tbl <- paste0("tmp_drop_feature_", sample(1e8, 1))
    DBI::dbWriteTable(con, tbl, data.frame(id = ids), overwrite = TRUE, temporary = TRUE)
    where <- sprintf("feature_id NOT IN (SELECT id FROM %s)", tbl)
    if (!is.null(x$feature_filter)) {
      where <- paste0("(", x$feature_filter$where, ") AND (", where, ")")
    }
    x$feature_filter <- list(table = tbl, where = where)
    return(x)
  }
  if (inherits(x, "assay")) {
    keep <- !colnames(x$X) %in% ids
    X <- x$X[, keep, drop = FALSE]
    feat <- if (!is.null(x$feat)) x$feat[keep, , drop = FALSE] else NULL
    return(new_assay(X, feat = feat, samp = x$samp, links = x$links))
  }
  stop("Unsupported object.")
}

#' @export
select_samples_where <- function(x, expr) {
  if (inherits(x, "assay_ref")) {
    con <- x$atlas$con
    view <- x$atlas$views$samples
    if (is.null(view)) {
      stop("No samples view in atlas.")
    }
    sql <- sprintf("SELECT %s AS sample_id FROM %s WHERE %s", x$atlas$manifest$assays[[x$assay_name]]$sample_col, view, expr)
    ids <- DBI::dbGetQuery(con, sql)$sample_id
    return(ref_apply_keep(x, ids, "sample"))
  }
  if (inherits(x, "assay")) {
    if (is.null(x$samp)) {
      stop("No samp metadata.")
    }
    keep <- with(x$samp, eval(parse(text = expr)))
    ids <- rownames(x$samp)[keep]
    return(keep_samples(x, ids))
  }
  stop("Unsupported object.")
}

#' @export
select_features_where <- function(x, expr) {
  if (inherits(x, "assay_ref")) {
    con <- x$atlas$con
    view <- x$atlas$views[[x$assay_name]]$features
    sql <- sprintf("SELECT %s AS feature_id FROM %s WHERE %s", x$atlas$manifest$assays[[x$assay_name]]$feature_col, view, expr)
    ids <- DBI::dbGetQuery(con, sql)$feature_id
    return(ref_apply_keep(x, ids, "feature"))
  }
  if (inherits(x, "assay")) {
    if (is.null(x$feat)) {
      stop("No feat metadata.")
    }
    keep <- with(x$feat, eval(parse(text = expr)))
    ids <- rownames(x$feat)[keep]
    return(keep_features(x, ids))
  }
  stop("Unsupported object.")
}
