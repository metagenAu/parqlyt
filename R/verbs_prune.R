#' Prune samples or features
#'
#' @export
prune <- function(x, ...) {
  UseMethod("prune")
}

#' @export
prune.assay_ctx <- function(x, min_total = 0, min_prev = 0, predicate = NULL, ...) {
  obj <- x$obj
  if (inherits(obj, "assay_ref")) {
    obj <- prune.assay_ref(obj, min_total = min_total, min_prev = min_prev, predicate = predicate, margin = x$margin, ...)
  } else {
    obj <- prune.assay(obj, min_total = min_total, min_prev = min_prev, predicate = predicate, margin = x$margin, ...)
  }
  new_assay_ctx(obj, x$margin)
}

#' @export
prune.assay <- function(x, min_total = 0, min_prev = 0, predicate = NULL, margin = c("sample", "feature"), ...) {
  margin <- match.arg(margin)
  X <- x$X
  if (margin == "sample") {
    totals <- Matrix::rowSums(X)
    prev <- Matrix::rowSums(X > 0) / ncol(X)
    stats <- data.frame(sample_id = rownames(X), total_abundance = totals, prevalence_features = prev)
    keep <- totals >= min_total & prev >= min_prev
    if (!is.null(predicate)) {
      keep <- keep & as.logical(predicate(stats))
    }
    X <- X[keep, , drop = FALSE]
    samp <- if (!is.null(x$samp)) x$samp[keep, , drop = FALSE] else NULL
    return(new_assay(X, feat = x$feat, samp = samp, links = x$links))
  }
  totals <- Matrix::colSums(X)
  prev <- Matrix::colSums(X > 0) / nrow(X)
  stats <- data.frame(feature_id = colnames(X), total_abundance = totals, prevalence_samples = prev)
  keep <- totals >= min_total & prev >= min_prev
  if (!is.null(predicate)) {
    keep <- keep & as.logical(predicate(stats))
  }
  X <- X[, keep, drop = FALSE]
  feat <- if (!is.null(x$feat)) x$feat[keep, , drop = FALSE] else NULL
  new_assay(X, feat = feat, samp = x$samp, links = x$links)
}

#' @export
prune.assay_ref <- function(x, min_total = 0, min_prev = 0, predicate = NULL, margin = c("sample", "feature"), ...) {
  margin <- match.arg(margin)
  con <- x$atlas$con
  assay_manifest <- x$atlas$manifest$assays[[x$assay_name]]
  counts_sql <- ref_counts_sql(x, include_transform = FALSE)
  use_stats <- is.null(x$sample_filter) && is.null(x$feature_filter) && !is.null(assay_manifest$stats)
  if (margin == "sample") {
    if (isTRUE(use_stats) && !is.null(assay_manifest$stats$sample_stats_uri)) {
      stats_sql <- sprintf("SELECT * FROM read_parquet('%s')", assay_manifest$stats$sample_stats_uri)
      df <- DBI::dbGetQuery(con, stats_sql)
      if (!all(c("sample_id", "total_abundance", "richness") %in% colnames(df))) {
        stop("sample_stats_uri must provide sample_id, total_abundance, and richness.")
      }
      if (!("n_features" %in% colnames(df)) && min_prev > 0) {
        use_stats <- FALSE
      } else {
        df$prevalence_features <- if ("n_features" %in% colnames(df)) df$richness / df$n_features else 0
      }
    }
    if (isTRUE(use_stats) && exists("df")) {
      keep <- df$total_abundance >= min_total & df$prevalence_features >= min_prev
      if (!is.null(predicate)) {
        keep <- keep & as.logical(predicate(df))
      }
      return(ref_apply_keep(x, df$sample_id[keep], "sample"))
    }
    sql <- sprintf(
      "WITH base AS (%s), totals AS (SELECT COUNT(DISTINCT feature_id) AS n_features FROM base) SELECT c.sample_id, SUM(c.x) AS total_abundance, COUNT(DISTINCT c.feature_id) * 1.0 / NULLIF(t.n_features, 0) AS prevalence_features FROM base c CROSS JOIN totals t GROUP BY c.sample_id, t.n_features",
      counts_sql
    )
    df <- DBI::dbGetQuery(con, sql)
    keep <- df$total_abundance >= min_total & df$prevalence_features >= min_prev
    if (!is.null(predicate)) {
      keep <- keep & as.logical(predicate(df))
    }
    return(ref_apply_keep(x, df$sample_id[keep], "sample"))
  }
  if (isTRUE(use_stats) && !is.null(assay_manifest$stats$feature_stats_uri)) {
    stats_sql <- sprintf("SELECT * FROM read_parquet('%s')", assay_manifest$stats$feature_stats_uri)
    df <- DBI::dbGetQuery(con, stats_sql)
    if (!all(c("feature_id", "total_abundance", "prevalence_samples") %in% colnames(df))) {
      stop("feature_stats_uri must provide feature_id, total_abundance, and prevalence_samples.")
    }
    keep <- df$total_abundance >= min_total & df$prevalence_samples >= min_prev
    if (!is.null(predicate)) {
      keep <- keep & as.logical(predicate(df))
    }
    return(ref_apply_keep(x, df$feature_id[keep], "feature"))
  }
  sql <- sprintf(
    "WITH base AS (%s), totals AS (SELECT COUNT(DISTINCT sample_id) AS n_samples FROM base) SELECT c.feature_id, SUM(c.x) AS total_abundance, COUNT(DISTINCT c.sample_id) * 1.0 / NULLIF(t.n_samples, 0) AS prevalence_samples FROM base c CROSS JOIN totals t GROUP BY c.feature_id, t.n_samples",
    counts_sql
  )
  df <- DBI::dbGetQuery(con, sql)
  keep <- df$total_abundance >= min_total & df$prevalence_samples >= min_prev
  if (!is.null(predicate)) {
    keep <- keep & as.logical(predicate(df))
  }
  ref_apply_keep(x, df$feature_id[keep], "feature")
}
