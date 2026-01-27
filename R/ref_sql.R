apply_transform_sql <- function(base, tr) {
  if (tr$type == "expr") {
    return(sprintf("SELECT sample_id, feature_id, %s AS x FROM (%s) t", tr$expr, base))
  }
  if (tr$type == "relab") {
    return(sprintf(
      "SELECT t.sample_id, t.feature_id, t.x / s.sum_x AS x FROM (%s) t JOIN (SELECT sample_id, SUM(x) AS sum_x FROM (%s) z GROUP BY sample_id) s USING(sample_id)",
      base, base
    ))
  }
  stop("Unknown transform type.")
}

ref_counts_sql <- function(ref, include_transform = FALSE) {
  atlas <- ref$atlas
  assay <- atlas$manifest$assays[[ref$assay_name]]
  views <- atlas$views[[ref$assay_name]]
  counts_view <- views$counts
  sample_col <- assay$sample_col
  feature_col <- assay$feature_col
  x_col <- assay$x_col

  select_expr <- sprintf("%s AS sample_id, %s AS feature_id, %s AS x", sample_col, feature_col, x_col)
  base <- sprintf("SELECT %s FROM %s", select_expr, counts_view)
  where_parts <- list()

  if (!is.null(ref$sample_filter)) {
    where_parts <- c(where_parts, ref$sample_filter$where)
  }
  if (!is.null(ref$feature_filter)) {
    where_parts <- c(where_parts, ref$feature_filter$where)
  }
  if (length(where_parts) > 0) {
    base <- paste(base, "WHERE", paste(where_parts, collapse = " AND "))
  }

  if (!is.null(ref$collapse_plan)) {
    plan <- ref$collapse_plan
    alias <- if (!is.null(plan$join_alias)) plan$join_alias else "f"
    join_where <- ""
    if (!is.null(plan$rank_filter)) {
      join_where <- paste("WHERE", plan$rank_filter)
    }
    base <- sprintf(
      "SELECT c.sample_id, %s AS feature_id, SUM(c.x) AS x FROM (%s) c JOIN %s %s ON c.feature_id = %s.%s %s GROUP BY c.sample_id, %s",
      plan$group_expr, base, plan$features_view, alias, alias, plan$feature_col, join_where, plan$group_expr
    )
  }

  if (isTRUE(include_transform) && length(ref$transform_plan) > 0) {
    for (tr in ref$transform_plan) {
      base <- apply_transform_sql(base, tr)
    }
  }
  base
}

ref_apply_keep <- function(ref, ids, margin) {
  if (length(ids) == 0) {
    stop("No ids provided for filter.")
  }
  con <- ref$atlas$con
  tbl <- paste0("tmp_keep_", margin, "_", sample(1e8, 1))
  DF <- data.frame(id = ids, stringsAsFactors = FALSE)
  DBI::dbWriteTable(con, tbl, DF, overwrite = TRUE, temporary = TRUE)
  if (margin == "sample") {
    where <- sprintf("sample_id IN (SELECT id FROM %s)", tbl)
    if (!is.null(ref$sample_filter)) {
      where <- paste0("(", ref$sample_filter$where, ") AND (", where, ")")
    }
    ref$sample_filter <- list(table = tbl, where = where, ids = ids)
    partition_cols <- ref$atlas$manifest$assays[[ref$assay_name]]$partition_cols
    if (!is.null(partition_cols) && "sample_bucket" %in% partition_cols) {
      buckets <- unique(sample_bucket(ids))
      bucket_where <- sprintf("sample_bucket IN (%s)", paste(sprintf("'%s'", buckets), collapse = ", "))
      ref$sample_filter$where <- paste(ref$sample_filter$where, bucket_where, sep = " AND ")
    }
  } else {
    where <- sprintf("feature_id IN (SELECT id FROM %s)", tbl)
    if (!is.null(ref$feature_filter)) {
      where <- paste0("(", ref$feature_filter$where, ") AND (", where, ")")
    }
    ref$feature_filter <- list(table = tbl, where = where, ids = ids)
  }
  ref
}
