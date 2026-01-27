#' Collapse features by taxonomy rank
#'
#' @export
collapse_tax <- function(x, ...) {
  UseMethod("collapse_tax")
}

#' @export
collapse_tax.assay_ctx <- function(x, rank, missing_tokens = NULL, missing_label = "Unclassified", drop_missing_rank = FALSE, ...) {
  obj <- x$obj
  if (inherits(obj, "assay_ref")) {
    obj <- collapse_tax.assay_ref(obj, rank = rank, missing_tokens = missing_tokens, missing_label = missing_label, drop_missing_rank = drop_missing_rank, ...)
  } else {
    obj <- collapse_tax.assay(obj, rank = rank, missing_tokens = missing_tokens, missing_label = missing_label, drop_missing_rank = drop_missing_rank, ...)
  }
  new_assay_ctx(obj, x$margin)
}

normalize_tax_token <- function(x, missing_tokens) {
  if (is.null(missing_tokens)) {
    missing_tokens <- c("", " ", "\t", "unknown", "unclassified", "__", "uncultured", "na", "n/a")
  }
  x0 <- trimws(as.character(x))
  is_missing <- is.na(x0) | tolower(x0) %in% missing_tokens
  x0[is_missing] <- NA_character_
  x0
}

collapse_feature_by <- function(X, group, groups) {
  group_index <- match(group, groups)
  M <- Matrix::sparseMatrix(i = seq_along(group_index), j = group_index, x = 1,
                            dims = c(length(group_index), length(groups)))
  X %*% M
}

#' @export
collapse_tax.assay <- function(x, rank, missing_tokens = NULL, missing_label = "Unclassified", drop_missing_rank = FALSE, ...) {
  if (is.null(x$feat)) {
    stop("feat metadata required for collapse_tax.")
  }
  feat <- x$feat
  if (!rank %in% colnames(feat)) {
    stop("rank column not found in feat.")
  }
  ranks <- colnames(feat)
  tax <- feat[, ranks, drop = FALSE]
  tax <- as.data.frame(lapply(tax, normalize_tax_token, missing_tokens = missing_tokens), stringsAsFactors = FALSE)
  if (drop_missing_rank) {
    keep <- !is.na(tax[[rank]])
    x <- prune.assay(x, margin = "feature", predicate = function(df) df$feature_id %in% rownames(feat)[keep])
    feat <- x$feat
    tax <- tax[keep, , drop = FALSE]
  }
  idx <- seq_len(match(rank, ranks))
  for (col in idx) {
    tax[[col]][is.na(tax[[col]])] <- missing_label
  }
  path <- apply(tax[, idx, drop = FALSE], 1, function(z) paste(z, collapse = "; "))
  groups <- unique(path)
  X_new <- collapse_feature_by(x$X, path, groups)
  feat_new <- data.frame(path = groups, stringsAsFactors = FALSE)
  rownames(feat_new) <- groups
  new_assay(X_new, feat = feat_new, samp = x$samp, links = x$links)
}

sql_tax_expr <- function(rank_cols, rank, missing_label, missing_tokens, alias = "f") {
  if (is.null(missing_tokens)) {
    missing_tokens <- c("", " ", "\t", "unknown", "unclassified", "__", "uncultured", "na", "n/a")
  }
  tokens <- paste(sprintf("'%s'", gsub("'", "''", tolower(missing_tokens))), collapse = ", ")
  cols <- rank_cols[seq_len(match(rank, rank_cols))]
  exprs <- lapply(cols, function(col) {
    sprintf("CASE WHEN %s.%s IS NULL OR trim(%s.%s) = '' OR lower(%s.%s) IN (%s) THEN '%s' ELSE %s.%s END",
            alias, col, alias, col, alias, col, tokens, gsub("'", "''", missing_label), alias, col)
  })
  paste(exprs, collapse = " || '; ' || ")
}

sql_rank_filter <- function(rank, missing_tokens, alias = "f") {
  if (is.null(missing_tokens)) {
    missing_tokens <- c("", " ", "\t", "unknown", "unclassified", "__", "uncultured", "na", "n/a")
  }
  tokens <- paste(sprintf("'%s'", gsub("'", "''", tolower(missing_tokens))), collapse = ", ")
  sprintf("%s.%s IS NOT NULL AND trim(%s.%s) <> '' AND lower(%s.%s) NOT IN (%s)",
          alias, rank, alias, rank, alias, rank, tokens)
}

#' @export
collapse_tax.assay_ref <- function(x, rank, missing_tokens = NULL, missing_label = "Unclassified", drop_missing_rank = FALSE, ...) {
  assay <- x$atlas$manifest$assays[[x$assay_name]]
  rank_cols <- assay$taxonomy_cols
  if (is.null(rank_cols) || !rank %in% rank_cols) {
    stop("rank not found in taxonomy_cols.")
  }
  features_view <- x$atlas$views[[x$assay_name]]$features
  group_expr <- sql_tax_expr(rank_cols, rank, missing_label, missing_tokens, alias = "f")
  rank_filter <- if (isTRUE(drop_missing_rank)) sql_rank_filter(rank, missing_tokens, alias = "f") else NULL
  x$collapse_plan <- list(
    group_expr = group_expr,
    features_view = features_view,
    feature_col = assay$feature_col,
    rank_filter = rank_filter
  )
  x
}

#' Collapse by explicit group mapping
#'
#' @export
collapse_by <- function(x, ...) {
  UseMethod("collapse_by")
}

#' @export
collapse_by.assay_ctx <- function(x, group, ...) {
  obj <- x$obj
  if (inherits(obj, "assay_ref")) {
    obj <- collapse_by.assay_ref(obj, group = group, ...)
  } else {
    obj <- collapse_by.assay(obj, group = group, ...)
  }
  new_assay_ctx(obj, x$margin)
}

#' @export
collapse_by.assay <- function(x, group, ...) {
  if (length(group) != ncol(x$X)) {
    stop("group must be length of features.")
  }
  groups <- unique(group)
  X_new <- collapse_feature_by(x$X, group, groups)
  feat_new <- data.frame(group = groups, stringsAsFactors = FALSE)
  rownames(feat_new) <- groups
  new_assay(X_new, feat = feat_new, samp = x$samp, links = x$links)
}

#' @export
collapse_by.assay_ref <- function(x, group, ...) {
  if (is.data.frame(group)) {
    if (!all(c("feature_id", "group_id") %in% colnames(group))) {
      stop("group data.frame must contain feature_id and group_id.")
    }
    con <- x$atlas$con
    tbl <- paste0("tmp_group_", sample(1e8, 1))
    DBI::dbWriteTable(con, tbl, group[, c("feature_id", "group_id")], overwrite = TRUE, temporary = TRUE)
    x$collapse_plan <- list(
      group_expr = "g.group_id",
      features_view = tbl,
      feature_col = "feature_id",
      join_alias = "g"
    )
    return(x)
  }
  stop("collapse_by for assay_ref requires data.frame mapping.")
}
