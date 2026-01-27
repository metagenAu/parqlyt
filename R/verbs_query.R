#' Query samples or features by metadata
#'
#' @param x assay, assay_ref, or assay_ctx.
#' @param query character vector of patterns to match.
#' @param columns character vector of columns to search; defaults to all columns.
#' @param case_sensitive logical; if FALSE use case-insensitive matching.
#' @param exact logical; if TRUE require exact matches instead of substring matches.
#' @param ... extra arguments (unused).
#' @export
query <- function(x, ...) {
  UseMethod("query")
}

#' @export
query.assay_ctx <- function(x, query, columns = NULL, case_sensitive = FALSE, exact = FALSE, ...) {
  obj <- x$obj
  if (inherits(obj, "assay_ref")) {
    return(query.assay_ref(obj, query = query, columns = columns, case_sensitive = case_sensitive, exact = exact, margin = x$margin, ...))
  }
  query.assay(obj, query = query, columns = columns, case_sensitive = case_sensitive, exact = exact, margin = x$margin, ...)
}

query_available_cols <- function(con, view) {
  colnames(DBI::dbGetQuery(con, sprintf("SELECT * FROM %s LIMIT 0", view)))
}

query_validate_columns <- function(columns, available) {
  if (length(columns) == 0) {
    stop("No columns available for query.")
  }
  missing <- setdiff(columns, available)
  if (length(missing) > 0) {
    stop("Unknown columns: ", paste(missing, collapse = ", "))
  }
  columns
}

query_build_sql_conditions <- function(con, columns, patterns, case_sensitive, exact) {
  op <- if (isTRUE(case_sensitive)) {
    if (isTRUE(exact)) "=" else "LIKE"
  } else {
    "ILIKE"
  }
  cols_sql <- as.character(DBI::dbQuoteIdentifier(con, columns))
  conds <- list()
  for (col in cols_sql) {
    col_expr <- sprintf("CAST(%s AS VARCHAR)", col)
    for (pattern in patterns) {
      if (isTRUE(exact)) {
        pattern_sql <- DBI::dbQuoteString(con, pattern)
      } else {
        pattern_sql <- DBI::dbQuoteString(con, paste0("%", pattern, "%"))
      }
      conds <- c(conds, sprintf("%s %s %s", col_expr, op, pattern_sql))
    }
  }
  paste(conds, collapse = " OR ")
}

#' @export
query.assay_ref <- function(x, query, columns = NULL, case_sensitive = FALSE, exact = FALSE,
                            margin = c("sample", "feature"), ...) {
  margin <- match.arg(margin)
  con <- x$atlas$con
  assay_manifest <- x$atlas$manifest$assays[[x$assay_name]]
  if (margin == "sample") {
    view <- x$atlas$views$samples
    if (is.null(view)) {
      stop("No samples view in atlas.")
    }
    id_col <- assay_manifest$sample_col
  } else {
    view <- x$atlas$views[[x$assay_name]]$features
    if (is.null(view)) {
      stop("No features view in atlas.")
    }
    id_col <- assay_manifest$feature_col
  }
  if (is.null(columns)) {
    columns <- query_available_cols(con, view)
  }
  columns <- query_validate_columns(columns, query_available_cols(con, view))
  patterns <- as.character(query)
  if (length(patterns) == 0) {
    stop("query must contain at least one pattern.")
  }
  where <- query_build_sql_conditions(con, columns, patterns, case_sensitive, exact)
  id_sql <- as.character(DBI::dbQuoteIdentifier(con, id_col))
  sql <- sprintf("SELECT DISTINCT %s AS id FROM %s WHERE %s", id_sql, view, where)
  DBI::dbGetQuery(con, sql)$id
}

query_match_vector <- function(values, pattern, case_sensitive, exact) {
  values <- as.character(values)
  if (isTRUE(exact)) {
    if (isTRUE(case_sensitive)) {
      match <- values == pattern
    } else {
      match <- tolower(values) == tolower(pattern)
    }
  } else {
    match <- grepl(pattern, values, ignore.case = !case_sensitive, fixed = TRUE)
  }
  match[is.na(match)] <- FALSE
  match
}

#' @export
query.assay <- function(x, query, columns = NULL, case_sensitive = FALSE, exact = FALSE,
                        margin = c("sample", "feature"), ...) {
  margin <- match.arg(margin)
  if (margin == "sample") {
    if (is.null(x$samp)) {
      stop("No samp metadata.")
    }
    df <- x$samp
  } else {
    if (is.null(x$feat)) {
      stop("No feat metadata.")
    }
    df <- x$feat
  }
  if (is.null(rownames(df))) {
    stop("Metadata must have rownames as ids.")
  }
  if (is.null(columns)) {
    columns <- colnames(df)
  }
  columns <- query_validate_columns(columns, colnames(df))
  patterns <- as.character(query)
  if (length(patterns) == 0) {
    stop("query must contain at least one pattern.")
  }
  keep <- rep(FALSE, nrow(df))
  for (col in columns) {
    values <- df[[col]]
    for (pattern in patterns) {
      keep <- keep | query_match_vector(values, pattern, case_sensitive, exact)
    }
  }
  rownames(df)[keep]
}
