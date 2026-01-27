#' Summarise samples or features
#'
#' @export
summarise <- function(x, ...) {
  UseMethod("summarise")
}

#' @export
summarise.assay_ctx <- function(x, ...) {
  obj <- x$obj
  if (inherits(obj, "assay_ref")) {
    return(summarise.assay_ref(obj, margin = x$margin, ...))
  }
  summarise.assay(obj, margin = x$margin, ...)
}

#' @export
summarise.assay <- function(x, margin = c("sample", "feature"), ...) {
  margin <- match.arg(margin)
  X <- x$X
  if (margin == "sample") {
    totals <- Matrix::rowSums(X)
    richness <- Matrix::rowSums(X > 0)
    data.frame(sample_id = rownames(X), total_abundance = totals, richness = richness, stringsAsFactors = FALSE)
  } else {
    totals <- Matrix::colSums(X)
    prevalence <- Matrix::colSums(X > 0)
    data.frame(feature_id = colnames(X), total_abundance = totals, prevalence_samples = prevalence, stringsAsFactors = FALSE)
  }
}

#' @export
summarise.assay_ref <- function(x, margin = c("sample", "feature"), ...) {
  margin <- match.arg(margin)
  con <- x$atlas$con
  counts_sql <- ref_counts_sql(x, include_transform = FALSE)
  if (margin == "sample") {
    sql <- sprintf(
      "SELECT sample_id, SUM(x) AS total_abundance, COUNT(DISTINCT feature_id) AS richness FROM (%s) c GROUP BY sample_id",
      counts_sql
    )
    return(DBI::dbGetQuery(con, sql))
  }
  sql <- sprintf(
    "SELECT feature_id, SUM(x) AS total_abundance, COUNT(DISTINCT sample_id) AS prevalence_samples FROM (%s) c GROUP BY feature_id",
    counts_sql
  )
  DBI::dbGetQuery(con, sql)
}
