#' Pretty printing for assay objects
#'
#' @export
print.assay <- function(x, ...) {
  dims <- dim(x$X)
  nnz <- length(x$X@x)
  cat("<assay>\n")
  cat(sprintf("  Samples:  %s\n", format(dims[1], big.mark = ",")))
  cat(sprintf("  Features: %s\n", format(dims[2], big.mark = ",")))
  cat(sprintf("  Nonzeros: %s\n", format(nnz, big.mark = ",")))
  if (!is.null(x$samp)) {
    cat(sprintf("  Sample metadata: %s columns\n", ncol(x$samp)))
  } else {
    cat("  Sample metadata: <none>\n")
  }
  if (!is.null(x$feat)) {
    cat(sprintf("  Feature metadata: %s columns\n", ncol(x$feat)))
  } else {
    cat("  Feature metadata: <none>\n")
  }
  if (length(x$links) > 0) {
    cat(sprintf("  Links: %s\n", paste(names(x$links), collapse = ", ")))
  } else {
    cat("  Links: <none>\n")
  }
  invisible(x)
}

format_transform_plan <- function(plan) {
  if (length(plan) == 0) {
    return("<none>")
  }
  entries <- vapply(plan, function(step) {
    if (is.null(step$type)) {
      return("<unknown>")
    }
    if (identical(step$type, "expr") && !is.null(step$expr)) {
      return(sprintf("expr:%s", step$expr))
    }
    step$type
  }, character(1))
  paste(entries, collapse = ", ")
}

format_collapse_plan <- function(plan) {
  if (is.null(plan)) {
    return("<none>")
  }
  if (!is.null(plan$type) && !is.null(plan$rank)) {
    return(sprintf("%s (%s)", plan$type, plan$rank))
  }
  if (!is.null(plan$type) && !is.null(plan$group)) {
    return(sprintf("%s (%s)", plan$type, plan$group))
  }
  if (!is.null(plan$type)) {
    return(plan$type)
  }
  "<unknown>"
}

describe_assay_obj <- function(obj) {
  if (inherits(obj, "assay_ref")) {
    return(sprintf("assay_ref (%s)", obj$assay_name))
  }
  if (inherits(obj, "assay")) {
    return("assay (collected)")
  }
  class(obj)[1]
}

#' @export
print.assay_ref <- function(x, ...) {
  cat("<assay_ref>\n")
  cat(sprintf("  Assay: %s\n", x$assay_name))
  if (is.null(x$active_margin)) {
    cat("  Active margin: <none>\n")
  } else {
    cat(sprintf("  Active margin: %s\n", x$active_margin))
  }
  cat(sprintf("  Sample filter: %s\n", if (is.null(x$sample_filter)) "<none>" else "set"))
  cat(sprintf("  Feature filter: %s\n", if (is.null(x$feature_filter)) "<none>" else "set"))
  cat(sprintf("  Transforms: %s\n", format_transform_plan(x$transform_plan)))
  cat(sprintf("  Collapse: %s\n", format_collapse_plan(x$collapse_plan)))
  invisible(x)
}

#' @export
print.assay_ctx <- function(x, ...) {
  cat("<assay_ctx>\n")
  cat(sprintf("  Margin: %s\n", x$margin))
  cat(sprintf("  Object: %s\n", describe_assay_obj(x$obj)))
  invisible(x)
}
