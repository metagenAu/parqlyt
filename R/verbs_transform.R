#' Transform assay values
#'
#' @export
transform <- function(x, ...) {
  UseMethod("transform")
}

#' @export
transform.assay_ctx <- function(x, method = c("relab", "log1p", "hellinger"), ...) {
  obj <- x$obj
  if (inherits(obj, "assay_ref")) {
    obj <- transform.assay_ref(obj, method = method, margin = x$margin, ...)
  } else {
    obj <- transform.assay(obj, method = method, margin = x$margin, ...)
  }
  new_assay_ctx(obj, x$margin)
}

#' @export
transform.assay <- function(x, method = c("relab", "log1p", "hellinger"), margin = c("sample", "feature"), ...) {
  method <- match.arg(method)
  margin <- match.arg(margin)
  X <- x$X
  if (method == "log1p") {
    X@x <- log1p(X@x)
    return(new_assay(X, feat = x$feat, samp = x$samp, links = x$links))
  }
  if (method %in% c("relab", "hellinger")) {
    if (margin != "sample") {
      stop("relab/hellinger only supported for by_sample in local mode.")
    }
    rs <- Matrix::rowSums(X)
    if (any(rs == 0)) {
      warning("Zero-sum samples present; leaving zeros unchanged.")
      rs[rs == 0] <- 1
    }
    X <- X / rs
    if (method == "hellinger") {
      X@x <- sqrt(X@x)
    }
    return(new_assay(X, feat = x$feat, samp = x$samp, links = x$links))
  }
  stop("Unknown method.")
}

#' @export
transform.assay_ref <- function(x, method = c("relab", "log1p", "hellinger"), margin = c("sample", "feature"), ...) {
  method <- match.arg(method)
  margin <- match.arg(margin)
  if (method == "log1p") {
    x$transform_plan <- c(x$transform_plan, list(list(type = "expr", expr = "log1p(x)")))
    return(x)
  }
  if (method %in% c("relab", "hellinger")) {
    if (margin != "sample") {
      stop("relab/hellinger only supported for by_sample in lazy mode.")
    }
    x$transform_plan <- c(x$transform_plan, list(list(type = "relab")))
    if (method == "hellinger") {
      x$transform_plan <- c(x$transform_plan, list(list(type = "expr", expr = "sqrt(x)")))
    }
    return(x)
  }
  stop("Unknown method.")
}
