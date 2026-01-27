md5_hex <- function(x) {
  if (!requireNamespace("digest", quietly = TRUE)) {
    stop("Package 'digest' required for md5 hashing. Please install it.")
  }
  vapply(x, function(z) digest::digest(z, algo = "md5", serialize = FALSE), character(1))
}

sample_bucket <- function(sample_ids) {
  substr(md5_hex(sample_ids), 1, 2)
}
