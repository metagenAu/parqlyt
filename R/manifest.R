#' Read atlas manifest JSON
#'
#' @param con DuckDB connection.
#' @param manifest_uri R2 URI for manifest JSON.
#' @return Parsed list.
read_manifest <- function(con, manifest_uri) {
  sql <- sprintf("SELECT to_json(struct_pack(*)) AS json FROM read_json_auto('%s')", manifest_uri)
  df <- DBI::dbGetQuery(con, sql)
  if (nrow(df) != 1) {
    stop("Manifest JSON must contain a single object.")
  }
  jsonlite::fromJSON(df$json[[1]], simplifyVector = TRUE)
}

normalize_manifest <- function(manifest) {
  if (is.null(manifest$assays)) {
    stop("Manifest missing assays.")
  }
  manifest
}
