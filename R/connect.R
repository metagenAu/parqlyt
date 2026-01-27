#' Connect to DuckDB
#'
#' @param dbdir DuckDB directory or ":memory:".
#' @return DBI connection.
#' @export
connect_duckdb <- function(dbdir = ":memory:") {
  DBI::dbConnect(duckdb::duckdb(), dbdir = dbdir)
}

#' Ensure httpfs extension is available
#'
#' @param con DuckDB connection.
#' @export
ensure_httpfs <- function(con) {
  DBI::dbExecute(con, "INSTALL httpfs;")
  DBI::dbExecute(con, "LOAD httpfs;")
  invisible(TRUE)
}

#' Set R2 secret for DuckDB
#'
#' @param con DuckDB connection.
#' @param key_id R2 access key ID.
#' @param secret R2 secret.
#' @param account_id R2 account id.
#' @param persistent If TRUE, use DuckDB persistent secret storage.
#' @export
set_r2_secret <- function(con, key_id, secret, account_id, persistent = FALSE) {
  scope <- if (isTRUE(persistent)) "PERSISTENT" else ""
  sql <- sprintf(
    "CREATE SECRET %s (TYPE r2, KEY_ID '%s', SECRET '%s', ACCOUNT_ID '%s');",
    scope,
    gsub("'", "''", key_id),
    gsub("'", "''", secret),
    gsub("'", "''", account_id)
  )
  DBI::dbExecute(con, sql)
  invisible(TRUE)
}
