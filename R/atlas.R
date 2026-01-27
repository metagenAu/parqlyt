#' Open an atlas on R2
#'
#' @param bucket R2 bucket.
#' @param prefix Base prefix for atlas.
#' @param manifest_uri Optional explicit manifest URI.
#' @param con Optional DuckDB connection; created if NULL.
#' @param manifest Optional manifest list to use directly.
#' @return atlas object.
#' @export
atlas_open <- function(bucket = Sys.getenv("ATLAS_R2_BUCKET"),
                       prefix = Sys.getenv("ATLAS_R2_PREFIX", "atlas/v1"),
                       manifest_uri = NULL,
                       con = NULL,
                       manifest = NULL) {
  if (is.null(con)) {
    con <- connect_duckdb()
  }
  ensure_httpfs(con)
  key_id <- Sys.getenv("ATLAS_R2_KEY_ID")
  secret <- Sys.getenv("ATLAS_R2_SECRET")
  account_id <- Sys.getenv("ATLAS_R2_ACCOUNT_ID")
  if (key_id == "" || secret == "" || account_id == "") {
    stop("Missing ATLAS_R2_* environment variables.")
  }
  set_r2_secret(con, key_id = key_id, secret = secret, account_id = account_id)
  DBI::dbExecute(con, "SET hive_partitioning=1;")
  if (is.null(manifest)) {
    if (is.null(manifest_uri) || manifest_uri == "") {
      if (bucket == "") {
        stop("bucket required when manifest_uri is not supplied.")
      }
      manifest_uri <- sprintf("r2://%s/%s/manifest/atlas.json", bucket, prefix)
    }
    manifest <- normalize_manifest(read_manifest(con, manifest_uri))
  } else {
    manifest <- normalize_manifest(manifest)
  }
  views <- create_atlas_views(con, manifest)
  structure(
    list(con = con, manifest = manifest, views = views),
    class = "atlas"
  )
}

create_atlas_views <- function(con, manifest) {
  views <- list()
  if (!is.null(manifest$samples_uri)) {
    DBI::dbExecute(con, sprintf(
      "CREATE OR REPLACE VIEW v_samples AS SELECT * FROM read_parquet('%s');",
      manifest$samples_uri
    ))
    views$samples <- "v_samples"
  }
  assays <- names(manifest$assays)
  for (assay_name in assays) {
    assay <- manifest$assays[[assay_name]]
    feat_view <- sprintf("v_features_%s", assay_name)
    cnt_view <- sprintf("v_counts_%s", assay_name)
    DBI::dbExecute(con, sprintf(
      "CREATE OR REPLACE VIEW %s AS SELECT * FROM read_parquet('%s');",
      feat_view, assay$features_uri
    ))
    DBI::dbExecute(con, sprintf(
      "CREATE OR REPLACE VIEW %s AS SELECT * FROM read_parquet('%s');",
      cnt_view, assay$counts_uri
    ))
    views[[assay_name]] <- list(features = feat_view, counts = cnt_view)
    if (!is.null(assay$aux)) {
      for (aux_name in names(assay$aux)) {
        aux_view <- sprintf("v_aux_%s_%s", assay_name, aux_name)
        DBI::dbExecute(con, sprintf(
          "CREATE OR REPLACE VIEW %s AS SELECT * FROM read_parquet('%s');",
          aux_view, assay$aux[[aux_name]]
        ))
        views[[assay_name]][[paste0("aux_", aux_name)]] <- aux_view
      }
    }
  }
  views
}

#' Access an assay from an atlas
#'
#' @param atlas atlas object.
#' @param assay_name assay name.
#' @return assay_ref.
#' @export
assay <- function(atlas, assay_name) {
  if (!inherits(atlas, "atlas")) {
    stop("atlas must be an atlas object.")
  }
  if (!assay_name %in% names(atlas$manifest$assays)) {
    stop("Unknown assay.")
  }
  new_assay_ref(atlas, assay_name)
}
