make_toy_assay <- function(include_zero_sample = FALSE) {
  X <- Matrix::sparseMatrix(
    i = c(1, 1, 2, 3),
    j = c(1, 3, 2, 3),
    x = c(10, 5, 2, 1),
    dims = c(3, 3),
    dimnames = list(c("s1", "s2", "s3"), c("f1", "f2", "f3"))
  )
  if (isTRUE(include_zero_sample)) {
    X <- rbind(X, Matrix::sparseMatrix(i = integer(0), j = integer(0), x = numeric(0),
                                       dims = c(1, 3),
                                       dimnames = list("s4", colnames(X))))
  }
  feat <- data.frame(
    Kingdom = c("Bacteria", "Bacteria", "Bacteria"),
    Genus = c("A", "B", NA),
    stringsAsFactors = FALSE
  )
  rownames(feat) <- c("f1", "f2", "f3")
  samp <- data.frame(project_id = c("P1", "P1", "P2"), stringsAsFactors = FALSE)
  rownames(samp) <- c("s1", "s2", "s3")
  if (isTRUE(include_zero_sample)) {
    samp <- rbind(samp, data.frame(project_id = "P3", row.names = "s4"))
  }
  new_assay(X, feat = feat, samp = samp)
}

write_parquet <- function(con, name, df, path) {
  DBI::dbWriteTable(con, name, df, overwrite = TRUE)
  DBI::dbExecute(con, sprintf("COPY %s TO '%s' (FORMAT PARQUET)", name, path))
}

make_manifest <- function(counts_path, features_path, samples_path,
                          sample_col = "sample_id",
                          feature_col = "feature_id",
                          x_col = "x",
                          partition_cols = NULL,
                          stats = NULL,
                          taxonomy_cols = c("Genus")) {
  list(
    version = "v1",
    bucket = "local",
    base_prefix = "atlas/v1",
    samples_uri = samples_path,
    assays = list(
      `16S` = list(
        counts_uri = counts_path,
        features_uri = features_path,
        partition_cols = partition_cols,
        x_col = x_col,
        sample_col = sample_col,
        feature_col = feature_col,
        taxonomy_cols = taxonomy_cols,
        stats = stats
      )
    )
  )
}

make_duckdb_atlas <- function(include_stats = FALSE, include_partition = FALSE) {
  con <- connect_duckdb()
  tmp <- tempdir()
  counts <- data.frame(
    sample_id = c("s1", "s1", "s2", "s3"),
    feature_id = c("f1", "f2", "f1", "f3"),
    x = c(1, 2, 3, 4),
    project_id = c("P1", "P1", "P1", "P2"),
    sample_bucket = sample_bucket(c("s1", "s1", "s2", "s3")),
    stringsAsFactors = FALSE
  )
  features <- data.frame(feature_id = c("f1", "f2", "f3"), Genus = c("A", "B", NA), stringsAsFactors = FALSE)
  samples <- data.frame(sample_id = c("s1", "s2", "s3"), project_id = c("P1", "P1", "P2"), stringsAsFactors = FALSE)

  counts_path <- file.path(tmp, "counts.parquet")
  features_path <- file.path(tmp, "features.parquet")
  samples_path <- file.path(tmp, "samples.parquet")
  write_parquet(con, "counts_tmp", counts, counts_path)
  write_parquet(con, "features_tmp", features, features_path)
  write_parquet(con, "samples_tmp", samples, samples_path)

  stats <- NULL
  if (isTRUE(include_stats)) {
    sample_stats <- data.frame(
      sample_id = c("s1", "s2", "s3"),
      total_abundance = c(3, 3, 4),
      richness = c(2, 1, 1),
      n_features = 3,
      stringsAsFactors = FALSE
    )
    feature_stats <- data.frame(
      feature_id = c("f1", "f2", "f3"),
      total_abundance = c(4, 2, 4),
      prevalence_samples = c(2, 1, 1),
      stringsAsFactors = FALSE
    )
    sample_stats_path <- file.path(tmp, "sample_stats.parquet")
    feature_stats_path <- file.path(tmp, "feature_stats.parquet")
    write_parquet(con, "sample_stats_tmp", sample_stats, sample_stats_path)
    write_parquet(con, "feature_stats_tmp", feature_stats, feature_stats_path)
    stats <- list(sample_stats_uri = sample_stats_path, feature_stats_uri = feature_stats_path)
  }

  partition_cols <- if (isTRUE(include_partition)) c("assay", "project_id", "sample_bucket") else NULL
  manifest <- make_manifest(
    counts_path = counts_path,
    features_path = features_path,
    samples_path = samples_path,
    partition_cols = partition_cols,
    stats = stats
  )
  manifest_path <- file.path(tmp, "atlas.json")
  jsonlite::write_json(manifest, manifest_path, auto_unbox = TRUE)

  views <- create_atlas_views(con, manifest)
  atlas <- structure(list(con = con, manifest = manifest, views = views), class = "atlas")
  list(atlas = atlas, manifest = manifest, manifest_path = manifest_path)
}
