# Migration Guide: MySQL → Partitioned Parquet (staging) → Cloudflare R2

This guide outlines a repeatable pipeline for exporting long-sparse assay data and metadata from MySQL to partitioned Parquet in a local staging directory, then uploading to Cloudflare R2. The goal is to produce a layout compatible with `parqlyt` and DuckDB’s `read_parquet()` with Hive partitioning.

## 1) R packages and helpers

```r
install.packages(c("DBI", "RMariaDB", "arrow", "digest", "jsonlite", "aws.s3"))

library(DBI)
library(RMariaDB)
library(arrow)
library(digest)
library(jsonlite)
library(aws.s3)
```

### 1.1 Compute a 2-hex `sample_bucket`

```r
sample_bucket <- function(sample_id) {
  substr(digest(sample_id, algo = "md5", serialize = FALSE), 1, 2)
}
```

### 1.2 Write one chunk to partitioned Parquet directories

```r
write_partitioned_chunk <- function(df, out_base, partition_cols,
                                    file_prefix = "part",
                                    compression = "zstd") {
  stopifnot(all(partition_cols %in% names(df)))

  # split by partitions in this chunk
  key <- do.call(paste, c(df[partition_cols], sep = "\r"))
  groups <- split(seq_len(nrow(df)), key)

  for (k in names(groups)) {
    idx <- groups[[k]]
    sub <- df[idx, , drop = FALSE]

    # build partition dir: col=value/col=value/...
    vals <- strsplit(k, "\r", fixed = TRUE)[[1]]
    part_dir <- out_base
    for (i in seq_along(partition_cols)) {
      part_dir <- file.path(part_dir, sprintf("%s=%s", partition_cols[i], vals[i]))
    }
    dir.create(part_dir, recursive = TRUE, showWarnings = FALSE)

    # unique file name per write (avoid append complexity)
    f <- file.path(part_dir, sprintf("%s-%s.parquet", file_prefix, format(Sys.time(), "%Y%m%d%H%M%OS6")))
    arrow::write_parquet(sub, sink = f, compression = compression)
  }
}
```

### 1.3 Upload a local directory tree to R2

```r
r2_upload_dir <- function(local_dir, bucket, prefix,
                          account_id, access_key_id, secret_access_key) {
  base_url <- sprintf("https://%s.r2.cloudflarestorage.com", account_id)

  Sys.setenv("AWS_ACCESS_KEY_ID" = access_key_id,
             "AWS_SECRET_ACCESS_KEY" = secret_access_key,
             "AWS_DEFAULT_REGION" = "auto")

  files <- list.files(local_dir, recursive = TRUE, full.names = TRUE)
  files <- files[file.info(files)$isdir == FALSE]

  for (f in files) {
    rel <- sub(paste0("^", normalizePath(local_dir), "/?"), "", normalizePath(f))
    key <- file.path(prefix, rel)
    key <- gsub("\\\\", "/", key)

    put_object(
      file = f,
      object = key,
      bucket = bucket,
      base_url = base_url
    )
  }

  invisible(TRUE)
}
```

## 2) Exporters

### 2A) Export master samples (sample metadata)

```r
export_samples <- function(mysql_con, sql, out_dir, project_col = "project_id") {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  rs <- dbSendQuery(mysql_con, sql)
  on.exit(dbClearResult(rs), add = TRUE)

  repeat {
    df <- dbFetch(rs, n = 200000)
    if (nrow(df) == 0) break

    # Partition samples by project if you have it (recommended)
    if (!project_col %in% names(df)) df[[project_col]] <- "NA"

    # Write as partitioned dataset (one parquet per chunk per project)
    write_partitioned_chunk(
      df = df,
      out_base = out_dir,
      partition_cols = c(project_col),
      file_prefix = "samples"
    )
  }

  invisible(TRUE)
}
```

### 2B) Export feature metadata per assay

```r
export_features <- function(mysql_con, sql, out_dir, assay) {
  out_base <- file.path(out_dir, sprintf("assay=%s", assay))
  dir.create(out_base, recursive = TRUE, showWarnings = FALSE)

  df <- dbGetQuery(mysql_con, sql)
  # write a single parquet (usually manageable)
  arrow::write_parquet(df, sink = file.path(out_base, "features.parquet"), compression = "zstd")
  invisible(TRUE)
}
```

### 2C) Export sparse counts per assay

```r
export_counts_sparse <- function(mysql_con, sql,
                                 out_dir, assay,
                                 sample_col = "sample_id",
                                 feature_col = "feature_id",
                                 x_col = "x",
                                 project_col = "project_id",
                                 chunk_n = 2000000) {
  out_base <- file.path(out_dir, "counts", sprintf("assay=%s", assay))
  dir.create(out_base, recursive = TRUE, showWarnings = FALSE)

  rs <- dbSendQuery(mysql_con, sql)
  on.exit(dbClearResult(rs), add = TRUE)

  repeat {
    df <- dbFetch(rs, n = chunk_n)
    if (nrow(df) == 0) break

    # ensure required columns exist
    stopifnot(all(c(sample_col, feature_col, x_col) %in% names(df)))
    if (!project_col %in% names(df)) df[[project_col]] <- "NA"

    # enforce types (good for parquet + duckdb)
    df[[sample_col]] <- as.character(df[[sample_col]])
    # strongly recommend integer surrogate for feature_id; if string, keep as character
    if (!is.integer(df[[feature_col]])) {
      suppressWarnings(df[[feature_col]] <- as.integer(df[[feature_col]]))
      if (anyNA(df[[feature_col]])) df[[feature_col]] <- as.character(df[[feature_col]])
    }
    df[[x_col]] <- as.numeric(df[[x_col]])

    # add sample_bucket
    df[["sample_bucket"]] <- vapply(df[[sample_col]], sample_bucket, character(1))

    # add assay column (optional, since path already has assay=...)
    df[["assay"]] <- assay

    # write partitioned:
    # assay (optional), project_id, sample_bucket
    write_partitioned_chunk(
      df = df,
      out_base = out_base,
      partition_cols = c(project_col, "sample_bucket"),
      file_prefix = "part"
    )
  }

  invisible(TRUE)
}
```

## 3) Build a manifest (atlas.json)

```r
write_manifest <- function(bucket, prefix, assays, out_file) {
  base <- sprintf("r2://%s/%s", bucket, prefix)

  manifest <- list(
    version = "v1",
    base_uri = base,
    samples_uri = sprintf("%s/dim/samples/*/*.parquet", base),
    assays = lapply(assays, function(a) {
      list(
        counts_uri   = sprintf("%s/counts/assay=%s/*/*/*.parquet", base, a),
        features_uri = sprintf("%s/dim/features/assay=%s/*.parquet", base, a),
        partition_cols = c("project_id", "sample_bucket"),
        sample_col = "sample_id",
        feature_col = "feature_id",
        x_col = "x",
        taxonomy_cols = c("Kingdom", "Phylum", "Class", "Order", "Family", "Genus", "Species")
      )
    })
  )
  names(manifest$assays) <- assays

  dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(manifest, out_file, pretty = TRUE, auto_unbox = TRUE)
}
```

## 4) Put it together for your assays

### 4.1 Connect to MySQL

```r
mysql_con <- dbConnect(
  RMariaDB::MariaDB(),
  host = "YOUR_HOST",
  user = "YOUR_USER",
  password = "YOUR_PASSWORD",
  dbname = "YOUR_DB",
  port = 3306
)
```

### 4.2 Define the SQL you’ll run

```r
assays <- c("16S", "EUK2", "NEM", "SOILCHEM")

sql_samples <- "
  SELECT * FROM sampleinfo
" # should include sample_id + project_id (recommended) + metadata

sql_features <- list(
  "16S" = "SELECT * FROM tax_16s_features",
  "EUK2" = "SELECT * FROM tax_euk2_features",
  "NEM" = "SELECT * FROM tax_nem_features",
  "SOILCHEM" = "SELECT * FROM soilchem_features"
)

sql_counts <- list(
  "16S" = "SELECT sample_id, feature_id, x, project_id FROM counts_16s_nz",
  "EUK2" = "SELECT sample_id, feature_id, x, project_id FROM counts_euk2_nz",
  "NEM" = "SELECT sample_id, feature_id, x, project_id FROM counts_nem_nz",

  # soil chemistry: if your MySQL table is wide, convert to long in SQL first
  "SOILCHEM" = "SELECT sample_id, analyte_id AS feature_id, value AS x, project_id FROM soilchem_long"
)
```

### 4.3 Run the export to a local staging directory

```r
staging <- "staging_atlas_v1"
dir.create(staging, recursive = TRUE, showWarnings = FALSE)

# 1) samples
export_samples(mysql_con, sql_samples, out_dir = file.path(staging, "dim", "samples"))

# 2) features + counts per assay
for (a in assays) {
  export_features(mysql_con, sql_features[[a]], out_dir = file.path(staging, "dim", "features"), assay = a)
  export_counts_sparse(mysql_con, sql_counts[[a]], out_dir = staging, assay = a)
}

# 3) manifest
write_manifest(
  bucket = "YOUR_BUCKET",
  prefix = "atlas/v1",
  assays = assays,
  out_file = file.path(staging, "manifest", "atlas.json")
)
```

## 5) Upload staging → R2 (one time)

```r
r2_upload_dir(
  local_dir = staging,
  bucket = "YOUR_BUCKET",
  prefix = "atlas/v1",
  account_id = "YOUR_33CHAR_ACCOUNT_ID",
  access_key_id = "YOUR_R2_ACCESS_KEY_ID",
  secret_access_key = "YOUR_R2_SECRET_ACCESS_KEY"
)
```

> R2 has no real folders; object keys with slashes behave like folders in consoles.

## 6) Verify from any machine (read-only key)

```r
library(DBI)
library(duckdb)

con <- dbConnect(duckdb())

dbExecute(con, "INSTALL httpfs;")
dbExecute(con, "LOAD httpfs;")

dbExecute(con, sprintf("
  CREATE SECRET (
    TYPE r2,
    KEY_ID  '%s',
    SECRET  '%s',
    ACCOUNT_ID '%s'
  );
", Sys.getenv("ATLAS_R2_KEY_ID"), Sys.getenv("ATLAS_R2_SECRET"), Sys.getenv("ATLAS_R2_ACCOUNT_ID")))

# sample count
DBI::dbGetQuery(con, "
  SELECT project_id, count(*) n
  FROM read_parquet('r2://YOUR_BUCKET/atlas/v1/dim/samples/*/*.parquet', hive_partitioning=true)
  GROUP BY 1
")

# count rows in sparse table for 16S
DBI::dbGetQuery(con, "
  SELECT count(*) nnz
  FROM read_parquet('r2://YOUR_BUCKET/atlas/v1/counts/assay=16S/*/*/*.parquet', hive_partitioning=true)
")
```

## Implementation notes

1) **Prefer integer `feature_id` everywhere.** Large, string feature IDs become expensive as matrix column names. If you can, use a stable integer surrogate plus a dictionary table in `dim/features/` and only join names when needed.

2) **Plan to compact small Parquet files.** The chunked exporter produces many `part-*.parquet` files; that’s fine for bootstrap, but a later compaction job (DuckDB or Spark) should rewrite partitions into fewer ~100MB+ files. This is a post-ingest maintenance task, not part of the core package.
