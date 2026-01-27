# parqlyt

`parqlyt` is a lazy query engine for sparse assay atlases stored as partitioned Parquet on Cloudflare R2, powered by DuckDB. It focuses on fast, reproducible filtering and aggregation over extremely large, long-form assay tables while keeping sparse matrix alignment consistent across transformations.

## Highlights

- **Lazy, pipeable DSL** for filtering, pruning, and transformation of assay data.
- **Assay-aware contexts** (`by_sample()`, `by_feature()`) to keep operations aligned.
- **Sparse collection** into `Matrix` objects with safe dimensional integrity.
- **DuckDB-backed execution** that pushes work to Parquet with Hive partitioning.

## Installation

```r
# install.packages("devtools")
# devtools::install_github("your-org/parqlyt")
```

## Quick start

```r
library(parqlyt)

# R2 credentials
Sys.setenv(
  ATLAS_R2_BUCKET = "your-bucket",
  ATLAS_R2_PREFIX = "atlas/v1",
  ATLAS_R2_KEY_ID = "your-key-id",
  ATLAS_R2_SECRET = "your-secret",
  ATLAS_R2_ACCOUNT_ID = "your-account-id"
)

# Open atlas and pick an assay
atlas <- atlas_open()
assay_16s <- assay(atlas, "16S")

# Summarise by feature
feature_summary <- assay_16s %>%
  by_feature() %>%
  summarise()

# Prune, collapse by taxonomy, collect sparse matrix
mat <- assay_16s %>%
  by_sample() %>%
  prune(min_total = 1000) %>%
  by_feature() %>%
  prune(min_prev = 0.01) %>%
  collapse_tax("Genus") %>%
  collect(max_features = 50000)
```

## Data layout and manifest

`parqlyt` expects a manifest file (`atlas.json`) that points to Parquet datasets for samples, features, and per-assay counts. The migration guide walks through exporting from MySQL and building a compatible layout.

- **Manifest path**: `r2://<bucket>/<prefix>/manifest/atlas.json`
- **Counts path**: `r2://<bucket>/<prefix>/counts/assay=<assay>/*/*/*.parquet`
- **Samples path**: `r2://<bucket>/<prefix>/dim/samples/*/*.parquet`
- **Features path**: `r2://<bucket>/<prefix>/dim/features/assay=<assay>/*.parquet`

See [docs/migration-guide.md](docs/migration-guide.md) for a full export pipeline.
See [docs/use-cases.md](docs/use-cases.md) for example workflows.

## Common tasks

- **List assays**: `names(atlas$manifest$assays)`
- **Materialize data**: `collect()` returns a sparse matrix.
- **Tidy interop**: `as_long()` plus `samp_tbl()` and `feat_tbl()` for joining metadata.

## Query metadata with `query()`

Use `query()` to search metadata within a context (e.g., `by_sample()` or `by_feature()`). By default, matching is case-insensitive, so `query("soil")` will match `Soil` or `SOIL`. To opt into case-sensitive matching, set `case_sensitive = TRUE`. You can also limit matching to specific columns (for example, `columns = c("host", "site")`), and request exact matches when supported via `exact = TRUE`.

## Notes

- `parqlyt` uses DuckDB’s `httpfs` extension to read Parquet from R2.
- Ensure your R2 credentials are available as `ATLAS_R2_*` environment variables.

## License

MIT
