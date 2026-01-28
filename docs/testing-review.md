# Testing review and plan

## Current coverage snapshot

Existing tests cover:

- `prune()` alignment on local assays for sample pruning. (`prune.assay` + `by_sample()` plumbing)
- `collapse_tax()` local aggregation sums.
- `transform(relab)` local sample-wise normalization.
- `collect()` returning a sparse matrix from a minimal DuckDB-backed atlas reference.

These are good smoke tests, but most public surface area remains untested. The items below outline gaps and a plan to close them.

## Missing tests (by module)

### `R/atlas.R` and `R/manifest.R`

- **`atlas_open()` error handling**
  - Missing `ATLAS_R2_*` env vars should error.
  - `bucket` required if `manifest_uri` is empty.
  - `manifest` passed directly bypasses `read_manifest()`.
- **`create_atlas_views()` view creation**
  - Creates `v_samples` only when `samples_uri` exists.
  - Creates per-assay feature/count views and optional `aux` views.
- **`assay()` guard rails**
  - Errors on non-`atlas` object.
  - Errors on unknown assay name.
- **`read_manifest()` input validation**
  - Rejects multiple objects.
  - Handles invalid JSON or missing manifest.
- **`normalize_manifest()`**
  - Rejects manifest missing `assays` key.

### `R/connect.R`

- **DuckDB connection**
  - `connect_duckdb()` returns a live connection.
  - `ensure_httpfs()` loads extension (exercise once with a local, no-network DB).
- **`set_r2_secret()` SQL escaping**
  - Single quotes in credentials are escaped.
  - `persistent = TRUE` emits `PERSISTENT`.

### `R/objects.R`

- **`new_assay()` validation**
  - Rejects non-`dgCMatrix` X.
  - Requires row/col names.
  - Trims X/metadata to intersecting IDs for `feat` and `samp`.
- **`new_assay_ref()` / `new_assay_ctx()`**
  - Class and field initialization.
- **`by_sample()` / `by_feature()` / `end()`**
  - `end()` unwraps only for `assay_ctx`.

### `R/verbs_filters.R`

- **Local (`assay`) filtering**
  - `keep_*` preserves alignment between X and metadata.
  - `drop_*` removes IDs and keeps order.
  - `select_*_where` uses R expression and errors without metadata.
- **Lazy (`assay_ref`) filtering**
  - `keep_*` populates `*_filter` and includes `sample_bucket` when partitioned.
  - `drop_*` builds proper `WHERE` and composes with existing filters.
  - `select_*_where` uses views and errors when `samples` view is absent.
- **Edge cases**
  - Empty id list should error (via `ref_apply_keep`).

### `R/verbs_prune.R`

- **Local pruning**
  - Both `sample` and `feature` margins.
  - `min_total`, `min_prev`, and `predicate` combined.
  - `predicate` receives stats in expected shape.
- **Lazy pruning**
  - Uses `stats` parquet when present (sample + feature stats), including required columns check.
  - Falls back to counts SQL when stats missing or incomplete.
  - `min_prev` interactions with `n_features`.

### `R/verbs_transform.R`

- **Local transform methods**
  - `log1p` alters nonzeros only.
  - `relab` warns on zero-sum samples and leaves zeros.
  - `hellinger` composed with relab.
  - Errors on `relab/hellinger` when `margin != sample`.
- **Lazy transform plan**
  - `transform_plan` entries appended correctly for each method.
  - Errors for unsupported method.

### `R/verbs_collapse.R`

- **Local collapse**
  - `collapse_tax()` missing rank errors.
  - `missing_tokens` + `missing_label` normalization.
  - `drop_missing_rank` removes features before collapse.
  - `collapse_by()` group length validation.
- **Lazy collapse**
  - `collapse_tax.assay_ref()` rejects rank not in `taxonomy_cols`.
  - `drop_missing_rank` adds rank filter into SQL.
  - `collapse_by.assay_ref()` requires data.frame with correct columns.

### `R/verbs_collect.R`

- **Limits/force**
  - `max_features`, `max_nnz`, `max_cells` error without `force = TRUE`.
- **Metadata collection**
  - `with_samp` and `with_feat` toggles.
  - `collapse_plan` -> feature table `path` handling.
  - respects `sample_filter$ids` / `feature_filter$ids` ordering.
- **Returned object**
  - `new_assay()` called with expected dimensions and metadata rownames.

### `R/ref_sql.R`

- **`ref_counts_sql()` query composition**
  - Base query uses `sample_col`, `feature_col`, `x_col`.
  - Adds `WHERE` clauses for filters.
  - Adds collapse JOIN/GROUP BY with optional `rank_filter`.
  - Applies `transform_plan` in order.
- **`apply_transform_sql()`**
  - Unknown type errors.
  - `relab` SQL has group-by sums.
- **`ref_apply_keep()`**
  - Errors on empty ids.
  - Adds `sample_bucket` filter when partitioned.

### `R/tidy_interop.R`

- **`as_long()`**
  - Errors on `assay_ref` unless collected.
  - Respects `max_nnz` limit.
  - Output columns and row count match NNZ.
- **`samp_tbl()` / `feat_tbl()`**
  - Local returns rownames as ID columns.
  - Lazy returns view data; returns `NULL` when view missing.

### `R/print.R`

- **S3 print methods**
  - `print.assay()` output includes dims and metadata info.
  - `print.assay_ref()` reflects set filters and plans.
  - `print.assay_ctx()` displays margin and object type.

### `R/utils_hash.R`

- **`md5_hex()`**
  - Errors when `digest` missing.
  - Hash output length and deterministic results.
- **`sample_bucket()`**
  - Stable 2-char bucket prefix.

## Implementation plan

### 1) Build test fixtures (Week 1)

- Create helpers in `tests/testthat/helper-fixtures.R`:
  - `make_toy_assay()` (extend current version).
  - `make_duckdb_atlas()` to create local parquet fixtures for counts/features/samples.
  - Optional `make_manifest()` to toggle `stats` fields and `partition_cols`.

### 2) Unit tests for local assays (Week 1–2)

- Cover `new_assay()`, `prune.assay()`, `transform.assay()`, `collapse_tax()` and `collapse_by()`.
- Validate alignment and edge cases (missing metadata, missing rank, zero-sum relab).

### 3) Unit tests for lazy refs & SQL composition (Week 2)

- Exercise `ref_counts_sql()` string composition with `keep_*`, `drop_*`, `collapse_*`, `transform()`.
- `ref_apply_keep()` sample bucket logic with a manifest containing `sample_bucket` in `partition_cols`.

### 4) Integration tests with DuckDB (Week 2–3)

- Expand the existing `collect()` test into a reusable fixture.
- Add tests for `summarise.assay_ref()`, `prune.assay_ref()`, `select_*_where()` and `collapse_tax.assay_ref()`.
- Validate `with_samp` and `with_feat` toggles.

### 5) Behavior and regression tests (Week 3)

- `as_long()` max-nnz guard.
- Print methods smoke tests (capture output with `testthat::capture_output`).
- `set_r2_secret()` SQL escaping.
- `normalize_manifest()` and `read_manifest()` error cases.

### 6) Automation and CI (Week 3)

- Ensure `Suggests: testthat` is used via `tests/testthat.R` and `testthat::test_check()`.
- Add an R CMD check job (if CI exists) and run tests in CI with DuckDB installed.

## Recommended test prioritization

1. **Data integrity/limits**: `collect()` limits, `prune()` logic, `transform()` correctness.
2. **SQL composition**: `ref_counts_sql()` with filters, collapse, transforms.
3. **Atlas/manifest guard rails**: missing env vars, missing manifest fields.
4. **Tidy interop**: `as_long()`, `samp_tbl()`, `feat_tbl()`.
5. **Print/utility**: print methods, hash helpers.

## Notes on test tooling

- Prefer `testthat::local_tempdir()` for parquet fixtures.
- Use DuckDB in-memory databases to avoid external dependencies.
- For `ensure_httpfs()`, allow skipping if extension not available on the runner.
