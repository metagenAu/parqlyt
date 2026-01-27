# Example use cases

This document showcases practical workflows you can build with `parqlyt`. Each example is meant to be copied as a starting point and customized for your atlas.

## 1) Profile a single assay by feature abundance

Goal: Summarize the most prevalent features across the entire assay.

```r
library(parqlyt)

atlas <- atlas_open()
assay_16s <- assay(atlas, "16S")

feature_summary <- assay_16s %>%
  by_feature() %>%
  summarise()

feature_summary
```

**When to use it**: You want a quick overview of high-coverage taxa (or genes) before deeper filtering.

## 2) Build a per-sample sparse matrix with quality filtering

Goal: Drop low-quality samples and low-prevalence features, then collect a matrix for downstream analysis.

```r
library(parqlyt)

atlas <- atlas_open()
assay_16s <- assay(atlas, "16S")

mat <- assay_16s %>%
  by_sample() %>%
  prune(min_total = 1000) %>%
  by_feature() %>%
  prune(min_prev = 0.01) %>%
  collect(max_features = 50000)

mat
```

**When to use it**: You need a clean sample-by-feature matrix for ordination, clustering, or differential abundance.

## 3) Collapse to a taxonomic rank before collecting

Goal: Aggregate features to a taxonomy level (e.g., genus) before matrix collection.

```r
library(parqlyt)

atlas <- atlas_open()
assay_16s <- assay(atlas, "16S")

mat_genus <- assay_16s %>%
  by_sample() %>%
  prune(min_total = 1000) %>%
  by_feature() %>%
  prune(min_prev = 0.01) %>%
  collapse_tax("Genus") %>%
  collect(max_features = 50000)

mat_genus
```

**When to use it**: You want a stable feature set at a higher taxonomic resolution for comparisons.

## 4) Join sample metadata to long-form assay data

Goal: Build a tidy table with per-sample metadata for modeling or visualization.

```r
library(parqlyt)

atlas <- atlas_open()
assay_16s <- assay(atlas, "16S")

long_tbl <- assay_16s %>%
  by_sample() %>%
  prune(min_total = 1000) %>%
  as_long() %>%
  dplyr::left_join(samp_tbl(assay_16s), by = "sample_id")

long_tbl
```

**When to use it**: You need tidy data for ggplot2, modeling, or exporting to another system.

## 5) Compare two assays with aligned feature pruning

Goal: Apply the same prevalence filter to multiple assays for consistent comparison.

```r
library(parqlyt)

atlas <- atlas_open()
assay_16s <- assay(atlas, "16S")
assay_its <- assay(atlas, "ITS")

shared_filter <- function(x) {
  x %>%
    by_feature() %>%
    prune(min_prev = 0.05)
}

mat_16s <- assay_16s %>%
  shared_filter() %>%
  collect(max_features = 50000)

mat_its <- assay_its %>%
  shared_filter() %>%
  collect(max_features = 50000)

list(mat_16s = mat_16s, mat_its = mat_its)
```

**When to use it**: You want comparable feature coverage across assays for multi-omics analysis.
