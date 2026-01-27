library(atlasr)

# Example A: open atlas + list assays + summary query
A <- atlas_open()
asv <- assay(A, "16S")
df <- asv %>% by_feature() %>% summarise()
head(df)

# Example B: prune + collapse + collect
mat <- asv %>%
  by_sample() %>% prune(min_total = 1000) %>%
  by_feature() %>% prune(min_prev = 0.01) %>%
  collapse_tax("Genus") %>%
  collect(max_features = 50000)

# Example C: tidy interop
# df_long <- as_long(mat) %>% merge(samp_tbl(mat), by = "sample_id")
# library(ggplot2)
# ggplot(df_long, aes(x = sample_id, y = x)) + geom_point()
