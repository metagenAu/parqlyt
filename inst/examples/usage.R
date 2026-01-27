library(parqlyt)

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

# Example D: metadata query
soil_ids <- asv %>%
  by_sample() %>%
  query("soil")

soil_ids_host_site <- asv %>%
  by_sample() %>%
  query("soil", columns = c("host", "site"))

soil_asv <- keep_samples(asv, soil_ids)

soil_asv_alt <- asv %>%
  by_sample() %>%
  query("soil") %>%
  keep_samples(asv, .)

soil_taxa <- asv %>%
  by_feature() %>%
  query("Bacteroides", columns = "taxonomy", exact = TRUE)
