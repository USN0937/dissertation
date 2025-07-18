fc_combined_df <- function(patristic_df, spatial_df, temporal_df) {
  
  patristic_long <- patristic_df %>%
    rownames_to_column(var = "sample1") %>%
    pivot_longer(
      cols = -sample1,
      names_to = "sample2",
      values_to = "patristic_dist"
    ) %>%
    filter(sample1 != sample2)
  
  spatial_long <- spatial_df %>%
    rownames_to_column(var = "sample1") %>%
    pivot_longer(
      cols = -sample1,
      names_to = "sample2",
      values_to = "spatial_dist"
    ) %>%
    filter(sample1 != sample2)
  
  temporal_long <- temporal_df %>%
    rownames_to_column(var = "sample1") %>%
    pivot_longer(
      cols = -sample1,
      names_to = "sample2",
      values_to = "time_diff"
    ) %>%
    filter(sample1 != sample2)
  
  combined_df <- patristic_long %>%
    inner_join(spatial_long, by = c("sample1", "sample2")) %>%
    inner_join(temporal_long, by = c("sample1", "sample2"))
  
  return(combined_df)
}
