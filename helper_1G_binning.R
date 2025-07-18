library(dplyr,
        purrr)

fc_1G_binning <- function(df) {
  #numeric vector of breakpoints from 0.25 to 20 in steps of 0.25
  thresholds <- seq(0.25, 20, by = 0.25)

  map_dfr(thresholds, function(g) {
    df %>%
      filter(!is.na(patristic_dist), patristic_dist <= g) %>% #cumulative binning 
      summarise(
        evolutionary_time = g, # record the cutoff
        spatial_mean      = mean(spatial_dist, na.rm = TRUE),
        spatial_sd        = sd(spatial_dist,   na.rm = TRUE),
        n                 = n(),
        spatial_se        = spatial_sd / sqrt(n),
        ci_mult           = qt(0.975, df = n - 1), #the critical value for a two‐tailed 95% interval
        spatial_lower     = spatial_mean - ci_mult * spatial_se, #the endpoints of the 95% confidence interval
        spatial_upper     = spatial_mean + ci_mult * spatial_se #the endpoints of the 95% confidence interval
      )
  })
}
#The lower and upper bounds of that 95% confidence interval


