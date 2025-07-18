# Required packages
library(dplyr)
library(purrr)
library(boot)
library(ggplot2)

# Function: fixed 1‐year bins, compute median spatial distance and 95% bootstrap CIs,
#            plot at the midpoint of each interval
fc_1G_binning_fixed_median <- function(df,
                                       max_dist  = 20,   # years
                                       bin_width  = 1,    # 1‐year bins
                                       n_boot     = 1000,
                                       seed      = 123# bootstrap replicates with replacement
) {
  breaks <- seq(0, max_dist, by = bin_width)        
  mids   <- breaks[-1] - (bin_width / 2)              
  
  map_dfr(seq_along(mids), function(i) {
    lower <- breaks[i]
    upper <- breaks[i + 1]
    df_sub <- df %>%
      filter(!is.na(patristic_dist),
             patristic_dist >  lower,
             patristic_dist <= upper)
    
    # 3) Empty‐bin placeholder
    if (nrow(df_sub) == 0) {
      return(tibble(
        midpoint        = mids[i],
        spatial_median  = NA_real_,
        lower_ci        = NA_real_,
        upper_ci        = NA_real_,
        n               = 0L
      ))
    }
    
    # 4) Bootstrap the median
    boot_out <- boot(
      data      = df_sub$spatial_dist,
      statistic = function(x, idx) median(x[idx], na.rm = TRUE),
      R         = n_boot
    )
    perc_ci <- boot.ci(boot_out, type = "perc")$percent[4:5]
    
    # 5) Return summary for this bin
    tibble(
      midpoint       = mids[i],
      spatial_median = median(df_sub$spatial_dist, na.rm = TRUE),
      lower_ci       = perc_ci[1],
      upper_ci       = perc_ci[2],
      n              = nrow(df_sub)
    )
  })
}
