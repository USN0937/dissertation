library(dplyr)
library(purrr)

fc_2A_binning <- function(df, step = 0.75, n_boot = 1000, seed = 123) {
  set.seed(seed)
  dat <- df %>%
    filter(!is.na(spatial_dist), !is.na(same_chain)) %>%
    arrange(spatial_dist)
  
  s        <- dat$same_chain
  max_dist <- max(dat$spatial_dist, na.rm = TRUE)
  bins     <- seq(0, max_dist, by = step)
  k_vec    <- vapply(bins, function(d) sum(dat$spatial_dist <= d), integer(1))
  max_k    <- max(k_vec)
  
  # 1) one big bootstrap of size max_k × n_boot
  idx_max   <- matrix(sample.int(max_k, max_k * n_boot, replace = TRUE),
                      nrow = max_k, ncol = n_boot)
  boot_vals <- matrix(s[idx_max], nrow = max_k, ncol = n_boot)
  boot_csum <- apply(boot_vals, 2, cumsum)  # max_k × n_boot
  
  # 2) assemble results
  map_dfr(seq_along(bins), function(i) {
    k <- k_vec[i]
    cutoff <- bins[i]
    if (k == 0) {
      return(tibble(
        bin_cutoff      = cutoff,
        n               = 0L,
        same_chain_count= 0L,
        prop_same_chain = NA_real_,
        se              = NA_real_,
        lower           = NA_real_,
        upper           = NA_real_
      ))
    }
    
    # observed counts & prop
    count <- sum(s[1:k])
    p_obs <- count / k
    
    # bootstrap props for this bin and its SE
    props <- boot_csum[k, ] / k
    se_boot <- sd(props)
    
    # symmetric 1.96*SE CI
    lo <- pmax(0, p_obs - 1.96 * se_boot)
    hi <- pmin(1, p_obs + 1.96 * se_boot)
    
    tibble(
      bin_cutoff        = cutoff,
      n                  = k,
      same_chain_count   = count,
      prop_same_chain    = p_obs,
      se                 = se_boot,
      lower              = lo,
      upper              = hi
    )
  })
}



