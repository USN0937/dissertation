fc_eta_binning <- function(
    df,
    distance_col   = "spatial_dist",
    same_chain_col = "same_chain",
    bin_width      = 0.75,
    n_boot         = 500,
    time_col       = NULL,
    max_time       = NULL
) {
  library(dplyr)
  library(purrr)
  library(boot)
  
  # optional within-season filter
  if (!is.null(time_col) && !is.null(max_time)) {
    df <- df %>% filter(.data[[time_col]] <= max_time)
  }
  
  # define bins starting at 0
  max_d <- max(df[[distance_col]], na.rm = TRUE)
  bins  <- seq(0, max_d + bin_width, by = bin_width)
  
  # bootstrap statistic: reciprocal of mean(same_chain)
  eta_stat <- function(x, i) {
    p <- mean(x[i], na.rm = TRUE)
    if (p == 0) return(NA_real_)
    1 / p
  }
  
  result <- map_dfr(bins, function(d_cut) {
    sub <- df %>% filter(.data[[distance_col]] < d_cut,
                         !is.na(.data[[same_chain_col]]))
    n    <- nrow(sub)
    if (n < 2) {
      return(tibble(
        bin_cutoff = d_cut,
        eta        = NA_real_,
        ci_lower   = NA_real_,
        ci_upper   = NA_real_,
        n_pairs    = n
      ))
    }
    
    obs_eta <- eta_stat(sub[[same_chain_col]], seq_len(n))
    bt      <- boot(sub[[same_chain_col]], eta_stat, R = n_boot)
    ci_tbl  <- tryCatch({
      q <- boot.ci(bt, type = "perc")$percent[4:5]
      tibble(ci_lower = q[1], ci_upper = q[2])
    }, error = function(e) tibble(ci_lower = NA_real_, ci_upper = NA_real_))
    
    tibble(bin_cutoff = d_cut, eta = obs_eta, n_pairs = n) %>%
      bind_cols(ci_tbl)
  })
  
  return(result)
}