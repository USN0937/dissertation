library(dplyr)

fc_same_chain <- function(df) {
  df %>%
    mutate(
      delta_yrs  = time_diff / 365,
      raw_mrca   = (patristic_dist - delta_yrs) / 2,
      mrca_age   = pmax(raw_mrca, 0),
      same_chain = if_else(
        delta_yrs <= 0.5 &
          mrca_age  <= 0.5,
        1L, 0L
      )
    )
}

# Estimate MRCA age by subtracting the sampling‐time lag (in years) from the total tree distance and dividing by two
# Floor negative values to zero so that any noise yielding raw_mrca<0 is set to 0 (no negative MRCA ages)
#patristic_dist is the total branch-length distance between the two tips, in years (because tree is time-scaled).
#if one sample was collected Δ years after the other, then part of the observed patristic distance simply reflects that sampling lag.
#remains is twice the time from the older tip back to the MRCA; dividing by 2 yields the raw estimate of how long ago (in years) that MRCA existed

#Same chain if: sampling times are within half a year of each other and reconstructed MRCA is no more than half a year ago

