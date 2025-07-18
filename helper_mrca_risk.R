#  n_sw = count in (spatial_bin = S, mrca_bin = W)
#   n_s = total pairs in spatial_bin S
#  n_rw = count in (spatial_bin = ">10 km", mrca_bin = W)
#   n_r = total pairs in ">10 km" bin
#
compute_rr_ci <- function(n_sw, n_s, n_rw, n_r, B = 2000) {
  # 1) continuity‐correction if you have a zero cell
  if (n_sw == 0 || n_rw == 0) {
    n_sw <- n_sw + 0.5
    n_s  <- n_s  + 0.5
    n_rw <- n_rw + 0.5
    n_r  <- n_r  + 0.5
  }
  
  # 2) build 0/1 vectors
  y1 <- c(rep(1, n_sw),      rep(0, n_s  - n_sw))
  y2 <- c(rep(1, n_rw),      rep(0, n_r  - n_rw))
  
  # 3) observed RR
  rr_obs <- mean(y1) / mean(y2)
  
  # 4) bootstrap replicates
  rr_boot <- replicate(B, {
    m1 <- mean(sample(y1, length(y1), replace = TRUE))
    m2 <- mean(sample(y2, length(y2), replace = TRUE))
    m1 / m2
  })
  
  # 5) percentile CI
  ci <- quantile(rr_boot, c(0.025, 0.975), names = FALSE)
  
  c(
    RR       = rr_obs,
    CI_lower = ci[1],
    CI_upper = ci[2]
  )
}
