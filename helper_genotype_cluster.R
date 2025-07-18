
library(ape)
library(cluster)
library(tibble)

fc_genotype_cluster <- function(consensus_tree,
                                k               = NULL,
                                max_k_override  = 10,
                                hclust_method   = "average") {
  # compute patristic distances
  dist_mat <- cophenetic.phylo(consensus_tree)
  dist_obj <- as.dist(dist_mat)
  
  # hierarchical clustering
  hc <- hclust(dist_obj, method = hclust_method)
  
  # determine max_k
  n_tips <- nrow(dist_mat)
  max_k  <- min(max_k_override, n_tips - 1)
  
  # choose k
  if (is.null(k)) {
    ks              <- 2:max_k
    avg_sil_widths <- sapply(ks, function(kk) {
      cl  <- cutree(hc, k = kk)
      sil <- silhouette(cl, dist_obj)
      mean(sil[, "sil_width"])
    })
    best_k <- ks[which.max(avg_sil_widths)]
  } else {
    if (! (k %in% 2:max_k)) {
      stop("`k` must be between 2 and ", max_k)
    }
    best_k <- k
  }
  
  # final cut
  cluster_assignments <- cutree(hc, k = best_k)
  names(cluster_assignments) <- rownames(dist_mat)
  
  # pack into tibble
  genotype_df <- tibble(
    tip      = names(cluster_assignments),
    genotype = paste0("G", cluster_assignments)
  )
  # store attribute
  attr(genotype_df, "k") <- best_k
  
  genotype_df
}
