fc_tree_prep <- function(mcc_tree, posterior_threshold = 0.90) {
  if (!requireNamespace("ggtree", quietly = TRUE)) {
    stop("Please install the 'ggtree' package first.")
  }
  options(ignore.negative.edge = TRUE)
  
  tree <- mcc_tree
  
  # Ensure it has the needed columns
  df <- tree@data
  if (!all(c("node", "posterior") %in% colnames(df))) {
    stop("`tree@data` must contain 'node' and 'posterior' columns")
  }
  
  high_nodes <- df$node[df$posterior >= posterior_threshold]
  
  # Return both for downstream plotting
  list(
    tree       = tree,
    high_nodes = high_nodes
  )
}
