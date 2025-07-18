library(ggplot2)
library(ggtree)

fc_plot_tree <- function(tree, high_nodes, title_text, y_buffer = 2) {
  p <- ggtree(tree, layout="rectangular", mrsd="2019-12-31", size=0.3) +
    theme_classic(base_size = 14) +
    theme(
      panel.grid     = element_blank(),
      axis.line.x    = element_line(color="black"),
      axis.ticks     = element_line(color="black"),
      axis.text.x    = element_text(angle=45, hjust=1, vjust=1, color="black"),
      axis.title.x   = element_text(face="plain", color="black"),
      axis.line.y    = element_blank(),
      axis.ticks.y   = element_blank(),
      axis.text.y    = element_blank(),
      plot.title     = element_text(
        hjust   = 0.5,
        face    = "plain",
        size    = 16,
        margin  = margin(b = 8)    
      ),
      plot.margin    = unit(c(5,5,5,5), "mm")
    ) +
    # TREE + NODES
    geom_point2(aes(subset = node %in% high_nodes),
                shape = 21, fill = "white", color = "black", size = 2.5) +
    geom_tippoint(size = 1.5, color = "black", alpha = 0.8) +
    # X-axis padding on both ends
    scale_x_continuous(
      "Sampling Year",
      breaks = seq(1994, 2019, by = 4),
      expand = expansion(add = c(1, 1))
    ) +
    ggtitle(title_text)
  built <- ggplot2::ggplot_build(p)
  y_max <- max(built$data[[1]]$y, na.rm = TRUE)
  p + expand_limits(y = y_max + y_buffer)
}
