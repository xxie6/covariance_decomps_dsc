
plot_loadings <- function(L_est, Pop, legendYN = TRUE, scales_option = "fixed"){
  # options for scales_option are "fixed" and "free_y"
  n <- nrow(L_est)
  k <- ncol(L_est)
  Idx <- rep(c(1:n), k)
  Loading <- c(L_est)
  Factor <- paste0('k=',c(sapply(c(1:k), function(x, n){rep(x, n)}, n = n)))
  tib <- data.frame(Idx, Loading, Factor, Pop)
  plt <- ggplot(tib, aes(x = Idx, y = Loading, col = Pop)) +
    geom_point(show.legend = legendYN) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    facet_grid(cols = vars(Factor), scales = scales_option) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          panel.spacing = unit(1, "lines"))
  plot(plt)
}

plot_heatmap <- function(L, title = "", colors_range = c("gray96", "red"), brks = NULL){
  ### define the color map
  cols <- colorRampPalette(colors_range)(49)
  if (is.null(brks) == TRUE){
    brks <- seq(min(L), max(L), length=50)
  }
  
  plt <- pheatmap(L, show_rownames = FALSE, show_colnames = FALSE, cluster_rows = FALSE, cluster_cols = FALSE, color = cols, breaks = brks, main = title)
  return(plt)
}

dot_plot <- function(data, title, x_label, y_label){
  p <- ggplot(data, aes(x = metric, y = method)) +
    geom_point(size = 3, aes(color = grouping)) + # Add dots
    geom_segment(aes(x = min(metric), xend = metric, y = method, yend = method, color = grouping),
                 linewidth = 0.5) + # Add connecting lines from the minimum
    labs(title = title,
         x = x_label,
         y = y_label) +
    theme_minimal() + # Use a minimal theme for a clean look
    theme(
      panel.grid.major.y = element_blank(), # Remove horizontal grid lines
      panel.grid.minor.y = element_blank(), # Remove minor horizontal grid lines
      axis.text.y = element_text(size = 10), # Adjust y-axis label size
      axis.title.y = element_text(size = 12, face = "bold"),
      axis.title.x = element_text(size = 12, face = "bold"),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5) # Center and bold title
    )
  return(p)
}