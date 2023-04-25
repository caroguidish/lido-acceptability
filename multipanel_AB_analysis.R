# Plot Panel A and Panel B together in one plot

require(cowplot)
require(gridExtra)
library(grid)

combined_plots <- plot_grid(indiv_lines + theme(legend.position = 'none'),
                              corr_plot + theme(legend.position = 'none'),
                              labels = c("A","B"),
                              align = "h",
                              rel_widths = c(1.5, 1.5),
          nrow = 1)

panels_ab <- ggdraw(add_sub(combined_plots, "n = 17", x = 0.5, vjust = 0))


save_plot("2023.04.17_PanelAB_final.pdf", 
       plot = combined_plots, 
       base_width = 184, 
       base_height = 100, 
       units = "mm")

