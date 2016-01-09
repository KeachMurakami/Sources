### for my ggplot2 theme
gg_theme <- function (legend = "none", ...) {
  theme_bw(...) %+replace%
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          legend.position = legend,
          panel.border = element_rect(fill = NA,colour="black"),  
          strip.background = element_rect(fill="white",colour="black"),
          plot.background = element_blank(),
          panel.background = element_blank(),
          panel.margin = unit(0, units = "points")
    )
}
