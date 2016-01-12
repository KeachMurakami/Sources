### for my ggplot2 theme
gg_theme <- function (legend = "none", ...) {
  theme_bw(...) %+replace%
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          legend.position = legend,
          legend.key = element_blank(),
          legend.background = element_blank(),
          panel.border = element_rect(fill = NA,colour="black"),  
          strip.background = element_blank(),
          plot.background = element_blank(),
          panel.background = element_blank(),
          panel.margin = unit(0, units = "points")
    )
}

### fix axes ranges
gg_xy <-
  function(ranges, ...){
    list(gg_x(ranges[1:2], ...),
         gg_y(ranges[3:4], ...))
  }

gg_x <-
  function(x_range, ...){
    scale_x_continuous(limits = range(x_range), expand = c(0, 0), ...)
  }

gg_y <-
  function(y_range, ...){
    scale_y_continuous(limits = range(y_range), expand = c(0, 0), ...)
  }