s.plot <- function (base_size = 12, base_family = "sans")
{
  theme(plot.title = element_text(family = base_family,
                                  size = base_size * 1.2, 
                                  face = "bold", 
                                  color = "#222222",
                                  hjust = 0), 
        plot.subtitle = element_text(family = base_family,
                                     size = base_size,
                                     hjust = 0,
                                     margin = margin(9, 0, 9, 0)), 
        plot.caption = element_text(family = base_family,
                                    size = base_size * .9, 
                                    color = "#222222",
                                    hjust = 1), 
        legend.position = "top", 
        legend.text.align = 0, 
        legend.background = element_blank(),
        legend.title = element_blank(), 
        legend.key = element_blank(),
        legend.text = element_text(family = base_family, 
                                   size = base_size,
                                   color = "#222222"), 
        axis.title = element_text(size = base_size),
        axis.text = element_text(family = base_family, 
                                 size = base_size,
                                 color = "#222222"), 
        axis.text.x = element_text(vjust = 0, 
                                   margin = margin(t = base_size * 0.2, b = base_size * 0.2, unit = "pt")),
        axis.text.y = element_text(hjust = 1, 
                                   margin = margin(l = base_size * 0.2, r = base_size * 0.2, unit = "pt")),
        axis.text.y.right = element_text(hjust = 0, 
                                         margin = margin(l = base_size * 0.2, r = base_size * 0.2, unit = "pt")),
        axis.ticks.y = element_line(color = "#cbcbcb", size = base_size / 26),
        axis.ticks.length.x = unit(base_size * 0.5, "points"), 
        axis.ticks.length.y = unit(base_size, "points"), 
        axis.line.x = element_line(colour = "#333333", 
                                   size = base_size / 22, linetype = "solid"),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(color = "#cbcbcb", size = base_size / 26),
        panel.grid.major.x = element_blank(), 
        panel.background = element_blank(), 
        strip.background = element_blank(),
        strip.text = element_text(size = base_size, face = "bold", hjust = 0, color = "#222222",
                                  margin = margin(t = base_size * 1.2, b = base_size * 1.2, unit = "pt")),
        plot.background = element_rect(fill = "#fcfcfc")
  )
}

# options(ggplot2.discrete.color = list(viridis::viridis(3), 
#                                       viridis::viridis(4), 
#                                       viridis::viridis(6),
#                                       viridis::viridis(12)),
#         ggplot2.discrete.color = list(viridis::viridis(3), 
#                                       viridis::viridis(4), 
#                                       viridis::viridis(6),
#                                       viridis::viridis(12)))

# breaks <- c(seq(1990, 2016, 10), 2016)
# highlights <- c(1990, 2016)


s.get_labs <- function(breaks, highlights, clearNonHL = F) {
  index <- breaks %in% highlights
  labs <- as.character(breaks)
  
  if (clearNonHL) {
    labs[!index] <- rep("", length(labs))[!index]
  } else {
    labs[!index] <- stringr::str_sub(as.character(breaks), -2, -1)[!index]
  }
  
  result = tibble(breaks = breaks,
                  labs = labs)
  return(result)
}


