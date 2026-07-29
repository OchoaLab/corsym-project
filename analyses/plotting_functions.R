###This script holds three of the primary plotting functions for both simulation and empirical analyses
#theme_professional(): the theme used to create all plots from the paper, Kennedy et al., 2026
#quick_point(): a function designed to create simple geom_point plots with a matching style
#quick_point_base(): a slightly more in depth version of quick_point() that is designed to plot data where x and y are both between 0 and 1 and we were examining the overall correlation

#Load necessary libraries
library(ggplot2)

#Define theme_professional, base_size determines the overall size of all parts of the figure.
theme_professional <- function(
  base_size = 12,
  base_family = "sans"
) {
  #Load the standard theme_bw() and build off of it
  theme_bw(base_size = base_size, base_family = base_family) +
    theme(
      # Text
      text = element_text(color = "black"),
      plot.title = element_text(
        face = "bold",
        size = rel(1.3),
        hjust = 0.5
      ),
      plot.subtitle = element_text(
        size = rel(1.0),
        hjust = 0.5
      ),
      axis.title = element_text(
        face = "bold",
        size = rel(1.1)
      ),
      axis.text = element_text(
        color = "black",
        size = rel(0.9)
      ),

      # Panel
      panel.border = element_rect(
        color = "black",
        linewidth = 0.7
      ),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),

      # Axes
      axis.line = element_line(
        color = "black",
        linewidth = 0.5
      ),
      axis.ticks = element_line(
        color = "black",
        linewidth = 0.5
      ),
      axis.ticks.length = unit(0.15, "cm"),

      # Legend
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.title = element_text(face = "bold"),

      # Facets
      strip.background = element_rect(
        fill = "grey90",
        color = "black"
      ),
      strip.text = element_text(
        face = "bold"
      ),

      # Margins
      plot.margin = margin(10, 10, 10, 10)
    )
}

#Define quick_point 
quick_point <- function(data, x, y,
                       color = NULL,
                       point_size = 3,
                       alpha = 0.7) {
  #Standard geom_point with an alpha and point size defined in function 
  p <- ggplot(
    data,
    aes(
      x = {{ x }},
      y = {{ y }},
      color = {{ color }}
    )
  ) +
    geom_point(
      size = point_size,
      alpha = alpha
    ) +
    #Load theme_professional and a large base_size, move legend to the bottom
    theme_professional(base_size = 30)+
    theme(
      panel.border = element_rect(linewidth = 3),
      legend.position = "bottom"
    )

  return(p)
}

#Define quick_point_base
quick_point_base <- function(data, x, y,
                       color = NULL,
                       point_size = 3,
                       alpha = 0.7) {
  #Standard geom_point with an alpha and point size defined in function 
  p <- ggplot(
    data,
    aes(
      x = {{ x }},
      y = {{ y }},
      color = {{ color }}
    )
  ) +
    geom_point(
      size = point_size,
      alpha = alpha
    ) +
    #Load theme_professional and a large base_size, move legend to the bottom
    theme_professional(base_size = 30)+
    theme(
      panel.border = element_rect(linewidth = 3),
      legend.position = "bottom"
    )+
  #Ensure that the 0-1 scale is used for both axes 
  scale_y_continuous(limits = c(0,1))+
  scale_x_continuous(limits = c(0,1))+
  #Add a dashed identity line for an expected correlation of 1
  geom_abline(intercept = 0,slope=1,linetype="dashed",size=2)
  
  return(p)
}
