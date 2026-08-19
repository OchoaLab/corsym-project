###This script holds three of the primary plotting functions for both simulation and empirical analyses
#theme_professional(): the theme used to create all plots from the paper, Kennedy et al., 2026
#quick_point(): a function designed to create simple geom_point plots with a matching style
#quick_point_base(): a slightly more in depth version of quick_point() that is designed to plot data where x and y are both between 0 and 1 and we were examining the overall correlation
#name_fun(): a very simple naming function for plot 1A

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

#Function to help in naming conventions for plot 1A
name_fun <- function(value) {
paste0("Correlation = ", value)
}

#Function to plot nonnormal data
nonnormal_plotting<-function(rhos,n,type){
  
  # Simulated and combine into one long data frame
  sim_data <- do.call(rbind, lapply(rhos, simulate_non_normal_cordata,distribution=type))
  #Create and export the necessary data for plot 1a, a dataframe with the information of the fully ordered and non-ordered samples, and a correlation dataframe saving the specific correlation values for text addition
  nonnormal_plot_data<-sim_data %>%
    mutate(type = "Random") %>%
    bind_rows(
      cbind(sim_data %>% select(-c(x,y)),
            reorder_xy(sim_data))%>%
        mutate(type = "Extreme")
    ) %>%
    #Round for easy visuals and make Random vs Ordered a type for facetting
    mutate(rho=round(rho,digits = 3),
           type= factor(type,
                        levels = c("Random", "Extreme")),
           Plot_rename="True Rho"
    )
  
  #Create Correlation Dataframe for labels
  nonnormal_cor_df <- nonnormal_plot_data %>%
    #Rename for better plotting
    mutate(Rho=rho)%>%
    #Get summary correlation coefficients
    group_by(type, Rho) %>%
    summarise(
      pear_r = pearson(x, y)[1],
      cor_r = corsym(x,y)[1],
      .groups = "drop"
    )%>%
    #Make nice labelled correlations for plotting
    mutate(
      label = sprintf(
        "<i>r<sub>p</sub></i> = %.2f<br><i>r<sub>c</sub></i> = %.2f",
        pear_r, cor_r)
    )
  
  #Plot data in a facetted grid across all rhos and sizes
  plot<-nonnormal_plot_data%>%
    mutate(Rho=rho)%>%
    ggplot(.,aes(x=x,y=y))+
    geom_point()+
    geom_abline(slope=1,intercept = 0,linetype="dashed",linewidth=1.5)+
    geom_smooth(method = "lm",se = TRUE,color = "steelblue",fill = "grey70",linewidth = 1,alpha = 0.25)  +
    facet_grid(type~Rho,labeller = labeller(Rho = as_labeller(name_fun), type = label_value))+
    theme_professional(base_size = 30)+
    theme(panel.border = element_rect(linewidth=1.5),
          strip.text = element_text(size=25,
                                    face = "bold"
          ),
          legend.position = "bottom",
          strip.placement = "outside",
          strip.switch.pad.grid = unit(0.1, "cm"))+
    geom_richtext(data=nonnormal_cor_df,
                  aes(label = label), x = -Inf,y = Inf,
                  fill = scales::alpha("white", 0.8),
                  label.color = "grey40",
                  hjust = 0,
                  vjust = 1,size=11
    ) 
  
  return(plot)
}

error_bars <- function(value, variance, direction = "y", width = 0) {
  
  if (direction == "y") {
    geom_errorbar(
      aes(
        ymin = {{ value }} - {{ variance }},
        ymax = {{ value }} + {{ variance }}
      ),
      width = width
    )
    
  } else if (direction == "x") {
    geom_errorbarh(
      aes(
        xmin = {{ value }} - {{ variance }},
        xmax = {{ value }} + {{ variance }}
      ),
      height = width
    )
  }
}
