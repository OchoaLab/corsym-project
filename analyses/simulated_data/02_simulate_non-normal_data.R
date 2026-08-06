###This script simulates data from the simulated functions for non-normal data and plots it
# load new function
source( 'simulation_analysis_functions.R' )

# load libraries
library(MASS)
library(tidyverse)
library(copula)
library(ggplot2)
library(ggh4x)
library(ggtext)
library(patchwork)

nonnormal_plotting<-function(rhos,n,type){
 
  # Simulated and combine into one long data frame
  sim_data <- do.call(rbind, lapply(rhos, simulate_non_normal_cordata,distribution=type))
  #Create and export the necessary data for plot 1a, a dataframe with the information of the fully ordered and non-ordered samples, and a correlation dataframe saving the specific correlation values for text addition
  nonnormal_plot_data<-sim_data %>%
    mutate(row_index = row_number())%>%
    #Ensure the original data is assigned as random
    transmute(rho, row_index,
              x = x, y = y, type = "Random") %>%
    bind_rows(
      sim_data %>%
        mutate(row_index = row_number(),
               var3 = ifelse(x > y, y, x),
               var4 = ifelse(x > y, x, y)) %>%
        transmute(rho, row_index,
                  x = var3, y = var4, type = "Ordered")
    ) %>%
    filter(!is.na(x), !is.na(y))%>%
    #Round for easy visuals and make Random vs Ordered a type for facetting
    mutate(rho=round(rho,digits = 3),
           type= factor(type,
                        levels = c("Random", "Ordered")),
           Plot_rename="True Rho"
    )

  #Create Correlation Dataframe for labels
  nonnormal_cor_df <- nonnormal_plot_data %>%
    #Rename for better plotting
    mutate(Rho=rho)%>%
    group_by(type, Rho) %>%
    summarise(
      R = pearson(x, y),
      corR = corsym(x,y),
      .groups = "drop"
    )%>%
    #Make nice labelled correlations for plotting
    mutate(
      label = sprintf(
        "<i>r<sub>p</sub></i> = %.2f<br><i>r<sub>c</sub></i> = %.2f",
        R, corR)
    )
  name_fun <- function(value) {
    paste0("Correlation = ", value)
  }
  
  #Plot data in a facetted grid across all rhos and sizes
  plot<-sim_data%>%
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

############################
#Supplemental Non-normal Data
############################

  return( list( x = x, y = y, n = n ) )
}
rhos<-c(-1,-.5,0,.5,1)


p_uni<-nonnormal_plotting(rhos,1000,"uniform")+
  labs(y="Vector 2 (Uniform)",x="Vector 1 (Uniform)")
  
p_ushape<-nonnormal_plotting(rhos,1000,"u_shaped")+
  labs(y="Vector 2 (U-Shape)",x="Vector 1 (U-Shape)")

p_skew<-nonnormal_plotting(rhos,1000,"slightly_skewed")+
  labs(y="Vector 2 (Skewed)",x="Vector 1 (Skewed)")
  
p_supplfig_nonnormal<-p_skew +  p_uni + p_ushape + plot_layout(ncol=1)+
  plot_annotation(tag_levels = "A") & theme(
    panel.border=element_rect(linewidth=3),
    plot.tag = element_text(face = 'bold',size=35),
    strip.background = element_rect(linewidth = 3))


ggsave("SupplementalFigure_Nonnormal_data.pdf",p_supplfig_nonnormal,
       dpi=600,width=29.5,height=30,units="in")

