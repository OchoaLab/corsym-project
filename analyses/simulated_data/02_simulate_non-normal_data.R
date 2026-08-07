###This script simulates data from the simulated functions for non-normal data and plots it
# load new function
source( 'simulation_analysis_functions.R' )
source( 'plotting_functions.R' )

# load libraries
library(MASS)
library(tidyverse)
library(copula)
library(ggplot2)
library(ggh4x)
library(ggtext)
library(patchwork)
library(corsym)

############################
#Supplemental Non-normal Data
############################

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

