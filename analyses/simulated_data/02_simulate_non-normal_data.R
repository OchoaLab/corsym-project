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

############################
#Supplemental Skewed Effect Data
############################

#Simulate data by rho for 1, -.5, 0, -0.5, and 1 for n=1000
rhos <- seq(-1, 1, by = 0.01)

#Generate data
sim_data <- do.call(rbind, lapply(rhos, simulate_cordata,n=10000))
sim_data<-cbind(
  sim_data %>% dplyr::select(-c(x,y)),
  reorder_xy(sim_data,1))
sim_plot<-cbind(
  sim_data %>% dplyr::select(-c(x,y)),
  exp(sim_data[c(2,3)])
  )%>%
  mutate(Rho=rho)%>%
  group_by(Rho) %>%
  summarise(
    pear_r = pearson(x, y)[1],
    cor_r = corsym(x,y)[1],
    .groups = "drop"
  )%>%
  ggplot(.,aes(x=cor_r,y=pear_r))+
  geom_point()+
  labs(y="Pearson Estimate (Extreme Order)",x="CorSym Estimate")+
  geom_vline(xintercept = 0,linetype="dashed",linewidth=1.5)+
  geom_hline(yintercept = 0,linetype="dashed",linewidth=1.5)+
  geom_abline(slope=1,intercept = 0,linetype="dashed",linewidth=1.5)+
  theme_professional(base_size = 75)+
  theme(panel.border = element_rect(linewidth=3),
        strip.text = element_text(size=25,
                                  face = "bold"),
        legend.position = "bottom",
        strip.placement = "outside",
        strip.switch.pad.grid = unit(0.1, "cm"))
ggsave("SupplementalFigure_Skewed_Marginal.pdf",sim_plot,
       dpi=600,width=29.5,height=30,units="in")
