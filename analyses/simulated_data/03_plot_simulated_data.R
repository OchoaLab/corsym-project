###This script plots simulated data
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

############################
############################
#read all data
plot1a_data<-read.csv("plot1a_data.csv")
plot1b_data<-read.csv("plot1b_data.csv")
plot1c_data<-read.csv("plot1c_data.csv")
plot1d_data<-read.csv("plot1d_data.csv")
plot1a_cor_df<-read.csv("plot1a_cor_df.csv")

p1a<-plot1a_data%>%
  mutate(Rho=rho)%>%
  ggplot(.,aes(x=x,y=y))+
  geom_point()+
  geom_abline(slope=1,intercept = 0,linetype="dashed",linewidth=1.5)+
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "steelblue",
    fill = "grey70",
    linewidth = 1,
    alpha = 0.25)  +
  facet_grid(factor(type,levels = c("Random","Ordered"))~Rho,labeller = labeller(Rho = as_labeller(name_fun), type = label_value))+
  labs(y="Vector 2",x="Vector 1") +
  theme(
    legend.position = "bottom",
    strip.placement = "outside",
    strip.switch.pad.grid = unit(0.1, "cm"))+
  theme_professional(base_size = 30) +
  theme(panel.border = element_rect(linewidth=1.5),
        strip.text = element_text(size=25,
                                  face = "bold"
        ))+
  geom_richtext(data=plot1a_cor_df,
                aes(label = label), x = -Inf,y = Inf,
                fill = scales::alpha("white", 0.8),
                label.color = "grey40",
                hjust = 0,
                vjust = 1,size=11
  )

p1b<-plot1b_data%>%
  pivot_longer(
    cols = c(Pearson, CorSym),
    names_to = "Correlation",
    values_to = "value"
  )%>%
  ggplot(.) +
  geom_point(aes(x=Rho,y=value,col=Correlation),shape=15,size=3,alpha=.5)+
  facet_grid(~Size,labeller = labeller(Size = as_labeller(name_fun)))+
  geom_abline(slope=1,intercept = 0,linetype="dashed",linewidth=4)+
  labs(y="Computed r",x="True Correlation")+
  scale_color_manual(
    name="Estimator",
    values = c("forestgreen","orchid4")#,        # Custom colors
    #labels = c("Ordered", "Random") # Custom labels
  )+
  theme_professional(base_size = 30)+
  theme(legend.position = "bottom",legend.text = element_text(size=27),panel.border = element_rect(linewidth = 4),legend.key.height =  unit(0.2, 'cm'),
        legend.margin = margin(t = -10))+
  guides(color = guide_legend(override.aes = list(size = 10)))

p1c<-ggplot(plot1c_data,aes(x=Size,y=variance_r,col=Rho))+
  geom_point()+
  theme_professional(base_size = 30)+
  scale_colour_viridis_c()+
  geom_point(size = 5) +
  scale_color_gradient2(
    low = "orchid4", 
    mid = "lightgray", 
    high = "darkgreen", 
    midpoint = 0
  )+
  labs(
    x = "Sample Size",
    y = expression(bold(Delta * italic(r) * " Variance")),
    color = "True\nCorrelation"
  ) +
  theme_professional(base_size=30)+
  theme(panel.border = element_rect(linewidth=3))

p1d<-plot1d_data%>%
  ggplot(.,aes(x=Rho,y=Pear,col=factor(Bias))) +
  geom_point()+
  geom_point(shape=15,size=5,alpha=.1)+
  geom_smooth(se = FALSE,linewidth=3)+
  geom_abline(slope=1,intercept = 0,linetype="dashed")+
  labs(
    y="Pearson's r",x="True Correlation")+
  scale_color_discrete(
    name="Ordering \nProportion",labels = scales::percent(as.numeric(levels(factor(plot1d_data$Bias))))
  )+
  theme_professional(base_size = 30)+
  theme(axis.text = element_text(size=22),panel.border = element_rect(linewidth = 3))

p_fig1<-p1a /
  p1b /
  (p1c+p1d+ plot_layout(widths = c(1.2, 1.8)))+
  plot_annotation(tag_levels = "A") & theme(
    panel.border=element_rect(linewidth=3),
    plot.tag = element_text(face = 'bold',size=35),
    strip.background = element_rect(linewidth = 3))

ggsave("Figure1.pdf",p_fig1,
       dpi=600,width=29.5,height=29,units="in")
