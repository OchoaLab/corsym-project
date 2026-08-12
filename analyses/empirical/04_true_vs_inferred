##This script prepares data for plotting various figures on the empirical dataset.
#This script will output a "Two-way" and "Three-way" analysis dataset with all important information held within

source( "empirical_analysis_functions.R" )
source( 'plotting_functions.R' )

# load libraries
library(tidyr)
library(dplyr)
library(corsym)
library(ggplot2)
library(RColorBrewer)
library(ggh4x)

############################
#########Plot Figure 4
############################

#Read all data
df_all<-read.csv("empirical_data.csv")
df_corr<-read.csv("empirical_correlations.csv")
df_means<-read.csv("empirical_means.csv")

df_corr <- df_corr %>%
  mutate(Rand_type = if_else(grepl("rand", 
                              name_of_P1), "Random", "Raw"),
         Type = if_else(grepl("adm", 
                              name_of_P1), "ADMIXTURE", 
                        ifelse(grepl("anc",name_of_P1),"ANCESTOR","RFMIX")),
         Ref_Num = if_else(grepl("2way", 
                                 name_of_P1), "2way", "3way")
  )

temp<-df_means%>%
pivot_wider(names_from = c(type,stat),values_from = value)
##Plot 4
p_fig4a<-ggplot(temp, aes(x = Delta.adm.YRI_mean, y = Delta.anc.YRI.2way_mean, color = coh)) +
  geom_point(size = 7) +
  geom_errorbar(
    aes(ymin = Delta.anc.YRI.2way_mean - Delta.anc.YRI_var,
        ymax = Delta.anc.YRI.2way_mean + Delta.anc.YRI.2way_var),
    width = 0,show.legend = FALSE
  ) +
  geom_errorbarh(
    aes(xmin = Delta.adm.YRI_mean - Delta.adm.YRI_var,
        xmax = Delta.adm.YRI_mean + Delta.adm.YRI_var),show.legend = FALSE,
    height = 0
  ) +
  labs(x="Mean Difference\n(True Parents, YRI-Like)",y="Mean Difference\n(Inferred Parents, YRI-Like)")+
  #geom_hline(yintercept = 0)+
  #geom_vline(xintercept = 0)+
  theme_professional(base_size = 30)+
  scale_color_discrete(name="Population")+
  geom_abline(slope=1,size=2,linetype="dashed")+
  theme(legend.position = "none",panel.border=element_rect(linewidth = 3))+
  scale_x_continuous(limits = c(0,.2))+
  scale_y_continuous(limits=c(0,.2))

p_fig4b<-df_all%>%
  mutate(Mean_Anc=(P1.anc.YRI.2way+P2.anc.YRI.2way)/2,
         Mean_Adm=(P1.adm.YRI+P2.adm.YRI)/2)%>%
  ggplot(., aes(x = Mean_Adm, y = Mean_Anc, color = coh)) +
  geom_point(size = 7) +
  labs(x="Avg. Ancestry\n(True Parents, YRI-Like)",y="Avg. Ancestry\n(Inferred Parents, YRI-Like)")+
  scale_color_discrete(name="Population")+
  geom_abline(slope=1,size=2,linetype="dashed")+
  theme_professional(base_size = 30)+
  theme(panel.border=element_rect(linewidth = 3))

p_fig4c<-df_corr %>%
  mutate(type_group=case_when(
    Type=="ANCESTOR"&R_type=="corsym"~"Inferred - CorSym",
    Type=="ANCESTOR"&R_type=="pearson"~"Inferred - Pearson",
    Type=="ADMIXTURE"&R_type=="corsym"~"Real Parent - CorSym",
    Type=="ADMIXTURE"&R_type=="pearson"~"Real Parent - Pearson",
    TRUE ~ "NA")
  )%>%
  filter(Ref_Num=="2way" | name_of_P1=="P1.adm.YRI")%>%
  filter(Rand_type!="Random")%>%
  ggplot(., aes(x = Population, y = R,color=type_group)) +
  geom_boxplot(size=3)+
  labs(y="Estimated r\n(YRI-Like)")+
  scale_color_manual(name="Estimation", values=c("#4E79A7","#F28E2B","#59A14F","orchid2"))+
  geom_hline(yintercept = 0,linetype='dashed',linewidth=3,color="black")+
  theme_professional(base_size = 30)+
  theme(legend.position = "bottom",panel.border = element_rect(linewidth=3))+
  geom_errorbar(aes(ymin=CIL,ymax=CIU),
                alpha=.8,size=1.3,width=.2,
                position=position_dodge(width = .75))
p_fig4c

p_fig4<-(p_fig4a+p_fig4b+ plot_layout(widths = c(1,1))) /
  p_fig4c+
  plot_annotation(tag_levels = "A") & theme(
    panel.border=element_rect(linewidth=3),
    plot.tag = element_text(face = 'bold',size=35),
    strip.background = element_rect(linewidth = 3))

ggsave("Figure4.pdf",p_fig4,
       dpi=600,width=29.5,height=23,units="in")
