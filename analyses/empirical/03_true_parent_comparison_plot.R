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
#########Plot Figure 2
############################

#Read all data
df_all<-read.csv("empirical_data.csv")
df_corr<-read.csv("empirical_correlations.csv")
df_means<-read.csv("empirical_means.csv")
df_admix<-read.csv("ADMIXTURE_Plot_data.csv")

#Plotting Figure 3
p_fig3a<-df_all%>%
  select(c(P1.adm.YRI, P2.adm.YRI,coh))%>%
  pivot_longer(
    cols = c(P1.adm.YRI, P2.adm.YRI),
    names_to = "Parent",
    values_to = "value"
  )%>%
  mutate(Parent=case_when(
    Parent=="P1.adm.YRI"~"Father",
    Parent=="P2.adm.YRI"~"Mother",
    TRUE ~ "NA")
  )%>%
  filter(!is.na(value))%>%
  ggplot(.)+
  geom_boxplot(aes(y=`value`,fill=Parent,x=coh,col=after_scale(fill)),size=2,alpha=.5)+
  labs(y="True Ancestry\n(YRI-Like)",x="Population")+
  scale_fill_manual(name="Parent", values=c("#00FFCC","#FF9933"))+
  theme_professional(base_size = 30)+
  theme(legend.position = "bottom",panel.border=element_rect(linewidth = 3))+
  plot_annotation(tag_levels = "A") & theme(legend.position = "bottom",legend.text = element_text(size=30),
                                            legend.title = element_text(size=30),panel.border=element_rect(linewidth=3),
                                            plot.tag = element_text(face = 'bold',size=35))


p_fig3b<-df_all%>%
  select(c(P1.adm.IBS, P2.adm.IBS,coh))%>%
  pivot_longer(
    cols = c(P1.adm.IBS, P2.adm.IBS),
    names_to = "Parent",
    values_to = "value"
  )%>%
  mutate(Parent=case_when(
    Parent=="P1.adm.IBS"~"Father",
    Parent=="P2.adm.IBS"~"Mother",
    TRUE ~ "NA")
  )%>%
  filter(!is.na(value))%>%
  ggplot(.)+
  geom_boxplot(aes(y=`value`,fill=Parent,x=coh,col=after_scale(fill)),size=2,alpha=.5)+
  labs(y="True Ancestry\n(IBS-Like)",x="Population")+
  scale_fill_manual(name="Parent", values=c("#00FFCC","#FF9933"))+
  theme_professional(base_size = 30)+
  theme(legend.position = "bottom",panel.border=element_rect(linewidth = 3))

p_fig3c<-df_all%>%
  select(c(P1.adm.NAM, P2.adm.NAM,coh))%>%
  pivot_longer(
    cols = c(P1.adm.NAM, P2.adm.NAM),
    names_to = "Parent",
    values_to = "value"
  )%>%
  mutate(Parent=case_when(
    Parent=="P1.adm.NAM"~"Father",
    Parent=="P2.adm.NAM"~"Mother",
    TRUE ~ "NA")
  )%>%
  filter(!is.na(value))%>%
  ggplot(.)+
  geom_boxplot(aes(y=`value`,fill=Parent,x=coh,col=after_scale(fill)),size=2,alpha=.5)+
  labs(y="True Ancestry\n(NAM-Like)",x="Population")+
  scale_fill_manual(name="Parent", values=c("#00FFCC","#FF9933"))+
  theme_professional(base_size = 30)+
  theme(legend.position = "bottom",panel.border=element_rect(linewidth = 3))


p_fig3<-(p_fig3a|p_fig3b|p_fig3c)+
  plot_annotation(tag_levels = "A")+
  plot_layout(guides = "collect",ncol = 1,nrow = 3) & theme(legend.position = "bottom",legend.text = element_text(size=30),
                                                            legend.title = element_text(size=30),panel.border=element_rect(linewidth=3),
                                                            plot.tag = element_text(face = 'bold',size=35))

ggsave("Figure3.pdf",p_fig3,
       dpi=600,width=29.5,height=20,units="in")



######Old Tests
ks_test_set<-df2%>%
  select(c(a3.p1, a3.p2,coh))%>%
  pivot_longer(
    cols = c(a3.p1, a3.p2),
    names_to = "Parent",
    values_to = "value"
  )%>%
  mutate(Parent=case_when(
    Parent=="a3.p1"~"Father",
    Parent=="a3.p2"~"Mother",
    TRUE ~ "NA")
  )%>%
  filter(!is.na(value))
for(pop in unique(ks_test_set$coh)){
  ks_test_output<-ks.test(subset(ks_test_set,(coh==pop&Parent=="Father"))$value,
          subset(ks_test_set,(coh==pop&Parent=="Mother"))$value,alternative = "two.sided")
  print(paste0(pop," for YRI had a p-value of: ",ks_test_output$p.value))
}

ks_test_set<-df2%>%
  select(c(a2.p1, a2.p2,coh))%>%
  pivot_longer(
    cols = c(a2.p1, a2.p2),
    names_to = "Parent",
    values_to = "value"
  )%>%
  mutate(Parent=case_when(
    Parent=="a2.p1"~"Father",
    Parent=="a2.p2"~"Mother",
    TRUE ~ "NA")
  )%>%
  filter(!is.na(value))
for(pop in unique(ks_test_set$coh)){
  ks_test_output<-ks.test(subset(ks_test_set,(coh==pop&Parent=="Father"))$value,
          subset(ks_test_set,(coh==pop&Parent=="Mother"))$value,alternative = "two.sided")
  print(paste0(pop," for IBS had a p-value of: ",ks_test_output$p.value))
}

ks_test_set<-df2%>%
  select(c(a1.p1, a1.p2,coh))%>%
  pivot_longer(
    cols = c(a1.p1, a1.p2),
    names_to = "Parent",
    values_to = "value"
  )%>%
  mutate(Parent=case_when(
    Parent=="a1.p1"~"Father",
    Parent=="a1.p2"~"Mother",
    TRUE ~ "NA")
  )%>%
  filter(!is.na(value))
for(pop in unique(ks_test_set$coh)){
  ks_test_output<-ks.test(subset(ks_test_set,(coh==pop&Parent=="Father"))$value,
          subset(ks_test_set,(coh==pop&Parent=="Mother"))$value,alternative = "two.sided")
  print(paste0(pop," for NAM had a p-value of: ",ks_test_output$p.value))
}
0.05/18

p1<-df2%>%
  select(c(a3.p1, a3.p2,coh))%>%
  pivot_longer(
    cols = c(a3.p1, a3.p2),
    names_to = "Parent",
    values_to = "value"
  )%>%
  mutate(Parent=case_when(
    Parent=="a3.p1"~"Father",
    Parent=="a3.p2"~"Mother",
    TRUE ~ "NA")
  )%>%
  filter(!is.na(value))%>%
  ggplot(.)+
  geom_boxplot(aes(y=`value`,fill=Parent,x=coh,col=after_scale(fill)),size=2,alpha=.5)+
  labs(y="True Ancestry\n(YRI-Like)",x="Population")+
  scale_fill_manual(name="Parent", values=c("#00FFCC","#FF9933"))+
  theme_professional(base_size = 30)+
  theme(legend.position = "bottom",panel.border=element_rect(linewidth = 3))

ggplot(df2)+
  geom_histogram(aes(a3.p1),fill="#00FFCC")+
    geom_histogram(aes(a3.p2),fill="#FF9933")+
  facet_wrap(.~coh)
