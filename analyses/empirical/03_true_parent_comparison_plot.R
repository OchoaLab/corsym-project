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

#Plotting
cols <- colorRampPalette(
  brewer.pal(4, "Accent")
)(length(unique(df_admix$Cluster)))

type_order <- c(
  "Reference",
  "Admixed Population"
)
df_admix <- df_admix %>%
  mutate(
    Type = factor(Type, levels = type_order)
  )

#Plot Fig2a, admixture plot 
p_admixture<-ggplot(df_admix,aes(x = Individual,y = Ancestry,fill = Cluster)) +
  geom_col(width = 1) +
  scale_x_continuous(expand = c(0, 0))+
  theme_professional(base_size=30)+
  facet_nested(
    ~ Type+factor(Population,levels= c(
      "YRI","IBS","NAM",
      "ACB","ASW",
      "CLM","MXL","PEL","PUR"
    )),
    scales = "free_x",
    space = "free_x"
  )+
  theme(legend.position = "none",axis.ticks.x = element_blank(),axis.text.x = element_blank(),
        panel.spacing = unit(0, "lines"),strip.text = element_text(size=25),axis.title = element_text(size=25))
p_admixture

temp<-df_all%>%
  select(c('coh','id','P1.adm.IBS', 'P2.adm.IBS','P1.adm.YRI',
           'P2.adm.YRI','P1.adm.NAM','P2.adm.NAM'))%>%
  pivot_longer(
    cols = -c("id","coh"),
    names_to = c("name"),
    #names_pattern = "(P[12])(.*)"
  )
temp$ref <- substr(temp$name, nchar(temp$name) - 3 + 1, nchar(temp$name))
temp$P_name <- substr(temp$name, 0, 2)

p_fig2b<-temp%>%
  mutate(Parent=case_when(
    P_name=="P1"~"Father",
    P_name=="P2"~"Mother",
    TRUE ~ "NA")
  )%>%
  ggplot(.)+
  geom_boxplot(aes(y=`value`,fill=Parent,x=coh,col=after_scale(fill)),size=2,alpha=.5)+
  labs(y="True Ancestry",x="Population")+
  scale_fill_manual(name="Parent", values=c("#00FFCC","#FF9933"))+
  theme_professional(base_size = 30)+
  theme(legend.position = "bottom",panel.border=element_rect(linewidth = 3),
        panel.spacing = unit(0.3, "lines"))+
  facet_grid(factor(ref,levels = c("YRI","IBS","NAM"))~.,switch = "y")+
  theme(legend.position = "bottom")

p_fig2<-(p_admixture+
           theme(legend.position = "none")
         |p_fig2b)+
  plot_layout(ncol = 1,nrow = 2,heights = c(1,5))+
  plot_annotation(tag_levels = "A",
                  )
ggsave("Fig2b.pdf",p_fig2,
       dpi=600,width=30,height=25,units="in")



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
