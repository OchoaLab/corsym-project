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

df_corr <- df_corr %>%
  mutate(Type = if_else(grepl("rand", 
                              name_of_P1), "Random", "Raw"),
         Ref_Num = if_else(grepl("2way", 
                              name_of_P1), "2way", "3way"))

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
  theme_professional()+
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

#Plot Fig 2b-d 
p1<-quick_point_base(df_all,x=`P1.anc.YRI.2way`,y=`P2.anc.YRI.2way`,color = coh)+
  labs(y="Inferred P2 (YRI-Like)",col="Population",x="Inferred P1 (YRI-Like)")
p2<-quick_point_base(df_all,`P1.anc.YRI.2way.rand.1`,`P2.anc.YRI.2way.rand.1`,color = coh)+
  labs(y="Random Inferred P2 (YRI-Like)",col="Population",x="Random Inferred P1 (YRI-Like)")
p3<-quick_point_base(df_all,`P1.adm.YRI`,`P2.adm.YRI`,color = coh)+
  labs(y="Mother (YRI-Like)",col="Population",x="Father (YRI-Like)")

#Plot Fig 2e
p_rc_fig2<-df_corr%>%
  filter(R_type=="corsym",Ref_Num=="2way")%>%
  filter(name_of_P1=="P1.anc.YRI.2way" | name_of_P1=="P1.anc.YRI.2way.rand.1")%>%
  select(-c(name_of_P1,name_of_P2,CIU,CIL))%>%
  pivot_wider(names_from = Type,values_from = R)%>%
  ggplot(.,aes(Raw,Random,colour = Population))+
  geom_point(size=6)+
  theme_professional(base_size = 30)+
  geom_abline(intercept = 0,slope=1,linetype="dashed")+
  labs(y=expression(bold(italic("r"['c']) * " (Inferred Randomized)")),col="Population",x=expression(bold(italic("r"['c']) * " (Inferred)")))+
  theme(legend.position = "none",panel.border = element_rect(linewidth = 3),axis.title = element_text(face="bold"))

#Plot Fig2F
x_labels <- df_corr %>%
  filter(Type=="Raw"&!is.na(N))%>%
  mutate(
    label = ifelse(is.na(N),
                   Population,
                   paste0(Population, "\nN=", N))
    
  )

p_fig2_boxplot<-df_corr %>%
  filter(
    !(R_type == "corsym" & Type=="Random"),Ref_Num=="2way"
  )%>%
  mutate(type_group=case_when(
    R_type=="corsym"~"CorSym",
    Type=="Random"&R_type=="pearson"~"Pearson - Data Randomized",
    Type=="Raw"&R_type=="pearson"~"Pearson - Original Data",
    TRUE ~ "NA")
  )%>%
  ggplot(., aes(x = Population, y = R,colour = type_group)) +
  geom_boxplot(size=2)+
  geom_hline(yintercept = 0,linetype='dashed',linewidth=3,color="black")+
  labs(y="Computed r (YRI-Like)")+
  geom_errorbar(
    data = subset(t, type_group %in% c("Pearson - Original Data", "CorSym")),
    aes(ymin = CIL, ymax = CIU), alpha=.8,size=1.3, 
    position = position_dodge(width = 1),   width = 1
  ) +
  geom_hline(yintercept = 0,linetype='dashed',linewidth=3,color="black")+
  labs(y="Computed r (YRI-Like)")+
  scale_x_discrete(
    labels = setNames(x_labels$label, x_labels$Population)
  )+
  scale_color_manual(name="Estimation", values=c("#4E79A7","#F28E2B","#59A14F"))+
  theme_professional(base_size = 30)+
  theme(legend.position = "bottom",panel.border=element_rect(linewidth = 3))

#Organize and save
p4<-(p1|p2|p3)+
  plot_annotation()+
  plot_layout(guides = "collect",ncol = 3,nrow = 1) & 
  theme(legend.position = "bottom",legend.text = element_text(size=30),
        legend.title = element_text(size=30),
        panel.border=element_rect(linewidth=3))

p_fig2<-p_admixture /
  p4 /
  (p_rc_fig2+p_fig2_boxplot+ 
     plot_layout(widths = c(1, 2)))+
  plot_annotation(tag_levels = "A") & theme(
    panel.border=element_rect(linewidth=3),
    plot.tag = element_text(face = 'bold',size=35),
    strip.background = element_rect(linewidth = 3))

ggsave("Figure2.pdf",p_fig2,
       dpi=600,width=29.5,height=29,units="in")
