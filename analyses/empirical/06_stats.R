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

#Read all data
df_all<-read.csv("empirical_data.csv")
df_corr<-read.csv("empirical_correlations.csv")
df_means<-read.csv("empirical_means.csv")
df_admix<-read.csv("ADMIXTURE_Plot_data.csv")


ibs<-df_all %>%
  group_by(coh) %>%
  dplyr::summarise(
    x = sum(P1.adm.IBS > P2.adm.IBS),
    n = n()
  )%>%
  bind_rows(summarise(., across(where(is.numeric), sum),
                      across(where(is.character), ~'Total')))
nam<-df_all %>%
  group_by(coh) %>%
  dplyr::summarise(
    x = sum(P1.adm.NAM > P2.adm.NAM),
    n = n()
  )%>%
  bind_rows(summarise(., across(where(is.numeric), sum),
                      across(where(is.character), ~'Total')))
yri<-df_all %>%
  group_by(coh) %>%
  dplyr::summarise(
    x = sum(P1.adm.YRI > P2.adm.YRI),
    n = n()
  )%>%
  bind_rows(summarise(., across(where(is.numeric), sum),
                      across(where(is.character), ~'Total')))

order_bias_test(yri)
order_bias_test(nam)
order_bias_test(ibs)

