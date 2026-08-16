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
    )
order_bias_test(ibs)

nam<-df_all %>%
  group_by(coh) %>%
  dplyr::summarise(
    x = sum(P1.adm.NAM > P2.adm.NAM),
    n = n()
    )
order_bias_test(nam)

yri<-df_all %>%
  group_by(coh) %>%
  dplyr::summarise(
    x = sum(P1.adm.yri > P2.adm.yri),
    n = n()
    )
order_bias_test(yri)

