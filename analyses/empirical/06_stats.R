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
order_test_stats<-function(df,col1,col2,group){
  df<-df %>%
    group_by( !!as.symbol(group) ) %>%
    dplyr::summarise(
      x = as.integer((sum(!!as.symbol(col1)>!!as.symbol(col2))+
                        sum(!!as.symbol(col1)==!!as.symbol(col2))*.5)),
      n = n()
    )%>%
    bind_rows(summarise(., across(where(is.numeric), sum),
                        across(where(is.character), ~'Total')))
  
  return(order_bias_test(df))
}

order_test_stats(df_all,"P1.adm.NAM","P2.adm.NAM","coh")
order_test_stats(df_all,"P1.adm.IBS","P2.adm.IBS","coh")
order_test_stats(df_all,"P1.adm.YRI","P2.adm.YRI","coh")

order_test_stats(df_all,"P1.anc.YRI.2way","P2.anc.YRI.2way","coh")
order_test_stats(df_all,"P1.anc.YRI","P2.anc.YRI","coh")
order_test_stats(df_all,"P1.anc.IBS","P2.anc.IBS","coh")
order_test_stats(df_all,"P1.anc.NAM","P2.anc.NAM","coh")

quick_correlations<-function(df,col1,col2,group){
  df<-df %>%
    group_by( !!as.symbol(group) ) %>%
    dplyr::summarise(
      cor_r = corsym(!!as.symbol(col1),!!as.symbol(col2))[1],
      pear_r = pearson(!!as.symbol(col1),!!as.symbol(col2))[1]
    )%>%
    mutate(
      t=pear_r-cor_r,
    )
  
  return(df)
}

quick_correlations(df_all,"P1.anc.YRI","P2.anc.YRI","coh")
quick_correlations(df_all,"P1.anc.IBS","P2.anc.IBS","coh")
quick_correlations(df_all,"P1.anc.NAM","P2.anc.NAM","coh")
