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
###Table Functions
three_Way_tables<-function(df,col_interior){
  col_suffix<-c("YRI","IBS","NAM")
  P1<-paste0("P1", col_interior, col_suffix)
  P2<-paste0("P2", col_interior, col_suffix)

  order_test<-list()
  for (i in 1:length(P1)){
    temp<-order_test_stats(df,P1[[i]],P2[[i]],"coh")
    if (i > 1) {
      temp <- temp %>% select(-coh,-n)
    }
    
    order_test[[i]]<-temp
  }
  return(bind_cols(order_test))
}
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
three_Way_cor_tables<-function(df,col_interior){
  col_suffix<-c("YRI","IBS","NAM")
  P1<-paste0("P1", col_interior, col_suffix)
  P2<-paste0("P2", col_interior, col_suffix)
  
  cor_test<-list()
  for (i in 1:length(P1)){
    temp<-quick_correlations(df,P1[[i]],P2[[i]],"coh")
    if (i > 1) {
      temp <- temp %>% select(-coh)
    }
    cor_test[[i]]<-temp
  }
  return(bind_cols(cor_test))
}

df_corr <- df_corr %>%
  mutate(Rand_type = if_else(grepl("rand", 
                                   name_of_P1), "Random", "Raw"),
         Type = if_else(grepl("adm", 
                              name_of_P1), "ADMIXTURE", 
                        ifelse(grepl("anc",name_of_P1),"ANCESTOR","RFMIX")),
         Ref_Num = if_else(grepl("2way", 
                                 name_of_P1), "2way", "3way")
  )
###ADM
adm_table<-three_Way_tables(df_all,".adm.")
adm_table<-adm_table%>%
  mutate(
    across(c(4:5,7:8,10:11),~ round(.x,3)),
    across(c(2:4,6:7,9:10),~as.character(.x)),
    across(c(5,8,10), ~ 
             ifelse(.x < 0.0028, paste0(.x, "*"), as.character(.x))
    ))
adm_table<-adm_table[c(1,3,2,4,5,6,7,8,9,10,11)]  
write_tsv(adm_table, '3way_adm.txt' )

###ANC
adm_table<-three_Way_tables(df_all,".anc.")
adm_table<-adm_table%>%
  mutate(
    across(c(4,7,10),~ round(.x,3)),
    across(c(2:4,6:7,9:10),~as.character(.x)),
    across(c(5,8,11), ~ 
             ifelse(.x < 0.00238, paste0(format(.x, 
                                                scientific=TRUE,
                                                digits=3,
                                                trim=TRUE),"*"), 
                    as.character(round(.x,3)))
    ))
adm_table<-adm_table[c(1,3,2,4,5,6,7,8,9,10,11)]  
write_tsv(adm_table, '3way_anc.txt' )

#########Corr Tables
cors_anc<-three_Way_cor_tables(df_all,".anc.")%>%
  mutate(
    across(c(2,3,4,5,6,7,8,9,10),~ round(.x,3))
  )
write_tsv(cors_anc, '3way_anc_corrs.txt' )
cors_adm<-three_Way_cor_tables(df_all,".adm.")%>%
  mutate(
    across(c(2,3,4,5,6,7,8,9,10),~ round(.x,3))
  )
write_tsv(cors_adm, '3way_adm_corrs.txt' )



##############
#Two Way Tables
left<-order_test_stats(df_all,"P1.anc.YRI.2way","P2.anc.YRI.2way","coh")%>%
  mutate(
    across(c(4),~ round(.x,3)),
    across(c(5), ~ 
             ifelse(.x < 0.00238, paste0(format(.x, 
                                         scientific=TRUE,
                                         digits=3,
                                         trim=TRUE),"*"), 
                    as.character(round(.x,3)))
    ),
    across(everything(), as.character))
cors<-quick_correlations(df_all,"P1.anc.YRI.2way","P2.anc.YRI.2way","coh")%>%
  mutate(
    across(c(2:4),~ round(.x,3))
    )
random<-df_corr%>%
  filter(.,Ref_Num=="2way"&
           Rand_type=="Random"&
           Type=="ANCESTOR"&
           R_type=="pearson")%>%
  group_by(Population)%>%
  summarise(
    rp_rand=round(mean(R),3)
  )
right<-cbind(cors,random[-1])
right<-right[c(2,5,3,4)]
blank_total<-rep("-",4)
right<-rbind(right,blank_total)
anc_2way<-cbind(left[-2],right)
write_tsv(anc_2way, '2way_anc.txt' )





#####OLD
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
