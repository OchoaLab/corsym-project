##This script prepares data for plotting various figures on the empirical dataset.
#This script will output a "Two-way" and "Three-way" analysis dataset with all important information held within

source( 'empirical_analysis_functions.R' )

# load libraries
library(data.table)
library(purrr)
library(tidyr)
library(dplyr)

remove_D_columns <- function(df) {
  df[, !grepl("^D[0-9]", names(df)), drop = FALSE]
}

#########ADMIXTURE
#Load ADMIXTURE output, combine with parent and fam file info. This is a limited fam file, used only to get the precise IDs per individual. Parent Info is the true Fam information
#'D#' columns are marked for deletion
adm_Q3 <- read.csv("C:/Users/Ictinike/Documents/Labs/GoldbergLab/Papers/admixture_analysis/TRIOS_HGDP_all_chrs_bfile_ldpruned.3.Q",sep=" ",header=FALSE)
fam<-read.csv("C:/Users/Ictinike/Documents/Labs/GoldbergLab/Papers/admixture_analysis/TRIOS_HGDP_all_chrs_bfile_ldpruned.fam",sep="",header=FALSE)
parent_info<-read.csv("C:/Users/Ictinike/Documents/Labs/GoldbergLab/Papers/admixture_analysis/HGDP_with_TGP_trios_merged_ldpruned.fam",sep=" ",header=FALSE)
colnames(parent_info)<-c("coh","id","p1","p2","sex","D1")
parent_info<-remove_D_columns(parent_info)
adm<-cbind(fam,adm_Q3)
colnames(adm)<-c("D1","id_main","D2","D3","D4",'D5',"adm.NAM","adm.IBS","adm.YRI")
adm <- adm %>%
  separate(id_main, into = c("coh", "id"),
           sep = "_",    extra = "merge",    fill = "right"  )
adm<-remove_D_columns(adm)
adm<-adm%>%
  dplyr::select(coh,id,adm.NAM,adm.IBS,adm.YRI)
adm<-merge(adm,parent_info,by=c("coh","id"),all.x = TRUE)

#########RFMIX
#Load RFMIX output
rfm<-read.csv("C:/Users/Ictinike/Documents/Labs/GoldbergLab/Papers/hgdp/three_harmonized_rfmix.csv",
              sep=",",header=TRUE)
rfm <- dcast(as.data.table(rfm),
  individual ~ ancestry,
  value.var = "proportion",
  fill = 0
)
colnames(rfm)<-c("id","rfm.NAM","rfm.IBS","rfm.YRI")
rfm <- rfm %>%
  separate(id,    into = c("coh", "id"),
           sep = "_",    extra = "merge",    fill = "right"  )

#########ANCESTOR
#Load ANCESTOR output
anc<-read.csv("C:/Users/Ictinike/Documents/Labs/GoldbergLab/Papers/hgdp/ancestor_3way_HGDP.txt",sep="",header=FALSE)
colnames(anc)<-c("T1","P1.anc.NAM","P1.anc.IBS","P1.anc.YRI","P2.anc.NAM","P2.anc.IBS","P2.anc.YRI.2")
anc <- anc %>%
  mutate(basename = basename(T1)) %>%
  separate(basename,    into = c("coh", "D1"),
           sep = "_",    extra = "merge",    fill = "right"  )%>%
  separate(D1,    into = c("id", "D2"),    sep = "_",
           extra = "merge",    fill = "right"  )%>%
  dplyr::select(-c(T1,D2))

anc.2way<-read.csv("C:/Users/Ictinike/Documents/Labs/GoldbergLab/Papers/hgdp/ancestor_2way_YRI_v_NonYRI.txt",sep="",header=FALSE)
colnames(anc.2way)<-c("T1","P1.anc.NonYRI.2way","P1.anc.YRI.2way","D1","P2.anc.NonYRI.2way","P2.anc.YRI.2way","D2")
anc.2way <- anc.2way %>%
  mutate(basename = basename(T1)) %>%
  separate(basename,    into = c("coh", "D3"),
           sep = "_",    extra = "merge",    fill = "right"  )%>%
  separate(D3,    into = c("id", "D4"),    sep = "_",
           extra = "merge",    fill = "right"  )%>%
  dplyr::select(-c(T1,D1,D2,D4))

df_anc<-merge(anc,anc.2way,by=c("coh","id"))

#########Combine
df<-merge(adm,rfm,by=c("coh","id"),all = TRUE)
df<-merge(df,df_anc,by=c("coh","id"),all = TRUE)
df$adm.NonYRI<-df$adm.NAM+df$adm.IBS


####################
###Two Way Dataset
####################
#Hold df to reuse
df_2way<-df

#########Extract Two Way Inference Dataset
#This splits parent true ancestry into P1 and P2 in the same way ANCESTOR output is
parent_split_values<-list("sex","adm.NAM","adm.IBS","adm.YRI","adm.NonYRI","rfm.NAM","rfm.IBS","rfm.YRI")
for (pv in parent_split_values){
  df_2way<-df_2way%>%
    left_join(df %>% dplyr::select(id, pv) %>% rename_with(~ paste0("P1.", .x),-id),
              by = c("p1" = "id"))
}
for (pv in parent_split_values){
  df_2way<-df_2way%>%
    left_join(df %>% dplyr::select(id, pv) %>% rename_with(~ paste0("P2.", .x),-id),
              by = c("p2" = "id"))
}

#remove non-children samples
df_2way <- df_2way %>%
  filter(!is.na(p1) & p1!=0)

#########Extract Two Way Correlations
#Randomize two way assignment
child_values_to_grab<-list("anc.NonYRI.2way","anc.YRI.2way")
for (cv in child_values_to_grab){
  P1<-paste0("P1.",cv)
  P2<-paste0("P2.",cv)
  df_2way<-randomize_order_filter(df_2way,P1,P2)
}

#Export File
write.csv(df_2way, file = "2way_analysis.csv", row.names = FALSE,quote=FALSE)

col_pairs <- list(
  c("P1.anc.YRI.2way", "P2.anc.YRI.2way",corsym,"corsym"),
  c("P1.anc.YRI.2way.rand", "P2.anc.YRI.2way.rand",corsym,"corsym"),
  c("P1.anc.YRI.2way", "P2.anc.YRI.2way",pearson,"pearson"),
  c("P1.anc.YRI.2way.rand", "P2.anc.YRI.2way.rand",pearson,"pearson")  
)

df_2way_corr <- do.call(rbind,
                  lapply(split(df_2way, df_2way$coh), function(dat) {
                    
                    do.call(rbind,
                            lapply(col_pairs, function(p)
                              run_pair(dat, p[[1]], p[[2]], p[[3]], p[[4]])
                            )
                    )
                    
                  })
)      

#Export File
write.csv(df_2way_corr, file = "2way_correlations.csv", row.names = FALSE,quote=FALSE)

####################
###Admixture Plot Dataset
####################
adm_Q3 <- read.csv("C:/Users/Ictinike/Documents/Labs/GoldbergLab/Papers/admixture_analysis/TRIOS_HGDP_all_chrs_bfile_ldpruned.3.Q",sep=" ",header=FALSE)
fam<-read.csv("C:/Users/Ictinike/Documents/Labs/GoldbergLab/Papers/admixture_analysis/TRIOS_HGDP_all_chrs_bfile_ldpruned.fam",sep="",header=FALSE)

pop_assign<-fam%>%
  dplyr::select(c(V2))
pop_assign <- pop_assign %>%
  separate(V2,    into = c("coh", "id"),
           sep = "_",    extra = "merge",    fill = "right"  )%>%
  mutate(coh = case_when(
    coh == "HGDP" ~ "NAM",
    TRUE ~ coh
  )
  )
# Define population order 
pop_order <- c("YRI","IBS","NAM",
               "ACB","ASW","CLM","MXL","PEL","PUR")
# Rename ADMIXTURE clusters
colnames(q) <- paste0("K", seq_len(ncol(q)))

q$Population <- factor(
  toupper(pop_assign$coh),
  levels = pop_order
)

pop_meta <- tibble(
  Population = c(
    "YRI","IBS","NAM",
    "ACB","ASW",
    "CLM","MXL","PEL","PUR"
  ),
  Type = c(
    "Reference","Reference","Reference",
    "Admixed Population","Admixed Population",
    "Admixed Population","Admixed Population","Admixed Population","Admixed Population"
  )
)
q <- q %>%
  arrange(
    Population,
    desc(K1),
    desc(K3),
    desc(K2)
  )

q <- q %>%
  left_join(pop_meta, by = "Population")

# Re-number individuals after sorting
q$Individual <- seq_len(nrow(q))

type_order <- c(
  "Reference",
  "Admixed Population"
)
q <- q %>%
  mutate(
    Type = factor(Type, levels = type_order)
  )

q_long <- q %>%
  pivot_longer(
    starts_with("K"),
    names_to = "Cluster",
    values_to = "Ancestry"
  )

#Export File
write.csv(q_long, file = "ADMIXTURE_Plot_data.csv", row.names = FALSE,quote=FALSE)



####################
###Three Way Dataset
####################
