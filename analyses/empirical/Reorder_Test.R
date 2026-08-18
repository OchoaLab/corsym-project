##This script prepares data for plotting various figures on the empirical dataset.
#This script will output a "Two-way" and "Three-way" analysis dataset with all important information held within

source( "empirical_analysis_functions.R" )

# load libraries
library(data.table)
library(purrr)
library(tidyr)
library(dplyr)
library(corsym)

remove_D_columns <- function(df) {
  df[, !grepl("^D[0-9]", names(df)), drop = FALSE]
}

#########ADMIXTURE
#Load ADMIXTURE output, combine with parent and fam file info. This is a limited fam file, used only to get the precise IDs per individual. Parent Info is the true Fam information
#'D#' columns are marked for deletion
adm_Q3 <- read.csv("TRIOS_HGDP_all_chrs_bfile_ldpruned.3.Q",sep=" ",header=FALSE)
fam<-read.csv("TRIOS_HGDP_all_chrs_bfile_ldpruned.fam",sep="",header=FALSE)
parent_info<-read.csv("HGDP_with_TGP_trios_merged_ldpruned.fam",sep=" ",header=FALSE)
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
rfm<-read.csv("three_harmonized_rfmix.csv",
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
anc<-read.csv("ancestor_3way_HGDP.txt",sep="",header=FALSE)
anc<-read.csv("ancestor_3way_reorder.txt",sep="",header=FALSE)
colnames(anc)<-c("T1","P1.anc.IBS","P1.anc.YRI","P1.anc.NAM","P2.anc.IBS","P2.anc.YRI","P2.anc.NAM")
anc <- anc %>%
  mutate(basename = basename(T1)) %>%
  separate(basename,    into = c("coh", "D1"),
           sep = "_",    extra = "merge",    fill = "right"  )%>%
  separate(D1,    into = c("id", "D2"),    sep = "_",
           extra = "merge",    fill = "right"  )%>%
  dplyr::select(-c(T1,D2))

anc.2way<-read.csv("ancestor_2way_YRI_v_NonYRI.txt",sep="",header=FALSE)
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
###Empirical Dataset
####################
#Hold df to reuse
df_filter<-df

#########Extract Inference Dataset
#This splits parent true ancestry into P1 and P2 in the same way ANCESTOR output is
parent_split_values<-list("sex","adm.NAM","adm.IBS","adm.YRI","adm.NonYRI","rfm.NAM","rfm.IBS","rfm.YRI")
for (pv in parent_split_values){
  df_filter<-df_filter%>%
    left_join(df %>% dplyr::select(id, pv) %>% rename_with(~ paste0("P1.", .x),-id),
              by = c("p1" = "id"))%>%
    left_join(df %>% dplyr::select(id, pv) %>% rename_with(~ paste0("P2.", .x),-id),
              by = c("p2" = "id"))
}

#remove non-children samples
df_filter <- df_filter %>%
  filter(!is.na(p1) & p1!=0)

###############
####Means and Deltas
###############

#########Extract Delta and Mean Values
P1_cols <- colnames(df_filter)[grep("^P1", colnames(df_filter))]
P2_cols <- colnames(df_filter)[grep("^P2", colnames(df_filter))]

#Get the Delta between P1 and P2
df_filter[paste0("Delta.", sub("^P1.", "", P1_cols))] <-
  abs(df_filter[P1_cols] - df_filter[P2_cols])
#Get the Mean between P1 and P2
df_filter[paste0("Mean.", sub("^P1.", "", P1_cols))] <-
  abs(df_filter[P1_cols] + df_filter[P2_cols])/2

#Summarize and pull mean values
means_df_filter <- df_filter %>%
  group_by(coh)%>%
  summarise(
    across(
      matches("^(Delta.|Mean.)"),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        var  = ~ var(.x, na.rm = TRUE)
      )
    )
  )%>%
  pivot_longer(
    cols = -coh,
    names_to = c("type", "stat"),
    names_sep = "_(?=[^_]+$)",
    values_to = "value"
  )

#Export File
write.csv(means_df_filter, file = "empirical_means2.csv", row.names = FALSE,quote=FALSE)

####################
###Extract Correlations
####################
child_value_to_randomize<-"anc.YRI.2way"

#Go through 100 repetitions of randomizing order
for (rep in 1:100){
  P1<-paste0("P1.",child_value_to_randomize)
  P2<-paste0("P2.",child_value_to_randomize)
  df_filter<-randomize_order_filter(df_filter,P1,P2,n=rep)
}

#Create list of lists that has all values to pull for correlations
value_type <- c("anc.YRI", "anc.IBS", "anc.NAM","anc.YRI.2way",
               "adm.YRI", "adm.IBS", "adm.NAM")
functions <- list(corsym, pearson)
function_info <- data.frame(
  fun_name = c("corsym", "pearson"),
  stringsAsFactors = FALSE
)
function_info$fun <- functions

#Pull possible combinations of lists togther
combinations <- expand.grid(
  suffix = value_type,
  fun = seq_len(nrow(function_info)),
  stringsAsFactors = FALSE
)

#Apply the combinations into a new list called col_pairs
col_pairs <- lapply(seq_len(nrow(combinations)), function(i) {
  f <- function_info[combinations$fun[i], ]
  list(
    paste0("P1.", combinations$suffix[i]),
    paste0("P2.", combinations$suffix[i]),
    f$fun[[1]],
    f$fun_name
  )
})

#Add randomized columns as well
for (i in 1:100) {
  add_cor <- c(
    paste0(c("P1.anc.YRI.2way.rand", "P2.anc.YRI.2way.rand"), ".", i),
    c(corsym, "corsym")
  )
  col_pairs[[length(col_pairs) + 1]] <- add_cor
  add_pear <- c(
    paste0(c("P1.anc.YRI.2way.rand", "P2.anc.YRI.2way.rand"), ".", i),
    c(pearson, "pearson")
  )
  col_pairs[[length(col_pairs) + 1]] <- add_pear
}

#Calculate all correlations
df_filter_corr <- do.call(rbind,
                  lapply(split(df_filter, df_filter$coh), function(dat) {
                    
                    do.call(rbind,
                            lapply(col_pairs, function(p)
                              run_pair(dat, p[[1]], p[[2]], p[[3]], p[[4]])
                            )
                    )
                    
                  })
)      

#Export Files
write.csv(df_filter, file = "empirical_data2.csv", row.names = FALSE,quote=FALSE)
write.csv(df_filter_corr, file = "empirical_correlations2.csv", row.names = FALSE,quote=FALSE)

df_all<-read.csv("empirical_data2.csv")
df_means<-read.csv("empirical_means2.csv")

p1<-quick_point_base(df_all,P1.anc.IBS,P2.anc.IBS,color = coh,point_size = 5)+
  labs(y="Inferred P2 (IBS-Like)",col="Population",x="Inferred P1 (IBS-Like)")+
  theme(legend.position = "none")
p2<-quick_point_base(df_all,P1.anc.NAM,P2.anc.NAM,color = coh,point_size = 5)+
  labs(y="Inferred P2 (NAM-Like)",col="Population",x="Inferred P1 (NAM-Like)")+
  theme(legend.position = "none")
p3<-quick_point_base(df_all,`P1.anc.YRI`,`P2.anc.YRI`,color = coh,point_size = 5)+
  labs(y="Inferred P2 (YRI-Like)",col="Population",x="Inferred P1 (YRI-Like)")+
  theme(legend.position = "none")

temp<-df_means%>%
  pivot_wider(names_from = c(type,stat),values_from = value)
error_bars <- function(value, variance, direction = "y", width = 0) {
  
  if (direction == "y") {
    geom_errorbar(
      aes(
        ymin = {{ value }} - {{ variance }},
        ymax = {{ value }} + {{ variance }}
      ),
      width = width
    )
    
  } else if (direction == "x") {
    geom_errorbarh(
      aes(
        xmin = {{ value }} - {{ variance }},
        xmax = {{ value }} + {{ variance }}
      ),
      height = width
    )
  }
}


p4<-quick_point_base(temp,Delta.adm.IBS_mean,Delta.anc.IBS_mean,color = coh,point_size = 5)+
  error_bars(Delta.adm.IBS_mean,Delta.adm.IBS_var,direction = "x")+
  error_bars(Delta.anc.IBS_mean,Delta.anc.IBS_var,direction = "y")+
  labs(x="Mean Difference\n(True Parents, IBS-Like)",y="Mean Difference\n(Inferred Parents, IBS-Like)",color="Population")+
  theme(legend.position = "none")+
  scale_x_continuous(limits = c(0,.3))+scale_y_continuous(limits=c(0,.3))+
  guides(color="none")
p5<-quick_point_base(temp,Delta.adm.NAM_mean,Delta.anc.NAM_mean,color = coh,point_size = 5)+
  error_bars(Delta.adm.NAM_mean,Delta.adm.NAM_var,direction = "x")+
  error_bars(Delta.anc.NAM_mean,Delta.anc.NAM_var,direction = "y")+
  labs(x="Mean Difference\n(True Parents, NAM-Like)",y="Mean Difference\n(Inferred Parents, NAM-Like)",color="Population")+
  theme(legend.position = "none")+
  scale_x_continuous(limits = c(0,.3))+scale_y_continuous(limits=c(0,.3))+
  guides(color="none")
p6<-quick_point_base(temp,Delta.adm.YRI_mean,Delta.anc.YRI_mean,color = coh,point_size = 5)+
  error_bars(Delta.adm.YRI_mean,Delta.adm.YRI_var,direction = "x")+
  error_bars(Delta.anc.YRI_mean,Delta.anc.YRI_var,direction = "y")+
  labs(x="Mean Difference\n(True Parents, YRI-Like)",y="Mean Difference\n(Inferred Parents, YRI-Like)",color="Population")+
  theme(legend.position = "none")+
  scale_x_continuous(limits = c(0,.3))+
  scale_y_continuous(limits=c(0,.3))+
  guides(color="none")

p7<-(p3|p6|p1|p4|p2|p5)+
  plot_annotation(tag_levels = "A")+
  plot_layout(guides = "collect",ncol = 2,nrow = 3) & theme(legend.position = "bottom",legend.text = element_text(size=30),
                                                            legend.title = element_text(size=30),
                                                            panel.border=element_rect(linewidth=3),
                                                            plot.tag = element_text(face = 'bold',size=35))

ggsave("Supplemental_Average_Comparison_With_reorder.pdf",p7,
       dpi=600,width=20,height=25,units="in")

order_test_stats(df_all,"P1.anc.NAM","P2.anc.NAM","coh")
order_test_stats(df_all,"P1.anc.IBS","P2.anc.IBS","coh")
order_test_stats(df_all,"P1.anc.YRI","P2.anc.YRI","coh")
