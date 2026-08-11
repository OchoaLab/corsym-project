#Simple scripts designed to run packages from R package CorSym for easier output

#A script designed to run a number of column pairs using the CorSym package's corsym and pearson functions, extract and return
run_pair <- function(dat, col1, col2, fun, name) {
  
  vals <- fun(dat[[col1]], dat[[col2]])
  
  cbind(
    data.frame(
      Population = dat$coh[1],
      name_of_P1 = col1,
      name_of_P2 = col2,
      R = vals[1],
      CIL = vals[2],
      CIU = vals[3],      
      N = nrow(dat),
      R_type = paste0(name),
      row.names = NULL
    )
  )
}

#Small Function to ensure randomize_order works correctly
randomize_order_filter <- function(df, col1, col2,n=1){
  df_randomize <- as.data.frame(randomize_order(as.matrix(df[c(col1,col2)])))
  colnames(df_randomize)<-c(paste0(col1,".rand.",n),paste0(col2,".rand.",n))
  df<-cbind(df,df_randomize)
  return(df)
}
