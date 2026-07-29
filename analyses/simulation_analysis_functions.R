#
reorder_proportion_p <- function(df, v1, v2, prop = .5) {

  idx <- sample(nrow(df), size = floor(prop * nrow(df)))
  reordered <- slice(df, idx)
  unordered  <- slice(df, -idx)

  reordered<-reordered%>%
    mutate(
      !!paste0(v1,".",as.character(prop)) := case_when(
        .data[[v1]] > .data[[v2]] ~ .data[[v1]],
        .data[[v2]] > .data[[v1]] ~ .data[[v2]],
        .data[[v2]] == .data[[v1]] ~ .data[[v1]]
      ),
      !!paste0(v2,".",as.character(prop)) := case_when(
        .data[[v1]] > .data[[v2]] ~ .data[[v2]],
        .data[[v2]] > .data[[v1]] ~ .data[[v1]],
        .data[[v2]] == .data[[v1]] ~ .data[[v2]]
      )
    )
  unordered<-unordered%>%
    mutate(
      !!paste0(v1,".",as.character(prop)) := .data[[v1]],
      !!paste0(v2,".",as.character(prop)) := .data[[v2]],
      )
  
  df <- bind_rows(reordered, unordered) 
  
  return(df)
}
