###This script holds three of the primary simulation functions for simulation analyses
#reorder_proportion_p(): a function that reorders data based on a designated proportion to bias datasets
#simulate_cordata(): a function designed to generate correlated multivariate data based on a selected rho value using R package mvtnorm
#simulate_cordata(): a function designed to generate correlated multivariate non-normal data based on a selected rho value using R package copula

#Load required libraries
library(MASS)
library(copula)

#Defined reorder_prop function, where df is the dataframe to edit, v1 is vector x, v2 is vector y, and prop is the proportion of samples to reorder on
reorder_prop <- function(df, v1, v2, prop = .5) {

  #Sample rows based on proportion and slice data into the set to be reordered and the set to remain the same
  idx <- sample(nrow(df), size = floor(prop * nrow(df)))
  reordered <- slice(df, idx)
  unordered  <- slice(df, -idx)

  #For data to be reordered, switch v1 and v2 and create new variables named [[v1]].[[Proportion]] and [[v2]].[[Proportion]], with the reordered samples  
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
  #For data that remains unordered, create new variables named [[v1]].[[Proportion]] and [[v2]].[[Proportion]], where v1 stays as the new [[v1]].[[Proportion]] and v2 remains [[v2]].[[Proportion]]   
  unordered<-unordered%>%
    mutate(
      !!paste0(v1,".",as.character(prop)) := .data[[v1]],
      !!paste0(v2,".",as.character(prop)) := .data[[v2]],
      )
  
  df <- bind_rows(reordered, unordered) 
  
  return(df)
}

simulate_cordata <- function(rho, n = 1000) {
  if (abs(rho) == 1) {
    # Handle degenerate cases
    x <- rnorm(n)
    y <- if (rho > 0) x else -x
    return(data.frame(x = x, y = y))
  } else {
    Sigma <- matrix(c(1, rho, rho, 1), nrow = 2)
    xy <- mvrnorm(n, mu = c(0, 0), Sigma = Sigma)
    return(data.frame(x = xy[,1], y = xy[,2]))
  }
}

simulate_non_normal_cordata <- function(rho,
                                n=1000,
                                distribution = c("slightly_skewed",
                                                 "highly_skewed",
                                                 "uniform",
                                                 "symmetric",
                                                 "u_shaped"),
                                shape1 = NULL,
                                shape2 = NULL,
                                seed = NULL) {
  n=1000

  if (!is.null(seed))
    set.seed(seed)

  # Choose Beta parameters
  if (is.null(shape1) || is.null(shape2)) {

    distribution <- match.arg(distribution)

    params <- switch(
      distribution,
      uniform           = c(1, 1),
      slightly_skewed   = c(2, 5),
      highly_skewed     = c(1, 8),
      symmetric         = c(2, 2),
      u_shaped          = c(0.5, 0.5)
    )

    shape1 <- params[1]
    shape2 <- params[2]
  }

  out <- vector("list", length(rho))

  for (i in seq_along(rho)) {

  cop <- copula::normalCopula(rho, dim = 2)

  model <- copula::mvdc(
   copula = cop,
    margins = c("beta", "beta"),
    paramMargins = list(
     list(shape1 = shape1, shape2 = shape2),
     list(shape1 = shape1, shape2 = shape2)
   )
  )

X <- copula::rMvdc(n, model)

    out[[i]] <- data.frame(
      rho = rho[i],
      x = X[,1],
      y = X[,2]
    )
  }

  do.call(rbind, out)
}
