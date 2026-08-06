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

#Define simulate_cordata, where rho is the correlation to generate the simulated data around and n is the sample size
simulate_cordata <- function(rho, n = 1000) {
    #Define the covariance matrix
    Sigma <- matrix(c(1, rho, rho, 1), nrow = 2)
    #Generate simple multivariate data based on the rho argument centered around a mean of 0
    xy <- mvrnorm(n, mu = c(0, 0), Sigma = Sigma)
    return(data.frame(rho = rho, x = xy[,1], y = xy[,2]))
}

#Define simulate_non_normal_cordata, where rho is the list of correlations to generate the simulated data, n is the sample size, distribution is the kind of distribution you intend to simulate if no shapes are selected and shape1 and shape2 are used to define the shape of the data in the beta probability distribution (alpha and beta, respectively)
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
  #Set seed if seed is chosen
  if (!is.null(seed))
    set.seed(seed)

  #Choose Beta parameters
  #If no shapes were selected, use the predefined distribution parameters instead
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

  #Prepare output df
  out <- vector("list", length(rho))

  #For rhos chosen, create copula (a multivariate distribution with uniform margins separate from marginal distributions) 
  for (i in seq_along(rho)) {
    
    cop <- copula::normalCopula(rho, dim = 2)
    
    #For the copula, create the marginal distribution based on the beta parameters/distribution
    model <- copula::mvdc(
    copula = cop,
    margins = c("beta", "beta"),
    paramMargins = list(
      list(shape1 = shape1, shape2 = shape2),
      list(shape1 = shape1, shape2 = shape2)
    )
    )

    #Generate simulated non-normal data and export and return
    X <- copula::rMvdc(n, model)
    out[[i]] <- data.frame(
    rho = rho[i],
    x = X[,1],
    y = X[,2]
  )
  }

  do.call(rbind, out)
}
