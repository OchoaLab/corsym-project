###This script holds three of the primary simulation functions for simulation analyses
#reorder_proportion_p(): a function that reorders data based on a designated proportion to bias datasets
#simulate_cordata(): a function designed to generate correlated multivariate data based on a selected rho value using R package mvtnorm
#simulate_cordata(): a function designed to generate correlated multivariate non-normal data based on a selected rho value using R package copula

#Load required libraries
library(MASS)
library(copula)
librar(corsym)

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

#Small Function to ensure reorder works correctly
reorder_xy <- function(df, prop=1){
  df_reorder <- as.data.frame(partial_order(as.matrix(df[c("x","y")]),q=prop))
  colnames(df_reorder)<-c("x","y")
  return(df_reorder)
}

#Small Function to ensure reorder works correctly
reorder_xy_rename <- function(df, prop=1){
  df_reorder <- as.data.frame(partial_order(as.matrix(df[c("x","y")]),q=prop))
  colnames(df_reorder)<-c(paste0("x.",prop),paste0("y.",prop))
  df<-cbind(df,df_reorder)
  return(df)
}

