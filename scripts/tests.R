# to simulate data with a know correlation
library(mvtnorm)
set.seed(1)

# load new function
source( 'corsym.R' )

# simulate some multivariate normal data that satisfies assumptions
# a decent sample size
n <- 100
# true means will be zero
# this is true covariance
rho <- 0.3
Covar <- matrix( c( 1, rho, rho, 1 ), nrow = 2, ncol = 2 )
# data has two columns, n rows
Q <- rmvnorm( n, sigma = Covar )

# calculate Pearson correlation first, then our estimator
# first test is with random order, where no estimation bias is expected
# both are close to the true value of `rho` above
cor( Q )[1,2] # [1] 0.306412 # not bad
corsym( Q )   # [1] 0.30943  # similar in this case

# now bias the order, which Dashiell found inflated Pearson's estimate
Q_sorted <- t( apply( Q, 1, sort ) )

# estimate now
cor( Q_sorted )[1,2] # [1] 0.6644077 # INFLATED!
corsym( Q_sorted )   # [1] 0.30943   # INVARIANT!
