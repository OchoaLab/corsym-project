# test adapted formulas that work better for smaller sample sizes
# previous CI tests showed practically no dependence on rho, so we can use independent data here (or any other rho)

library(mvtnorm)
library(tidyverse)
library(ochoalabtools)
library(corsym)

# simulate some multivariate normal data that satisfies assumptions
# true means will be zero

# this is the default, but we need it for plots and just have for clarity
alpha <- 0.05

# simulate many, many reps, so our CI coverage estimates are more accurate
n_rep <- 1000000

# sample sizes comparable to the real data
n <- 10
rho <- 0
# what we really want to test, the variance offsets
# 3 is good if not too big, we want to consider smaller values
# for bigger n this tiny offset becomes negligible, so it really only matters at the small sample size of interest
offsets <- (0:6)/2
n_offsets <- length( offsets )

# this is true covariance
Covar <- matrix( c( 1, rho, rho, 1 ), nrow = 2, ncol = 2 )

# initialize data structure
datac <- vector( 'list', n_offsets )
for ( i in 1 : n_offsets )
    datac[[ i ]] <- matrix( NA, n_rep, 3 )

# simulate replicates
for ( rep in 1 : n_rep ) {
    # data has two columns, n rows
    Q <- rmvnorm( n, sigma = Covar )
    for ( i in 1 : n_offsets )
        datac[[ i ]][ rep, ] <- corsym( Q, alpha = alpha, ci_offset = offsets[ i ] )
}

# calculate final coverage data
data <- NULL
for ( i in 1 : n_offsets ) {
    datac_i <- datac[[ i ]]
    colnames( datac_i ) <- c( 'r', 'CIL', 'CIU' )
    datac_i <- as_tibble( datac_i )

    # confirm frequency in which CIs cover real value
    data_i <- tibble(
        offset = offsets[ i ],
        eCI = datac_i %>% filter( CIL < rho, rho < CIU ) %>% nrow() / n_rep
    )
    data <- bind_rows( data, data_i )
}

# calculate absolute errors for easier reading
data <- data %>% mutate( err = abs( eCI - (1 - alpha) ) )

# save results
write_tsv( data, 'CI-test-offset.txt.gz' )

# this data is too dumb for a plot
