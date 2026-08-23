# this script tests empirical CI coverage using simulations
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
ns <- c(10, 20, 30, 50, 100, 1000)
# to use subsampling approach
n_max <- max( ns )
# length of n vector
n_n <- length( ns )
# and correlations in the full range, like the other simulations
# except no 1 and -1, their CIs never cover the real value
rhos <- ( -9 : 9 ) / 10

data <- NULL
for ( rho in rhos ) {
    message( 'rho = ', rho )
    # this is true covariance
    Covar <- matrix( c( 1, rho, rho, 1 ), nrow = 2, ncol = 2 )
    # to save time across huge numbers of replicates, pre-process this matrix now
    # (code adapted from mvtnorm::rmvnorm)
    ev <- eigen(Covar, symmetric = TRUE)
    R <- t(ev$vectors %*% (t(ev$vectors) * sqrt(pmax(ev$values, 0))))
    
    # initialize data structure for this round of tests
    data_nn <- vector( 'list', n_n )
    for ( i in 1 : n_n )
        data_nn[[ i ]] <- matrix( NA, n_rep, 3 )
    for ( rep in 1 : n_rep ) {
        # data has two columns, n_max rows
        # (code adapted from mvtnorm::rmvnorm)
        Q <- matrix( rnorm( 2 * n_max ), nrow = n_max ) %*% R
        
        # get estimates for nested subsamples of the biggest simulation
        for ( i in 1 : n_n )
            data_nn[[ i ]][ rep, ] <- corsym( Q[ 1 : ns[ i ], ], alpha = alpha )
    }

    for ( i in 1 : n_n ) {
        # make data a bit nicer
        data_n <- data_nn[[ i ]]
        colnames( data_n ) <- c( 'r', 'CIL', 'CIU' )
        data_n <- as_tibble( data_n )

        # confirm frequency in which CIs cover real value
        data_i <- tibble(
            n = ns[ i ],
            rho = rho,
            eCI = data_n %>% filter( CIL < rho, rho < CIU ) %>% nrow() / n_rep
        )
        data <- bind_rows( data, data_i )
    }
}

# save data to replot later if needed
write_tsv( data, 'CI-test.txt.gz' )

fig_start( 'CI-test', wh = fig_scale( 1.2 ) )
ggplot( data, aes( x = factor( n ), y = rho, fill = eCI ) ) +
    geom_tile() +
    theme_classic() +
    scale_fill_gradient2(
        low = "blue", 
        mid = "white", 
        high = "red", 
        midpoint = 1 - alpha
    ) +
    labs( x = 'Sample size', y = 'True correlation', fill = 'CI coverage' )
fig_end()
