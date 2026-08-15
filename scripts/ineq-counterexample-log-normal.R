# when distribution is symmetric, or more generally when sigma_l == sigma_h, the inequality is proven.
# also when at least rho_lh > 0, for any sign of rho.
# so we need to test both negative cases and sigma_l != sigma_h.
# hopefully log-MVN provides insight
# yes, this shows that inequality is not guaranteed in that missing case!

# to simulate data with a know correlation
library(mvtnorm)
library(corsym)
library(tidyverse)
library(ochoalabtools)

# simulate some multivariate normal data that satisfies assumptions
# a decent sample size
n <- 10000
# true means will be zero

# this is true covariance
rhos <- ( (-100) : 100 ) / 100

data <- NULL
for ( rho in rhos ) {
    Covar <- matrix( c( 1, rho, rho, 1 ), nrow = 2, ncol = 2 )
    X <- exp( rmvnorm( n, sigma = Covar ) )
    Xe <- partial_order( X )

    data_i <- tibble(
        rho = rho,
        mu = mean( X ),
        sigma2 = var( as.numeric( X ) ),
        mul = mean( Xe[,1] ),
        muh = mean( Xe[,2] ),
        sigma2l = var( Xe[,1] ),
        sigma2h = var( Xe[,2] ),
        rhor = corsym( X )[1],
        rhoe = pearson( Xe )[1]
    )
    data <- bind_rows( data, data_i )
}

# save data
write_tsv( data, 'ineq-counterexample-log-normal.txt.gz' )

# main result
fig_start( 'ineq-counterexample-log-normal', wh = fig_scale(1) )
ggplot( data, aes( x = rhor, y = rhoe ) ) +
    geom_point() +
    theme_classic() +
    geom_abline( linetype = 'dashed', color = 'gray' ) +
    geom_hline( yintercept = 0, linetype = 'dashed', color = 'gray' ) + 
    geom_vline( xintercept = 0, linetype = 'dashed', color = 'gray' ) + 
    labs( x = 'CorSym estimate', y = 'Pearson estimate, extreme order' )
fig_end()
