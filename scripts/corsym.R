# implementation of the method proposed in this paper

# TODO: do we need to handle missingness?
corsym <- function( Q, q2 = NULL, alpha = 0.05 ) {
    # validate inputs
    if ( missing( Q ) )
        stop( '`Q` is required!' )
    if ( is.null( q2 ) ) {
        # in this mode, Q must be a matrix with 2 columns
        if ( !is.matrix( Q ) )
            stop( 'When a single argument `Q` is provided, it must be a matrix!' )
        if ( ncol( Q ) != 2 )
            stop( '`Q` must have 2 columns!' )
        # extract the two vectors of interest
        q1 <- Q[ , 1 ]
        q2 <- Q[ , 2 ]
        n <- nrow( Q )
    } else {
        # in this case both need to be vectors of equal length
        q1 <- Q # rename internally
        # make sure length is equal
        n <- length( q1 )
        if( length( q2 ) != n )
            stop( 'Both inputs must have the same length!' )
    }
    
    # n only appears as 2*n...
    nn <- 2 * n
    
    # estimate pooled mean
    # this uses a bit less memory than concatenating q1 and q2 first
    mu <- ( mean( q1 ) + mean( q2 ) ) / 2
    # now estimate "base" pooled variance
    sigma_sq <- ( sum( ( q1 - mu )^2 ) + sum( ( q2 - mu )^2 ) ) / ( nn - 1 )
    # and "base" covariance estimate
    covar <- sum( ( q1 - mu ) * ( q2 - mu ) ) * 2 / ( nn - 1 )

    # now form small sample unbiased estimates
    an <- ( nn - 1 ) ^2 / ( nn * ( nn - 2 ) )
    bn <- an / ( nn - 1 )
    sigma_sq_u <- an * sigma_sq + bn * covar
    covar_u <- an * covar + bn * sigma_sq

    # finally, estimate correlation and return!
    r <- covar_u / sigma_sq_u

    # use Fisher's transformation to estimate confidence intervals
    z <- ( log( 1 + r ) - log( 1 - r ) ) / 2
    rad <- stats::qnorm( alpha / 2, lower.tail = FALSE ) / sqrt( n - 3 )
    # alpha confidence intervals
    CIL <- tanh(z - rad)
    CIU <- tanh(z + rad)
    
    return( c(r, CIL, CIU) )
}
