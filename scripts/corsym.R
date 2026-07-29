# implementation of the method proposed in this paper

# TODO: do we need to handle missingness?
corsym <- function( x, y = NULL, alpha = 0.05, ci_offset = 1.5 ) {
    # normalize inputs
    obj <- handle_args( x, y )
    x <- obj$x
    y <- obj$y
    n <- obj$n
    
    # n only appears as 2*n...
    nn <- 2 * n
    
    # estimate pooled mean
    # this uses a bit less memory than concatenating x and y first
    mu <- ( mean( x ) + mean( y ) ) / 2
    # now estimate "base" pooled variance
    sigma_sq <- ( sum( ( x - mu )^2 ) + sum( ( y - mu )^2 ) ) / ( nn - 1 )
    # and "base" covariance estimate
    covar <- sum( ( x - mu ) * ( y - mu ) ) * 2 / ( nn - 1 )

    # now form small sample unbiased estimates
    an <- ( nn - 1 ) ^2 / ( nn * ( nn - 2 ) )
    bn <- an / ( nn - 1 )
    sigma_sq_u <- an * sigma_sq + bn * covar
    covar_u <- an * covar + bn * sigma_sq

    # finally, estimate correlation and return!
    r <- covar_u / sigma_sq_u
    # use external function to add CIs
    return( add_cor_CI( r, n, alpha = alpha, ci_offset = ci_offset ) )
}

# same interface as corsym, adds CIs unlike default `cor`
pearson <- function( x, y = NULL, alpha = 0.05, ci_offset = 3 ) {
    # normalize inputs
    obj <- handle_args( x, y )
    x <- obj$x
    y <- obj$y
    n <- obj$n

    # calculate default correlation
    r <- cor( x, y )
    # use external function to add CIs
    return( add_cor_CI( r, n, alpha = alpha, ci_offset = ci_offset ) )
}

# shared by corsym and pearson
handle_args <- function( x, y = NULL ) {
    # validate inputs
    if ( missing( x ) )
        stop( '`x` is required!' )
    if ( is.null( y ) ) {
        # in this mode, x must be a matrix with 2 columns
        if ( !is.matrix( x ) )
            stop( 'When a single argument `x` is provided, it must be a matrix!' )
        if ( ncol( x ) != 2 )
            stop( '`x` must have 2 columns!' )
        n <- nrow( x )
        # extract the two vectors of interest
        y <- x[ , 2 ]
        x <- x[ , 1 ]
    } else {
        # in this case both need to be vectors of equal length
        # make sure length is equal
        n <- length( x )
        if( length( y ) != n )
            stop( 'Both inputs must have the same length!' )
    }
    return( list( x = x, y = y, n = n ) )
}

# default to Pearson's offset
add_cor_CI <- function( r, n, alpha = 0.05, ci_offset = 3 ) {
    # use Fisher's transformation to estimate confidence intervals
    z <- ( log( 1 + r ) - log( 1 - r ) ) / 2
    rad <- stats::qnorm( alpha / 2, lower.tail = FALSE ) / sqrt( n - ci_offset )
    # alpha confidence intervals
    CIL <- tanh(z - rad)
    CIU <- tanh(z + rad)
    
    return( c(r, CIL, CIU) )
}
