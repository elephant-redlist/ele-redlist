

reg_restart <- function(X.dims, filename) {
    
    # create function
    reg.init <- function() { list(gamma0  = scan(filename, skip = 0, nlines = 1), 
                                  gammaY  = structure(scan(filename, skip = 1, nlines = 1), dim = X.dims[c('Y')]),
                                  gammaA  = structure(scan(filename, skip = 2, nlines = 1), dim = X.dims[c('A','P')]), 
                                  gammaG  = structure(scan(filename, skip = 3, nlines = 1), dim = X.dims[c('G')]), 
                                  beta0   = scan(filename, skip = 4, nlines = 1), 
                                  betaY   = structure(scan(filename, skip = 5, nlines = 1), dim = X.dims[c('Y')]),
                                  betaA   = structure(scan(filename, skip = 6, nlines = 1), dim = X.dims[c('A','P')]), 
                                  betaG   = structure(scan(filename, skip = 7, nlines = 1), dim = X.dims[c('G')]),
                                  sigma   = scan(filename, skip = 8, nlines = 1),
                                  tau_gamma   = scan(filename, skip = 9, nlines = 1),
                                  tau_beta  = scan(filename, skip = 10, nlines = 1)) }
    
    # return function
    return(reg.init)
    
}