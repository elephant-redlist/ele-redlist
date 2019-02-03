

reg_init <- function(data, X.dims, filename, n.map = 0, check.bound = TRUE) {
    
    require(plyr)
    require(reshape2)
    require(rstan)
    
    check_bound1 <- function(x) {
        
        x[is.na(x)]  <-  0
        x[x ==  Inf] <-  9
        x[x == -Inf] <- -99
        x[x >  9]    <-  9
        x[x < -99]   <- -99
        
        return(x)
    }
	
	check_bound2 <- function(x) {
        
        x[is.na(x)]  <-  0
        x[x ==  Inf] <-  9
        x[x == -Inf] <- -9
        x[x >  9]    <-  9
        x[x < -9]    <- -9
        
        return(x)
	}
	
	# binomial catch coefficients
	dat_tmp <- ddply(data, .(year, area), summarize, tmp0 = mean(count > 0), .drop = FALSE) 
	dat_tmp$tmp0[is.nan(dat_tmp$tmp0)] <- 0
	dat_tmp$tmp <- with(dat_tmp, log(tmp0 / (1 - tmp0)))
	dat_tmp$tmp <- with(dat_tmp, check_bound1(tmp))
	
	dat_tmp <- merge(dat_tmp, ddply(dat_tmp, .(year), summarize, year_effect = mean(tmp)))
	
	dat_tmp$area_effect <- with(dat_tmp, tmp - year_effect)
	dat_tmp$area_effect[is.nan(dat_tmp$area_effect)] <- 0
	
	gammaY_init <- ddply(dat_tmp, .(year), summarize, year_effect = unique(year_effect), .drop = FALSE)
	gammaY_init <- gammaY_init[match(X.dimnames[['Y']], gammaY_init$year),]
	gammaY_init <- structure(gammaY_init$year_effect, dims = X.dims['Y'])
	
	gammaA_init <- ddply(dat_tmp, .(area), summarize, area_effect = mean(area_effect), .drop = FALSE)
	gammaA_init <- gammaA_init[match(X.dimnames[['A']], gammaA_init$area),]
	gammaA_init <- structure(gammaA_init$area_effect, dims = X.dims['A'])
	
    gammaA_init <- array(rep(gammaA_init, X.dims['P']), dim = X.dims[c('A','P')])
    
    stopifnot(dim(gammaY_init) == X.dims[c('Y')])
    stopifnot(dim(gammaA_init) == X.dims[c('A','P')])
    
	# positive catch log-normal coefficients
	dat_tmp <- ddply(subset(data, count > 0), .(year, area), summarize, tmp = mean(log(nonzero)), .drop = FALSE) 
	dat_tmp <- merge(dat_tmp, ddply(subset(data, count > 0), .(year), summarize, year_effect = mean(log(nonzero))))
	dat_tmp$area_effect <- with(dat_tmp, tmp - year_effect)
	dat_tmp$area_effect[is.nan(dat_tmp$area_effect)] <- 0
	
	betaY_init <- ddply(dat_tmp, .(year), summarize, year_effect = unique(year_effect), .drop = FALSE)
	betaY_init <- betaY_init[match(X.dimnames[['Y']], betaY_init$year),]
	betaY_init <- structure(betaY_init$year_effect, dims = X.dims['Y'])
	
	betaA_init <- ddply(dat_tmp, .(area), summarize, area_effect = mean(area_effect), .drop = FALSE)
	betaA_init <- betaA_init[match(X.dimnames[['A']], betaA_init$area),]
	betaA_init <- structure(betaA_init$area_effect, dims = X.dims['A'])

	betaA_init <- array(rep(betaA_init, X.dims['P']), dim = X.dims[c('A','P')])
    
    stopifnot(dim(betaY_init) == X.dims[c('Y')])
    stopifnot(dim(betaA_init) == X.dims[c('A','P')])
    
    gamma0_init <- 0
    beta0_init  <- 0
    sigma_init  <- 1
    tau_gamma_init  <- rep(1,2)
    tau_beta_init   <- rep(1,2)
    
    if (TRUE) {
        
        gammaY_init  <- check_bound1(gammaY_init)
        gammaA_init  <- check_bound1(gammaA_init)
        
        betaY_init   <- check_bound2(betaY_init)
        betaA_init   <- check_bound2(betaA_init)
    }
    
    reg.init <- function() { list(gamma0 = gamma0_init, gammaY = gammaY_init, gammaA = gammaA_init,
                                  beta0 = beta0_init,  betaY  = betaY_init,  betaA = betaA_init,
                                  sigma = sigma_init, 
                                  tau_gamma = tau_gamma_init,
                                  tau_beta = tau_beta_init) }
    
    # MPD run(s) to get inital values
    if (n.map > 0) {
        for (i in 1:n.map) {
        
            reg.init <- optimizing(reg, reg.dat, init = reg.init, verbose = TRUE)
            
            print(paste('ll:', round(reg.init$value, 2)))
            
            gamma0_init <- round(extract_data.frame(reg.init$par, "gamma0")[, 'value'], 3)
            beta0_init  <- round(extract_data.frame(reg.init$par, "beta0")[, 'value'], 3)
            
            gammaY_init <- round(extract_data.frame(reg.init$par, "gammaY")[, 'value'], 3)
            betaY_init  <- round(extract_data.frame(reg.init$par, "betaY")[, 'value'], 3)
            
            gammaY_init <- structure(gammaY_init, dim = X.dims[c('Y')])
            betaY_init  <- structure(betaY_init,  dim = X.dims[c('Y')])
            
            gammaA_init <- round(extract_data.frame(reg.init$par, "gammaA")[, 'value'], 3)
            betaA_init  <- round(extract_data.frame(reg.init$par, "betaA")[, 'value'], 3)
            
            gammaA_init <- structure(gammaA_init, dim = X.dims[c('A', 'P')])
            betaA_init  <- structure(betaA_init,  dim = X.dims[c('A', 'P')])
            
            sigma_init <- round(extract_data.frame(reg.init$par, "sigma")[, 'value'], 3)
            tau_gamma_init <- round(extract_data.frame(reg.init$par, "tau_gamma")[, 'value'], 3)
            tau_beta_init <- round(extract_data.frame(reg.init$par, "tau_beta")[, 'value'], 3)
            
            if (check.bound) {
                
                gamma0_init  <- check_bound1(gamma0_init)
                beta0_init   <- check_bound2(beta0_init)
                gammaY_init  <- check_bound1(gammaY_init)
                betaY_init   <- check_bound2(betaY_init)
                gammaA_init  <- check_bound1(gammaA_init)
                betaA_init   <- check_bound2(betaA_init)
    			sigma_init   <- check_bound2(sigma_init)
    			
            }
            
            reg.init <- function() { list(gamma0 = gamma0_init, gammaY = gammaY_init, gammaA = gammaA_init,
                                          beta0 = beta0_init,  betaY  = betaY_init,  betaA = betaA_init,
                                          sigma = sigma_init, 
                                          tau_gamma = tau_gamma_init,
                                          tau_beta = tau_beta_init) }
        }
    }
    
    # clean up previous connections
    if (file.exists(filename))
        file.remove(filename)
    
    # write values to file
    out <- file(filename, "w")
    cat(gamma0_init, file = out)
    cat("\n",        file = out, append = TRUE)
    cat(gammaY_init,  file = out, append = TRUE)
    cat("\n",        file = out, append = TRUE)
    cat(gammaA_init,  file = out, append = TRUE)
    cat("\n",        file = out, append = TRUE)
    cat(beta0_init,  file = out, append = TRUE)
    cat("\n",        file = out, append = TRUE)
    cat(betaY_init,   file = out, append = TRUE)
    cat("\n",        file = out, append = TRUE)
    cat(betaA_init,   file = out, append = TRUE)
    cat("\n",        file = out, append = TRUE)
    cat(sigma_init,   file = out, append = TRUE)
    cat("\n",        file = out, append = TRUE)
    cat(tau_gamma_init,   file = out, append = TRUE)
    cat("\n",        file = out, append = TRUE)
    cat(tau_beta_init,   file = out, append = TRUE)
    close(out)
    
    # create function
    reg.init <- function() { list(gamma0    = scan(filename, skip = 0, nlines = 1), 
                                  gammaY    = structure(scan(filename, skip = 1, nlines = 1), dim = X.dims[c('Y')]),
                                  gammaA    = structure(scan(filename, skip = 2, nlines = 1), dim = X.dims[c('A','P')]), 
                                  beta0     = scan(filename, skip = 3, nlines = 1), 
                                  betaY     = structure(scan(filename, skip = 4, nlines = 1), dim = X.dims[c('Y')]),
                                  betaA     = structure(scan(filename, skip = 5, nlines = 1), dim = X.dims[c('A','P')]), 
                                  sigma     = scan(filename, skip = 6, nlines = 1),
                                  tau_gamma = scan(filename, skip = 7, nlines = 1),
                                  tau_beta  = scan(filename, skip = 8, nlines = 1)) }
        
    # return function
    return(reg.init)
    
}
