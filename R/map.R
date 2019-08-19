#'
#' @title Extract maximum a posterior fit
#'
#' @description Takes as input a list from call to \code{\link[rstan]{optimizing}} and extracts maximum a posteriori (MAP) parameter estimates.
#' 
#' @details A call to \code{\link[rstan]{optimizing}} returns MAP parameter estimates, which represent the model of the posterior distribution and are equivalent to the maximum penalised likelihood or maximum posterior density. If 
#' the call was made with \code{optimizing(..., draws = <n>)} then it will draw \code{n} samples from the assumed multivariate normal posterior density on the untransformed scale, where \code{n} is
#' a large number (>2000). These are then used by the \code{map()} function to calculate standard errors and confidence intervals.
#' 
#' @param object    output from call to \code{\link[rstan]{optimizing}}
#' @param pars      character vector of map parameter esimates to be extracted
#' @param dims      named list of dimension vectors for each parameter. If only a single list entry is given it is applied to all parameters. If the parameter is a unit vector (i.e. of length one), it should be given dimension 0.
#' @param dim.names optional list of dimension names for each parameter. Names for each parameter should be given as a list. Dimension names for unit vectors are ignored (i.e. if \code{dims = 0}). 
#' If only a single \code{dim.names} list entry is given it is applied to all parameters.
#' 
#' @note This function uses regular expression matching to find the parameter values in the returned object from \code{\link[rstan]{optimizing}}.
#'
#' @examples
#' require(rstan)
#' 
#' mdl <- "data{ int n; vector[n] x; }	parameters{ real mu; }	model{ x ~ normal(mu, 1.0);} generated quantities{ vector[n] x_sim; real x_sim_sum; for (i in 1:n) x_sim[i] = normal_rng(mu, 1.0); x_sim_sum = sum(x_sim);}\n"	
#' mdl <- stan_model(model_code = mdl)
#' n = 20
#' x = rnorm(n, 0, 2)
#' 
#' mdl.fit <- optimizing(mdl, data = list(n = n, x = x), init = list(mu = 0), draws = 2000)
#' 
#' map(mdl.fit, pars = c("mu", "x_sim", "x_sim_sum"), dims = list("mu" = 0, "x_sim" = n, "x_sim_sum" = 0))
#' 
#' @export
"map" <- function(object, pars, ...) UseMethod("map")
#' @rdname map
#' @export
"map.list" <- function(object, pars, dims, dim.names, ...) {
    
	out <- vector('list', 3)
	names(out) <- c('estimate', 'sd', 'quantiles')
	
	if ("theta_tilde" %in% names(object) & dim(object$theta_tilde)[1] > 1) { ERROR <- TRUE
	} else { ERROR <- FALSE }
	    
    if (missing(pars)) {
        pars <- names(object[['par']])
    }
    
    out[['estimate']] <- vector('list', length(pars))
    names(out[['estimate']]) <- pars
	
	if (ERROR) {
	    
		out[['sd']] <- vector('list', length(pars))
		names(out[['sd']]) <- pars
		
		out[['quantiles']] <- vector('list', length(pars))
		names(out[['quantiles']]) <- pars
	}
    
    for (i in 1:length(pars)) {
        
        if (length(dims) > 1) {
            ds <- dims[[pars[i]]]
        } else ds <- dims[[1]]
        
        if (!missing(dim.names)) {
            if (length(dims) > 1) {
                dn <- dim.names[[pars[i]]]
            } else dn <- dim.names[[1]]
        } else dn <- NULL
        
        m <- regexpr(paste0(pars[i],"(\\[|$)"), names(object[['par']]))
        m <- object[['par']][m > 0]
        
        if (all(ds == 0)) { 
            # par is a real number
            out[['estimate']][[pars[i]]] <- as.numeric(m)
        } else {
            if (length(ds) == 1) {
                # par is a vector
                out[['estimate']][[pars[i]]] <- structure(as.numeric(m), dim = ds, names = NULL, dimnames = dn)
            } else {
                # par is an array
                out[['estimate']][[pars[i]]] <- structure(as.numeric(m), dim = ds, names = NULL, dimnames = dn)
            }
        }
		
		if (ERROR) {
			
			m <- regexpr(paste0(pars[i],"(\\[|$)"), colnames(object[['theta_tilde']]))
			m <- object[['theta_tilde']][, m > 0]
        
			if (all(ds == 0)) { 
			    
			    # par is a real number
			    
			    m1 <- sd(as.numeric(m))
			    out[['sd']][[i]] <- m1
			    
			    m2 <- quantile(as.numeric(m), c(0.5, 0.025, 0.975))
			    out[['quantiles']][[i]] <- structure(m2, dim = 3, names = c("50%", "2.5%", "97.5%"))
			    
			    
			} else {
			    
			    if (length(ds) == 1) {
			        
			        # par is a vector

			        m1 <- apply(as.matrix(m), 2, sd)
			        out[['sd']][[i]] <- structure(m1, dim = ds, names = NULL, dimnames = dn)
			        
			        ds <- c(3, ds)
			        dn <- c(list(quantile = c("50%", "2.5%", "97.5%")), dn)
			        m2 <- apply(as.matrix(m), 2, quantile, c(0.5, 0.025, 0.975))
			        out[['quantiles']][[i]] <- structure(m2, dim = ds, names = NULL, dimnames = dn)
			        
			        
			    } else {
			        
			        # par is an array

    			    m1 <- apply(as.matrix(m), 2, sd)
    			    out[['sd']][[i]] <- structure(m1, dim = ds, names = NULL, dimnames = dn)
    			    
    			    ds <- c(3, ds)
    			    dn <- c(list(quantile = c("50%", "2.5%", "97.5%")), dn)
    			    m2 <- apply(as.matrix(m), 2, quantile, c(0.5, 0.025, 0.975))
    			    out[['quantiles']][[i]] <- structure(m2, dim = ds, names = NULL, dimnames = dn)
			    }
			}
		}
    }
    
    # add class definition for downstream functions
    class(out) <- c(class(out), "map")
    
    # return
    return(out)
}
