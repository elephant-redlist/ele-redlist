#'
#' @title Summary function for maximum a posteriori estimate
#' 
#' @description Takes as argument a return from a call to \code{\link{map}} and summarises the mean, sd and quantile values.
#' 
#' @param object return from a call to \code{\link{map}}
#' @param pars   parameters to be summarised
#'
#' @note This function will fail of the \code{draws} argument is not specified in the optimisation i.e. \code{optimizing(..., draws = <n>)}, where \code{<n>} is a large number.
#' 
#' @include map.R
#' 
#' @method summary map
#' 
#' @examples 
#' require(rstan)
#' 
#' mdl <- "data{ int n; vector[n] x; } parameters{ real mu; real sigma;} model{ x ~ normal(mu, sigma);} generated quantities{ vector[n] x_sim; for (i in 1:n) x_sim[i] = normal_rng(mu, sigma);} \n"	
#' mdl <- stan_model(model_code = mdl)
#' n <- 20
#' x <- rnorm(n, 0, 2)
#' 
#' mdl.fit <- optimizing(mdl, data = list(n = n, x = x), init = list(mu = 0, sigma = 1), draws = 2000)
#' 
#' mdl.map <- map(mdl.fit, pars = c("mu", "sigma", "x_sim"), dims = list("mu" = 0, "sigma" = 0, "x_sim" = n))
#' 
#' summary(mdl.map, pars = c("mu", "sigma"))
#' 
#' cbind(x = x, x_sim = summary(mdl.map, pars = "x_sim")[[1]][, "mean"])
#'
#' @export
"summary.map" <- function(object, pars) {
    
    if (missing(pars)) {
        
        pars <- names(object$estimate)    
    }
    
    out <- list()
    
    if (is.null(object$sd)) {
        
        stop("Must specify 'draws' argument in call to optimizing to calculate standard errors and credibility intervals") 
        
    } else {
    
        for (x in pars) {
            
            n <- length(object$estimate[[x]])
            
            if (n > 1) {
            
                # par is a vector
                out[[x]] <- cbind("mean"  = object$estimate[[x]], 
                                  "sd"    = object$sd[[x]],
                                  "2.5%"  = object$quantiles[[x]][2,],
                                  "50%"   = object$quantiles[[x]][1,],
                                  "97.5%" = object$quantiles[[x]][3,])
                
                rownames(out[[x]]) <- paste0(x, "[", 1:n, "]")
                
            } else {
                
                # par is a real value
                out[[x]] <- c("mean"  = object$estimate[[x]], 
                              "sd"    = object$sd[[x]],
                              object$quantiles[[x]][2],
                              object$quantiles[[x]][1],
                              object$quantiles[[x]][3])
                
            }
        }
    }
    return(out)
}

