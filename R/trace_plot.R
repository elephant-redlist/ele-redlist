#'
#' @title Plot MCMC sample traces
#'
#' @description Takes as input a \code{stanfit} object from call to \code{\link[rstan]{sampling}} and plots traces for desired parameters.
#' 
#' @param object      output from call to \code{\link[rstan]{sampling}}.
#' @param pars        character vector of posterior parameter samples to be extracted.
#' @param labels      character or expression vector of labels
#' @param divergences should divergent samples be marked?
#' @param warmup      should warmup samples be included?
#'
#' @examples 
#' require(rstan)
#' 
#' mdl <- "data{ int n; vector[n] x; }	parameters{ real mu; }	model{ x ~ normal(mu, 1.0);} generated quantities{ vector[n] x_sim; real x_sim_sum; for (i in 1:n) x_sim[i] = normal_rng(mu, 1.0); x_sim_sum = sum(x_sim);}\n"	
#' mdl <- stan_model(model_code = mdl)
#' n = 20
#' x = rnorm(n, 0, 2)
#' 
#' mdl.fit <- sampling(mdl, data = list(n = n, x = x), init = function() list(mu = 0), chains = 1)
#' 
#' trace_plot(mdl.fit, pars = "mu", labels = expression(mu))
#' 
#' @importFrom rstan extract
#' @importFrom bayesplot mcmc_trace nuts_params color_scheme_set
#' 
#' @export
"trace_plot" <- function(object, pars, ...) UseMethod("trace_plot")
#' @rdname get_rhat
#' @export
"trace_plot.stanfit" <- function(object, pars, labels, divergences = FALSE, warmup = FALSE, ...) {

  bayesplot::color_scheme_set("mix-blue-red")

  x <- rstan::extract(object, pars = pars, permute = FALSE, inc_warmup = warmup)
  y <- if (divergences) bayesplot::nuts_params(object) else NULL
  
  iter <- slot(object, "stan_args")[[1]][c("iter", "thin", "warmup")]
  
  if (!missing(labels)) {
    
    stopifnot(length(labels) == dim(x)[3])
    
    dimnames(x)[["parameters"]] <- labels
  }
  
  gg <- if (warmup) bayesplot::mcmc_trace(x, np = y, n_warmup = iter$warmup / iter$thin, ...) else bayesplot::mcmc_trace(x, np = y, iter1 = (iter$iter - iter$warmup) / iter$thin, ...)
    
  gg <- gg + theme_bw(base_size = 20) + theme(legend.position="none") + labs(x = "Posterior samples", y = "") + if (missing(labels)) facet_wrap(~parameter) else facet_wrap(~parameter, labeller = label_parsed)
  
  return(gg)
}
