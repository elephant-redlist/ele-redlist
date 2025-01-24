#'
#' @title Diagnostic functions
#'
#' @description Takes as input a \code{stanfit} object from call to \code{\link[rstan]{sampling}} and extracts Rhat and neff values.
#' 
#' @param object    output from call to \code{\link[rstan]{sampling}}.
#' @param pars      character vector of posterior parameter samples to be extracted.
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
#' get_rhat(mdl.fit, pars = "mu")
#' get_neff(mdl.fit, pars = "mu")
#' 
#' @importFrom rstan extract Rhat summary
#' 
#' @export
"get_rhat" <- function(object, pars, ...) UseMethod("get_rhat")
#' @export
"get_rhat.stanfit" <- function(object, pars, ...) {

  x <- rstan::extract(object, pars = pars, permute = FALSE)
  
  y <- data.frame(parameter = dimnames(x)[["parameters"]], value = as.numeric(apply(x, 3, rstan::Rhat)))
  
  y$rating                                  <- "Good"
  y$rating[y$value > 1.05 | y$value < 0.95] <- "Moderate"
  y$rating[y$value > 1.10 | y$value < 0.90] <- "Unreliable"
  
  return(y)
}
#' @rdname get_rhat
#' @export
"plot_rhat" <- function(object, pars, labels...) UseMethod("plot_rhat")
#' @export
"plot_rhat.stanfit" <- function(object, pars, labels) {
  
  y <- do.call("get_rhat", args = list(object = object, pars = pars))
  
  gg <- ggplot(y, aes(x = value, y = parameter)) + 
    geom_segment(aes(yend = parameter, xend = 1, col = rating), linewidth = 1) + 
    geom_point(aes(col = rating), size = 5) +
    geom_vline(xintercept = 1, col = "grey") + 
    geom_vline(xintercept = c(1.05, 1.10), linetype = "dashed", col = "black") + 
    theme_bw(base_size = 20) + 
    theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    labs(y = "", x = expression(hat(R))) +
    scale_color_manual(values = c("Good" = "darkgrey", "Moderate" = "lightblue", "Unreliable" = "tomato"))
  
  if (!missing(labels)) {
    stopifnot(length(labels) == nrow(y))
    names(labels) <- y[,"parameter"]
    gg + scale_y_discrete(labels = labels)
  } else {
    gg + theme(axis.text.y = element_blank())
  }
}
#' @rdname get_rhat
#' @export
"get_neff" <- function(object, pars, ...) UseMethod("get_neff")
#' @export
"get_neff.stanfit" <- function(object, pars, ...) {

  x <- rstan::summary(object, pars = pars)[["summary"]]
  
  n <- nrow(as.matrix(object, pars = "lp__"))
  
  y <- data.frame(parameter = dimnames(x)[[1]], n = n, neff = as.numeric(x[, "n_eff"]), value = as.numeric(x[, "n_eff"] / n))
  
  y$rating                <- "Unreliable"
  y$rating[y$value > 0.1] <- "Moderate"
  y$rating[y$value > 0.5] <- "Good"
  
  return(y)
}
#' @rdname get_rhat
#' @export
"plot_neff" <- function(object, pars, labels...) UseMethod("plot_neff")
#' @export
"plot_neff.stanfit" <- function(object, pars, labels) {
  
  y <- do.call("get_neff", args = list(object = object, pars = pars))
  
  gg <- ggplot(y, aes(x = value, y = parameter)) + 
    geom_segment(aes(yend = parameter, xend = 0, col = rating), linewidth = 1) + 
    geom_point(aes(col = rating), size = 5) +
    geom_vline(xintercept = 0, col = "grey") + 
    geom_vline(xintercept = c(0.1, 0.5), linetype = "dashed", col = "black") + 
    theme_bw(base_size = 20) + 
    theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    labs(y = "", x = expression(N[eff] / N)) +
    scale_color_manual(values = c("Good" = "darkgrey", "Moderate" = "lightblue", "Unreliable" = "tomato"))
  
  if (!missing(labels)) {
    stopifnot(length(labels) == nrow(y))
    names(labels) <- y[,"parameter"]
    gg + scale_y_discrete(labels = labels)
  } else {
    gg + theme(axis.text.y = element_blank())
  }
}


