#' @title Calculate ELPD and WAIC
#'
#' @description Preferred model has the highest expected log-posterior density (ELPD) value
#'
#' @param x posterior log-likelihood matrix with dimensions $iterations \times n$
#'
#' @importFrom stats var
#' 
#' @export
"waic" <- function(object, ...) UseMethod("waic")
#' @rdname waic
#' @export
"waic.matrix" <- function(object, ...) {
  
    x <- list(object, ...)
    
    if (length(x) > 1) {
      
      do.call("waic", args = list(object = x))
      
    } else {
      
      x <- object
      
      n <- dim(x)[2]
    
      cc <- apply(x, 2, max, na.rm = TRUE)
      
      # function to compute the log-mean-exponential
      lme <- function(y, a, ...) a + log(mean(exp(y - a), ...))
      
      # log-predictive density
      # per data point
      lpd <- numeric(n)
      
      for (i in 1:n)
        lpd[i] <- lme(x[,i], cc[i], na.rm = TRUE)
      
      # penalty
      pwaic <- apply(x, 2, var, na.rm = TRUE)
      
      # expected log predictive density
      elpd <- lpd - pwaic
      
      # summarise across data
      SE   <- sqrt(n * var(elpd))
      elpd <- sum(elpd)
      
      # return
      return(c("elpd" = round(elpd, 1), "SE_elpd" = round(SE, 1), "WAIC" = round(-2 * elpd, 1)))
    }
  }

#' @export
"waic.list" <- function(object, ...) {
  
  y <- list()
  z <- list()
  
  n <- lapply(object, function(a) dim(a)[2])
  
  if (all(unlist(n) == n[[1]])) {
    n <- n[[1]]
  } else {
    stop("number of data points is not equal between models")  
  }
  
  # function to compute the log-mean-exponential
  lme <- function(y, a, ...) a + log(mean(exp(y - a), ...))
  
  for (j in 1:length(object)) {
    
    x <- object[[j]]
    
    cc <- apply(x, 2, max, na.rm = TRUE)
    
    # log-predictive density
    # per data point
    lpd <- numeric(n)
    
    for (i in 1:n)
      lpd[i] <- lme(x[,i], cc[i], na.rm = TRUE)
    
    # penalty
    pwaic <- apply(x, 2, var, na.rm = TRUE)
    
    # expected log predictive density
    y[[j]] <- elpd <- lpd - pwaic
    
    # summarise across data
    SE   <- sqrt(n * var(elpd))
    elpd <- sum(elpd)
    
    # record
    z[[j]] <- c("elpd" = round(elpd, 1), "SE_elpd" = round(SE, 1), "WAIC" = round(-2 * elpd, 1))
  
  }
  
  # add names
  if (!is.null(names(object))) {
    names(z) <- names(object)
  } else {
    names(z) <- paste0("model:", 1:length(z))  
  }
  
  # select best model
  rank <- rev(order(unlist(lapply(z, function(a) a['elpd']))))
  
  y <- y[rank]
  z <- z[rank]
  
  if (length(z) > 1) {
    
    for (k in 1:length(z)) {
    
      z[[k]]["elpd_diff"] <- -sum(y[[1]] - y[[k]])
      z[[k]]["SE_diff"]   <- sqrt(n * var(y[[1]] - y[[k]]))
    }
    
    # create data frame
    out <- plyr::ldply(z, .id = "model")
  
  } else {
    out <- z[[1]]
  }
  
  # return
  return(out)
}
