#'
#' @title Identify values to trim according to defined difference from modal value
#' 
#' @include mode.R
#' 
#' @export
trim <- function(x, z, delta, ties = c("first", "random", "z", NULL)[4]) {
    
    # find mode
    m <- mode(x)
    
    # record difference between x
    # and the mode(s)
    trim <- vector('list', length(m))
    
    for (i in 1:length(m)) {
        trim[[i]] <- abs(x - m[i]) / mean(x)
    }
    
    # which mode causes the minimum difference?
    err  <- unlist(lapply(trim, sum))
    loc  <- which(err == min(err))
    
    # if more than one mode with equal
    # error values then either:
    
    if (length(loc) > 1 & is.na(ties)) {
        
        # keep both
        trim <- trim[loc]
        
        # distance between x value and closest mode
        trim <- apply(matrix(unlist(trim), ncol = length(trim)), 1, min)
    
    } else {
        
        if (missing(z)) {
            z <- rep("A", length(x))
        }
        
        # or break ties by identifiying which 
        # mode to delete and re-calculating
        # differences with x
        while (length(loc) > 1) {
            
            del <- switch(ties,
                          first  = length(loc),                                                  # delete last modal value
                          random = sample(loc, 1),                                               # delete random modal value
                          z      = ifelse(length(unique(z)) == 1, length(loc), rev(order(z))[1]) # delete lowest reliability score 'z'
            )
            
            trim <- lapply(trim, function(x) x[-del])
            trim <- trim[-del]
            m    <- m[-del]
            z    <- z[-del]
            
            err  <- unlist(lapply(trim, sum))
            loc  <- which(err == min(err))
        }
        
        trim <- abs(x - m[loc]) / mean(x)
    }
    
    if (missing(delta)) {
        
        return(paste(round(m[loc], 2), collapse = ";"))
    
    } else {
        
        return(list(x = x, trim = trim < delta, y = x[trim < delta], n_modes = length(loc)))
    }
}
