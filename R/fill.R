#'
#' @title Linear interpolation function
#'
#' @export
fill <- function(x, complete = FALSE) {
    
    for (k in 1:length(x)) {
        
        if (is.na(x[k])) {
            
            if (all(is.na(x[1:(k-1)]))) {
                
                if (complete) x[k] <- x[k + min(which(!is.na(x[(k+1):length(x)])))] else next
                
            } else {
                
                if (all(is.na(x[(k+1):length(x)]))) {
                    
                    if (complete) x[k] <- x[max(which(!is.na(x[1:(k-1)])))] else next
                    
                } else {
                    
                    loc1 <- max(which(!is.na(x[1:(k-1)])))
                    loc2 <- k + min(which(!is.na(x[(k+1):length(x)])))
                    
                    x[k] <- x[loc1] + (x[loc2] - x[loc1]) / (loc2 - loc1)
                    
                }
            }
        }
    }
    
    return(x)
}
