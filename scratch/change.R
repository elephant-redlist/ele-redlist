#'
#' @title Calculate average change per unit increment
#' 
#' @param x numeric vector
#' @param i integer vector of locations
#'
#' @export
change <- function(x, i) {
    
    if (missing(i)) {
        i <- 1:length(x)
    }
    
    # non-NA values of x
    y <- c()    
    
    # location of non-NA values
    k <- c()
    
    # identify non-NA values 
    # and their location
    for (j in 1:length(x)) {
        
        if (!is.na(x[j])) {
            
            y <- c(y, x[j])
            k <- c(k, i[j])    
        }
    }
    
    if (length(y) > 1) {
        
        z <- numeric(length(y) - 1)
        
        for (j in 2:length(y)) {
            z[j - 1] <- (y[j] - y[j - 1]) / (k[j] - k[j - 1])
        }
        
        return(z)
        
    } else {
        return(NA)
    }
    
}
