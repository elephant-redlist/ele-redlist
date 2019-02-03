

data_dims <- function(data, ..., dims = TRUE, dimnames = !dims) {
    
    data.list <- list(data, ...)
    
    X_dims     <- structure(numeric(2), dim = 2, names = c('Y', 'A'))
    X_dimnames <- structure(vector('list', 2), names = c('Y', 'A'))
    
    # combine data
    data.list <- lapply(data.list, function(x) x[, c('year', 'area')])
    data      <- plyr::ldply(data.list)

    # labels
    areas <- levels(data$area)
    years <- unique(data$year)
    
    areas <- areas[order(areas)]
    years <- years[order(years)]
    
    X_dimnames[[1]] <- years
    X_dimnames[[2]] <- areas
    
    # dimensions
    X_dims[1] <- length(years)
    X_dims[2] <- length(areas)
    
    # return
    if (dims & !dimnames)     return(X_dims)
    if (dimnames) return(X_dimnames)
    
}
