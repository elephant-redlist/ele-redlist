

data_design_matrix <- function(data, X.dims, X.dimnames) {
    
    X <- structure(vector('list', 2), names = c('year', 'area'))
    
    N <- nrow(data)
    Y <- X.dims['Y']
    A <- X.dims['A']
    
    yrs <- X.dimnames[['Y']]
    ars <- X.dimnames[['A']]
    
    X.year <- matrix(0, N, Y)
    for (y in 1:Y)
        X.year[data$year == yrs[y], y] <- 1
    colnames(X.year) <- yrs
    
    X.area <- matrix(0, N, A)
    for (a in 1:A)
        X.area[data$area == ars[a], a] <- 1
    colnames(X.area) <- ars
    
    X[['year']]   <- as.integer(reg_dimnames_from_X(X.year))
    X[['area']]   <- X.area
    
    return(X)
}
