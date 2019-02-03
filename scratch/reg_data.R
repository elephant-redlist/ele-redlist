

reg_data <- function(data.sample, data.predict, X.dims, X.sample, X.predict) {
    
    stopifnot(nrow(data.sample)  == dim(X.sample[[1]])[1])
    stopifnot(nrow(data.predict) == dim(X.predict[[1]])[1])
    
    if(any(c(nlevels(data.sample$year),  nlevels(data.sample$area), nlevels(data.sample$period)) < as.integer(X.dims))) warning("No data for some model covariates")
    if(any(c(nlevels(data.predict$year),  nlevels(data.predict$area), nlevels(data.predict$period)) > as.integer(X.dims))) stop("Some covariates needed for prediction are not in the design matrix")
    
    list(N1 = nrow(data.sample), N2 = nrow(data.predict), 
         Y = X.dims['Y'], A = X.dims['A'], P = X.dims['P'], M = X.dims['M'],
         X1Y = X.sample[['year']],  X1A = X.sample[['area']],  X1P = X.sample[['period']], 
         X2Y = X.predict[['year']], X2A = X.predict[['area']], X2P = X.predict[['period']], 
         nonzero = as.numeric(data.sample$nonzero), count = as.integer(data.sample$count), effort1 = as.integer(data.sample$effort), effort2 = as.integer(data.predict$effort))
}
