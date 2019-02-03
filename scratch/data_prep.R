
data_prep <- function(data, var.names = list(), covar.names = list(), cofac.names = list()) {
    
    loc_var   <- match(var.names[['from']],   colnames(data))
    loc_covar <- match(covar.names[['from']], colnames(data))
    loc_cofac <- match(cofac.names[['from']], colnames(data))
    
    # coerce (continuous) dependent variable
    if (!is.null(var.names)) {
        for (i in loc_var)
            data[, i] <- as.numeric(data[, i])
        colnames(data)[loc_var] <- var.names[['to']]
    }
    
    # coerce continuous covariates
    if (!is.null(covar.names)) {
        for (i in loc_covar)
            data[, i] <- as.numeric(data[, i])
        colnames(data)[loc_covar] <- covar.names[['to']]
    }
    
    # coerce discrete covariates
    if (!is.null(cofac.names)) {
        for (i in loc_cofac)
            data[, i] <- factor(data[, i])
        colnames(data)[loc_cofac] <- cofac.names[['to']]
    }
    
    
    # return cleaned data
    return(data[, c(loc_var, loc_cofac, loc_covar)])
    
}
