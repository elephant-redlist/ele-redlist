


reg_dimnames_from_X <- function(X) apply(X, 1, function(x) colnames(X)[as.logical(x)])
