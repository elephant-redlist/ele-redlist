
# linear regression through summary data points each 
# with a standard error
lreg <- function(y, x, se) {
    
    variance <- se^2
    
    n <- length(x)
    
    # total variance assuming unit sample size for each year
    # see: http://math.stackexchange.com/questions/80195/variance-over-two-periods-with-known-variances
    total_variance <- (1/n) * (sum(variance) + sum((y - mean(y))^2))
    
    # total sums of squares
    SST <- total_variance * n
    
    # regression variance
    hat <- fitted(lm(y ~ x))
    SSR <- sum((hat - mean(y))^2)
    
    # error variance
    SSE <- SST - SSR
    
    # degrees of freedom (regression, error)
    df <- c(1, n - 2)
    
    # F ratio
    MSR <- SSR/df[1]
    MSE <- SSE/df[2]
    
    Fstat <- MSR/MSE
    
    # significance
    pvalue <- pf(Fstat[1], df[1], df[2], lower.tail = FALSE)
    
    return(list(slope = as.numeric(coef(lm(y ~ x))[2]), F = Fstat, r2 = SSR / SST, df = df, p = pvalue))
    
}

