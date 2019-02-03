#'
#' @title Calculate and check modal values
#' 
#' @export
mode <- function(x) {
    ux  <- na.omit(unique(x) )
    tab <- tabulate(match(x, ux))
    ux[tab == max(tab)]
}
#' @export
is.mode <- function(x, y) {
    
    m <- mode(y)
    
    x %in% m & length(m) == 1
}

