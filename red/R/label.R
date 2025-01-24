#' Label function
#'
#' @export
label <- function(x, upper = FALSE) {
  
  region <- c("east", "forest", "north", "south", "savannah", "global")
  
  label <- region[match(tolower(x), region)]
  
  ifelse(upper, toupper(label), label)
}