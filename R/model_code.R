#'
#' @title Get model code
#'
#' @description \code{stan} model code is extracted from the \code{red} package for compilation with \code{rstan}.
#' 
#' @param id character vector being one of slope or poly
#' @return model code 
#'
#' @export
model_code <- function(id, path = ".") {
    
	if (missing(id)) 
		stop("supply one of: id = 'slope' or 'poly'")
		
	switch(id,
         "slope"      = readLines(system.file("extdata", "hierarchical_slope.stan", package = "red")),
		 "poly"      = readLines(system.file("extdata", "hierarchical_poly.stan", package = "red")))
		 
}
