#'
#' @title Get model code and write to file
#'
#' @description \code{stan} model code is extracted from the \code{red} package and written to file for compilation with \code{rstan}.
#' 
#' @return Text file with \code{*.stan} suffix 
#'
#' @export
model_code <- function(id, path = ".") {
    
	if (missing(id)) 
		stop("supply one of: id = 'global' or 'region' or 'region_pike'")
		
	switch(id,
         "region"      = { data("reg01", package = "red", envir = environment()); writeLines(reg01, con = paste0(path, "/", id, ".stan")) },
		 "region_pike" = { data("reg02", package = "red", envir = environment()); writeLines(reg02, con = paste0(path, "/", id, ".stan")) },
		 "global"      = { data("reg03", package = "red", envir = environment()); writeLines(reg03, con = paste0(path, "/", id, ".stan")) },
		 "region_interaction"      = { data("reg04", package = "red", envir = environment()); writeLines(reg04, con = paste0(path, "/", id, ".stan")) },
		 "region_interaction_pike"      = { data("reg05", package = "red", envir = environment()); writeLines(reg05, con = paste0(path, "/", id, ".stan")) })
		 
}
