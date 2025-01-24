#' @title Version update
#' @description Update version number
#' @export
versionUpdate <- function(version, update = "0.0.0") {

  version <- as.integer(unlist(strsplit(version, split = ".", fixed = TRUE)))
  update  <- as.integer(unlist(strsplit(update,  split = ".", fixed = TRUE)))
  
  version <- version + update
  
  if (any(version > 9)) {
    l <- which(version > 9)
    if (l == 1L) stop("no more version numbers available")
    version[l - 1] <- version[l - 1] + 1
    version[l] <- 0
  }

  return(paste(version, collapse = "."))
}

