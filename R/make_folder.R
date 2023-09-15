#' @title Utility function
#' @description Make a folder if it doesn't already exist
#' @param path path character string
#' @export
make_folder <- function(path) {
    if(dir.exists(path)) {
        message("directory already exists: ", ifelse(length(list.files(path)) > 0, paste0(list.files(path), collapse = "; "), "(empty)"))
    } else {
        dir.create(path, recursive = TRUE)
    }
}
