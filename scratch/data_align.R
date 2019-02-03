

data_align <- function(data, label = character(), values = character()) {
    
    loc <- match(label, colnames(data))
    
    data <- data[data[,loc] %in% values,]
    
    data[,loc] <- factor(data[,loc])
    
    return(data)
}
