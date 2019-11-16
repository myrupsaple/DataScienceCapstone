removeSingles <- function(data, twoOrMoreWords = TRUE){
        newdata <- data[!(data[, 2 + twoOrMoreWords] == 1), ] 
        data <- newdata
        data
}