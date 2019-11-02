removeSingles <- function(data, twoOrMoreWords = TRUE){
        data <- data[(data[, 2 + twoOrMoreWords] == 1), ] 
        data
}