removeLeadingSpace <- function(data){
        interest <- data[, 1]
        interest <- gsub('^\\s', '', interest)
        data[, 1] <- interest
        data
}