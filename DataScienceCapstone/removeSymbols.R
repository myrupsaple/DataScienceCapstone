removeSymbols <- function(data, twoOrMoreWords = TRUE){
        length <- dim(data)[1]
        toRemove <- vector()
        index <- 1
        for (i in 1:length){
                if(i %% 1000 == 0)
                {
                        print(paste0(i, '/', length, ' done'))
                }
                
                if (grepl('[^[:alnum:]|[:blank:]]', data[i, 1])){
                        toRemove[index] <- i
                        index = index + 1
                }
                else if(twoOrMoreWords && grepl('[^[:alnum:]|[:blank:]]', data[i, 2])){
                        toRemove[index] <- i
                        index = index + 1
                }
        }
        newdata <- data[-toRemove,]
        newdata
        
}