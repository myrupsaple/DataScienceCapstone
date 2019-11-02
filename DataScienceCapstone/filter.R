filter <- function(data, removeSingles = TRUE, removeSymbols = TRUE, 
                   removeNoAlpha = TRUE, twoOrMoreWords = TRUE){
        # Removes any items/pairings that appeared only once
        if(removeSingles){
                newdata <- data[!(data[, 2 + twoOrMoreWords] == 1), ] 
                data <- newdata
        }
        
        # Removes any entries containing non-alphanumeric symbols
        if(removeSymbols){
                length <- dim(data)[1]
                toRemove <- vector()
                index <- 1
                for (i in 1:length){
                        if(i %% 10000 == 0)
                        {
                                print(paste0('RemoveSymbols: ', i, '/', length, ' done'))
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
                newdata <- vector()
                newdata <- data[-toRemove,]
                data <- newdata
        }
        
        # Removes any entries that contain no letters
        if(removeNoAlpha){
                length <- dim(data)[1]
                toRemove <- vector()
                index <- 1
                for (i in 1:length){
                        if(i %% 10000 == 0)
                        {
                                print(paste0('RemoveNoAlpha: ', i, '/', length, ' done'))
                        }
                        
                        if (!grepl('+[[:lower:]]', data[i, 1])){
                                toRemove[index] <- i
                                index = index + 1
                        }
                        else if(twoOrMoreWords && !grepl('+[[:lower:]]', data[i, 2])){
                                toRemove[index] <- i
                                index = index + 1
                        }
                }
                newdata <- vector()
                newdata <- data[-toRemove,]
                data <- newdata
        }
        
        
        
        data <- data[order(-data$Count), ]
        data
}