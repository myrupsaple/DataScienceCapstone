filter <- function(data, removeSingles = TRUE, removeSymbols = TRUE, 
                   removeNoAlpha = TRUE, twoOrMoreWords = TRUE){
        # Removes any items/pairings that appeared only once
        if(removeSingles){
                newdata <- data[!(data[, 2 + twoOrMoreWords] == 1), ] 
                data <- newdata
        }
        
        nonzero <- dim(data)[1] > 0
        
        # Removes any entries containing non-alphanumeric symbols
        if(removeSymbols && nonzero){
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
                if(length(toRemove) > 0){
                        newdata <- data[-toRemove,]
                }
                else{
                        newdata <- data
                }
                newdata
        }
        
        nonzero <- dim(data)[1] > 0
        
        # Removes any entries that contain no letters
        if(removeNoAlpha && nonzero){
                if(length == 0){
                        next
                }
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
                if(length(toRemove) > 0){
                        newdata <- data[-toRemove,]
                }
                else{
                        newdata <- data
                }
                newdata
        }
        
        
        
        data <- data[order(-data$Count), ]
        data
}