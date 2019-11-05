filter <- function(data, removeSingles = TRUE, removeSymbols = TRUE, 
                   removeNoAlpha = TRUE, removeProfanity = TRUE,
                   convertContractions = TRUE, twoOrMoreWords = TRUE){
        mode <- 'Mac'
        if(mode == 'Mac'){
                setwd('/Users/Riley 1/Documents/Data Science/John Hopkins/Course 10 - Capstone/DataScienceCapstone/DataScienceCapstone')       
        }
        else if(mode == 'PC'){
                setwd('C:/DS Capstone/DataScienceCapstone/DataScienceCapstone/extras')
        }
        # Removes any items/pairings that appeared only once
        if(removeSingles){
                newdata <- data[!(data[, 2 + twoOrMoreWords] < (1 + 2*(!twoOrMoreWords))), ] 
                data <- newdata
        }
        
        length <- dim(data)[1]
        nonzero <- length > 0
        
        # Removes any entries containing non-alphanumeric symbols
        if(removeSymbols && nonzero){
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
                        newdata <- data[-toRemove, ]
                        data <- newdata
                }
        }
        
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
                        newdata <- data[-toRemove, ]
                        data <- newdata
                }
        }
        
        if(removeProfanity && nonzero){
                length <- dim(data)[1]
                badWords <- readLines('extras/Bad Words.csv')
                badWords <- unlist(strsplit(badWords, ', '))
                toRemove <- vector()
                index <-  1
                progressMarker <- ceiling(length(badWords)/10)
                percent <- 10
                for (i in 1:length(badWords)){
                        if(i %% progressMarker == 0){
                                print(paste0('removeProfanity: ', percent, '%', ' done'))
                                percent <- percent + 10
                        }
                        match <- paste0('^', badWords[i], '$', '|^', badWords[i],
                                       ' | ', badWords[i], '$| ', badWords[i], ' ')
                        for(j in 1:length){
                                if(grepl(match, data[j, 1])){
                                        toRemove[index] <- j
                                        index = index + 1
                                }
                                else if(twoOrMoreWords && grepl(match, data[j, 2])){
                                        toRemove[index] <- j
                                        index = index + 1
                                }
                        }
                }
                if(length(toRemove) > 0){
                        newdata <- data[-toRemove, ]
                        data <- newdata
                }
        }
        
        if(convertContractions && nonzero){
                length <- dim(data)[1]
                contractions <- readLines('extras/Contractions.txt')
                contractions <- unlist(strsplit(contractions, ','))
                noAps <- gsub("'", '', contractions)
                progressMarker <- ceiling(length(contractions)/10)
                percent <- 10
                for (i in 1:length(contractions)){
                        if(i %% progressMarker == 0){
                                print(paste0('convertContractions: ', percent, '%', ' done'))
                                percent <- percent + 10
                        }
                        match <- paste0('^', noAps[i], '$')
                        for (j in 1:length){
                                if(grepl(match, data[j, 1 + twoOrMoreWords])){
                                        data[j, 1 + twoOrMoreWords] <- contractions[i]
                                }
                        }
                }
        }
        
        data <- data[order(-data$Count), ]
        data
}