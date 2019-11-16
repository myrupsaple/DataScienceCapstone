removeProfanity <- function(data, twoOrMoreWords = TRUE){
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