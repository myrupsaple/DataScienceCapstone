removeProfanity <- function(data){
        badWords <- readLines('extras/Facebook Bad Words.txt')
        badWords <- unlist(strsplit(badWords, ', '))
        toRemove <- vector()
        index = 1
        for (i in 1:length(badWords)){
                for(j in 1:length){
                        if(grepl(paste0('^', badWords[i], '$'), data[j, 1])){
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