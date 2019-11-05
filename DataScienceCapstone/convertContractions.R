convertContractions <- function(data){
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
                        print(match)
                        if(grepl(match, data[j, 1 + twoOrMoreWords])){
                                data[j, 1 + twoOrMoreWords] <- contractions[i]
                        }
                }
        }
}