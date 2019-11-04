predict <- function(text, hashList){
        hash5 <- hashList[[1]]
        hash4 <- hashList[[2]]
        hash3 <- hashList[[3]]
        hash2 <- hashList[[4]]
        hash1 <- hashList[[5]]
        
        text <- unlist(strsplit(text, ' '))
        len <- length(text)

        lastWords <- tail(text, len)
        lastWords <- paste(lastWords, collapse = ' ')
        
        nSuggestions <- 0
        suggestions <- c('', '', '')
        for (i in 1:4){
                hashRef <- hashList[[i]]
                if(len >= (5 - i) && nSuggestions < 3){
                        potentials <- hashRef
                        first <- potentials[1]
                        second <- potentials[2]
                        third <- potentials[3]
                        if(first != '' && nSuggestions < 3){
                                suggestions[nSuggestions + 1] <- first
                                nSuggestions <- nSuggestions + 1
                        }
                        if(second != '' && nSuggestions < 3){
                                suggestions[nSuggestions + 1] <- second
                                nSuggestions <- nSuggestions + 1
                        }
                        if(third != '' && nSuggestions < 3){
                                suggestions[nSuggestions + 1] <- third
                                nSuggestions <- nSuggestions + 1
                        }
                }
        }
        ## Single word implementation
        
}