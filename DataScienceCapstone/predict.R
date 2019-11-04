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
        suggest1 <- ''
        suggest2 <- ''
        suggest3 <- ''
        if(len >= 4){
                potentials <- hash5[[text]]
                first <- potentials[1]
                second <- potentials[2]
                third <- potentials[3]
        }

        
}