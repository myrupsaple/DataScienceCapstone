countWords <- function(phrases){
        library(hash)
        wordCounts <- hash()
        
        # Parse each phrase and then each word from that phrase
        for (i in 1:length(phrases)){
                if(phrases[i] == ''){
                        next
                }
                currentPhrase <- unlist(strsplit(phrases[i], ' '))
                for (j in 1:length(currentPhrase)){
                        # If a word was previously converted to a 0-length string,
                        # skip the word. This could have occurred if a word was
                        # entirely composed of special characters
                        currentWord <- tolower(currentPhrase[j])
                        if(currentWord == ''){
                                next
                        }
                        # Add one to the word's frequency count in the hash table
                        currentCount <- wordCounts[[currentWord]]
                        if(is.null(currentCount)){
                                wordCounts[[currentWord]] <- 1
                        }
                        else{
                                wordCounts[[currentWord]] <- currentCount + 1
                        }
                }
        }
        # Convert the hash table data back into a dataframe 
        # and sort in descemding order
        word <- keys(wordCounts)
        counts_named <- values(wordCounts)
        count <- vector()
        for (i in 1:length(counts_named)){
                count[i] <- counts_named[[i]]
        }
        wordCountsDf <- data.frame(word, count)
        wordCountsDf <- wordCountsDf[order(-count), ]
        rownames(wordCountsDf) <- c()
        wordCountsDf
}