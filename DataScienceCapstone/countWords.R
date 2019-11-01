countWords <- function(phrases){
        library(hash)
        wordCounts <- hash()
        
        ptm <- proc.time()
        times = vector()
        
        # Parse each phrase and then each word from that phrase
        for (i in 1:length(phrases)){
                if(i %% 500 == 0){
                        print(paste("Analyzing:", i, "/", length(phrases), "complete"))
                }
                if(i %% 2500 == 0){
                        time <- proc.time() - ptm
                        time <- round(time[[1]], 3)
                        timems <- time * 1000
                        print(paste("Last 2500:", timems, "ms"))
                        times[floor(i/2500)] <- timems
                        print(paste("Total:", sum(times)/1000, "s"))
                        ptm <- proc.time()
                }
                if(phrases[i] == ''){
                        next
                }
                currentPhrase <- unlist(strsplit(phrases[i], ' '))
                # Parse each word
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