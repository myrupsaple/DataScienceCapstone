countTwo <- function(phrases){
        library(hash)
        wordCounts <- hash()
        
        ptm <- proc.time()
        times = vector()
        # Parse each phrase and then each pair of words from that phrase
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
                if(length(currentPhrase) == 1){
                        next
                }
                # Parse each pair of words
                for (j in 1:(length(currentPhrase) - 1)){
                        # If a word was previously converted to a 0-length string,
                        # skip the word.
                        currentWord <- tolower(currentPhrase[j])
                        nextWord <- tolower(currentPhrase[j + 1])
                        if(currentWord == '' || nextWord == ''){
                                next
                        }
                        # Add to the combined words' frequency count
                        currentFirst <- wordCounts[[currentWord]]
                        if(is.null(currentFirst)){
                                wordCounts[[currentWord]] <- hash()
                                wordCounts[[currentWord]][[nextWord]] <- 1
                        }
                        # Create embedded hash table on first instance of currentWord
                        else{
                                currentSecond <- wordCounts[[currentWord]][[nextWord]]
                                # Add if not the first occurrence of currentWord + nextWord
                                if(is.null(currentSecond)){
                                        wordCounts[[currentWord]][[nextWord]] <- 1
                                }
                                # If first occurrence of combination, set value to 1
                                else{
                                        wordCounts[[currentWord]][[nextWord]] <-
                                                wordCounts[[currentWord]][[nextWord]] + 1
                                }
                        }
                }
        }
        # Convert the hash table data back into a dataframe 
        # and sort in descemding order
        firstWords <- keys(wordCounts)
        
        wordPairs <- vector()
        pairCounts <- vector()
        
        index = 1;
        
        for(i in 1:length(firstWords)){
                firstWord <- firstWords[i]
                secondaryHash <- wordCounts[[firstWord]]
                secondWords <- keys(secondaryHash)
                for(j in 1:length(secondWords)){
                        secondWord = secondWords[j]
                        wordPairs[index] <- paste(firstWord, secondWord, ' ')
                        pairCounts[index] <- wordCounts[[firstWord]][[secondWord]]
                        index = index + 1
                }
                        
        }
        
        print(times)
        
        count <- vector()
        for (i in 1:length(pairCounts)){
                count[i] <- pairCounts[[i]]
        }
        wordCountsDf <- data.frame(wordPairs, count)
        wordCountsDf <- wordCountsDf[order(-count), ]
        rownames(wordCountsDf) <- c()
        wordCountsDf
}