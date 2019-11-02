countTwo <- function(phrases, log = TRUE){
        library(hash)
        wordCounts <- hash()
        
        ptm <- proc.time()
        times = vector()
        # Parse each phrase and then each pair of words from that phrase
        for (i in 1:length(phrases)){
                ###################Function Status Printouts###################
                # Printout data so you can have the peace of mind of knowing
                # how close the function is to completing :)
                # Disable this part by setting argument log = False
                freqUpdate <- 2000
                if(log){
                        if(i %% freqUpdate == 0){
                                percent = round(i/length(phrases), 4)*100
                                print(paste("Analyzing: ", i, "/", length(phrases), ' (', 
                                            percent, '%) ', "complete", sep = ''))
                        }
                        freqReport <- 2000
                        if(i %% freqReport == 0){
                                time <- proc.time() - ptm
                                time <- round(time[[1]], 4)
                                timems <- time * 1000
                                print(paste("Last ", freqReport, " :", timems, "ms"))
                                times[i/freqReport] <- timems
                                total = sum(times)/1000
                                remaining = total*(100 - percent)/percent
                                print(paste("Total:", sum(times)/1000, "s"))
                                print(paste("Estimated Time Remaining: ", round(remaining, 2), "s"))
                                ptm <- proc.time()
                        }
                }
                ################################################################
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
        
        print("Finishing things up...")
        # Convert the hash table data back into a dataframe 
        # and sort in descemding order
        firstWords <- keys(wordCounts)
        
        firstWordDat <- vector()
        secondWordDat <- vector()
        counts <- vector()
        
        index = 1
        for(i in 1:length(firstWords)){
                firstWord <- firstWords[i]
                secondaryHash <- wordCounts[[firstWord]]
                secondWords <- keys(secondaryHash)
                for(j in 1:length(secondWords)){
                        secondWord = secondWords[j]
                        firstWordDat[index] <- firstWord
                        secondWordDat[index] <- secondWord
                        counts[index] <- secondaryHash[[secondWord]]
                        index = index + 1
                }
                        
        }
        
        tempCount <- vector()
        for (i in 1:length(counts)){
                tempCount[i] <- counts[[i]]
        }
        wordCountsDf <- data.frame(firstWordDat, secondWordDat, tempCount)
        wordCountsDf <- wordCountsDf[order(-counts), ]
        rownames(wordCountsDf) <- c()
        colnames(wordCountsDf) <- c("First Word", "Second Word", "Count")
        wordCountsDf
}