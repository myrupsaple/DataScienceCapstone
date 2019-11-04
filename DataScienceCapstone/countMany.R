countMany <- function(phrases, n = 3, log = TRUE) {
        # Log printout constants
        freqUpdate <- 2000
        freqReport <- 20000
        
        library(hash)
        wordCounts <- hash()
        
        ptm <- proc.time()
        times = vector()
        # Parse eachnd then each phrase a pair of words from that phrase
        for (i in 1:length(phrases)){
                ###################Function Status Printouts###################
                # Printout data so you can have the peace of mind of knowing
                # how close the function is to completing :)
                # Disable this part by setting argument log = False
                if(log){
                        if(i %% freqUpdate == 0){
                                percent = round(i/length(phrases), 4)*100
                                print(paste0("Analyzing: ", i, "/", length(phrases), ' (', 
                                            percent, '%) ', "Complete"))
                        }
                        if(i %% freqReport == 0){
                                time <- proc.time() - ptm
                                time <- round(time[[1]], 4)
                                timems <- time * 1000
                                print(paste0("Last ", freqReport, ": ", timems, " ms"))
                                times[i/freqReport] <- timems
                                total = sum(times)/1000
                                remaining = total*(100 - percent)/percent
                                print(paste0("Total: ", sum(times)/1000, " s"))
                                print(paste0("Estimated Time Remaining: ", round(remaining, 2), " s"))
                                ptm <- proc.time()
                        }
                }
                ################################################################
                
                if(phrases[i] == ''){
                        next
                }
                currentPhrase <- unlist(strsplit(phrases[i], ' '))
                if(length(currentPhrase) < n){
                        next
                }
                # Analyze each substring of n words
                for (j in 1:(length(currentPhrase) - (n - 1))){
                        wordSequence <- ''
                        voided <- FALSE
                        for (k in j:(j + (n - 2))){
                                if(currentPhrase[k] == ''){
                                        voided <- TRUE
                                        break;
                                }
                                wordSequence <- paste(wordSequence, tolower(currentPhrase[k]))
                        }
                        nextWord <- tolower(currentPhrase[j + (n - 1)])
                        if(nextWord == '' || voided){
                                next
                        }
                        currentCount <- wordCounts[[wordSequence]]
                        if(is.null(currentCount)){
                                wordCounts[[wordSequence]] <- hash()
                                wordCounts[[wordSequence]][[nextWord]] <- 1
                        }
                        else{
                                nextCount <- wordCounts[[wordSequence]][[nextWord]]
                                if(is.null(nextCount)){
                                        wordCounts[[wordSequence]][[nextWord]] <- 1
                                }
                                else{
                                        wordCounts[[wordSequence]][[nextWord]] = 
                                                wordCounts[[wordSequence]][[nextWord]] + 1        
                                }
                                
                        }
                }
        }
        
        print("Finishing Things Up...")
        # Convert the hash table data back into a dataframe 
        # and sort in descemding order
        print("Retrieving Keys...")
        wordSequences <- keys(wordCounts)
        
        wordSequenceDat <- vector()
        nextWordDat <- vector()
        counts <- vector()
        
        index = 1
        for (i in 1:length(wordSequences)){
                ################################################################
                if(i %% freqReport == 0){
                        percent = round(i/length(wordSequences), 4)*100
                        print(paste0("De-Hashing Counts: ", i, "/", length(wordSequences), ' (', 
                                     percent, '%) ', "complete"))
                }
                ################################################################
                wordSequence <- wordSequences[i]
                secondaryHash <- wordCounts[[wordSequence]]
                nextWords <- keys(secondaryHash)
                for (j in 1:length(nextWords)){
                        nextWord <- nextWords[j]
                        wordSequenceDat[index] <- wordSequence
                        nextWordDat[index] <- nextWord
                        counts[index] <- secondaryHash[[nextWord]]
                        index = index + 1
                }
        }
        
        print('De-Hashing Complete. Finishing Final Touches...')
        tempCount <- vector()
        for (i in 1:length(counts)){
                tempCount[i] <- counts[[i]]
        }
        wordCountsDf <- data.frame(wordSequenceDat, nextWordDat, tempCount)
        wordCountsDf <- wordCountsDf[order(-counts), ]
        rownames(wordCountsDf) <- c()
        colnames(wordCountsDf) <- c("Leading Phrase", "Next Word", "Count")
        wordCountsDf
}