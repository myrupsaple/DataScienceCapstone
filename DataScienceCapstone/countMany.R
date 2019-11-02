countMany <- function(phrases, n = 3, log = TRUE) {
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
                                print(paste("Analyzing: ", i, "/", length(phrases), ' (', 
                                            round(i/length(phrases), 4)*100 , '%) ', "complete", sep = ''))
                        }
                        freqReport <- 20000
                        if(i %% freqReport == 0){
                                time <- proc.time() - ptm
                                time <- round(time[[1]], 4)
                                timems <- time * 1000
                                print(paste("Last ", freqReport, " :", timems, "ms"))
                                times[i/freqReport] <- timems
                                print(paste("Total:", sum(times)/1000, "s"))
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
        
        print("Finishing things up...")
        # Convert the hash table data back into a dataframe 
        # and sort in descemding order
        wordSequences <- keys(wordCounts)
        
        wordSequenceDat <- vector()
        nextWordDat <- vector()
        counts <- vector()
        
        index = 1
        for (i in 1:length(wordSequences)){
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