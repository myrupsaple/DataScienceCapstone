countMany <- function(phrases, n = 3) {
        library(hash)
        wordCounts <- hash()
        
        ptm <- proc.time()
        times = vector()
        # Parse each phrase and then each pair of words from that phrase
        for (i in 1:length(phrases)){
                ###################Function Status Printouts###################
                # Printout data so you can have the peace of mind of knowing
                # how close the function is to completing :)
                freqUpdate <- 500
                if(i %% freqUpdate == 0){
                        print(paste("Analyzing: ", i, "/", length(phrases), ' (', 
                                    round(i/length(phrases), 4)*100 , '%) ', "complete", sep = ''))
                }
                freqReport <- 2500
                if(i %% freqReport == 0){
                        time <- proc.time() - ptm
                        time <- round(time[[1]], 4)
                        timems <- time * 1000
                        print(paste("Last ", freqReport, " :", timems, "ms"))
                        times[i/freqReport] <- timems
                        print(paste("Total:", sum(times)/1000, "s"))
                        ptm <- proc.time()
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
                        for (k in j:(j + (n - 1))){
                                wordSequence <- paste(wordSequence, currentPhrase[k])
                        }
                        currentCount <- wordCounts[[wordSequence]]
                        if(is.null(currentCount)){
                                wordCounts[[wordSequence]] <- 1
                        }
                        else{
                                wordCounts[[wordSequence]] = wordCounts[[wordSequence]] + 1
                        }
                }
        }
        
        print("Finishing things up...")
        # Convert the hash table data back into a dataframe 
        # and sort in descemding order

        
        words <- keys(wordCounts)
        counts_named <- values(wordCounts)
        count <- vector()
        for (i in 1:length(counts_named)){
                count[i] <- counts_named[[i]]
        }
        wordCountsDf <- data.frame(words, count)
        wordCountsDf <- wordCountsDf[order(-count), ]
        rownames(wordCountsDf) <- c()
        wordCountsDf
}