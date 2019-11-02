countWords <- function(phrases, log = TRUE){
        # Log printout constants
        freqUpdate <- 2000
        freqReport <- 20000
        
         library(hash)
        wordCounts <- hash()
        
        ptm <- proc.time()
        times = vector()
        
        # Parse each phrase and then each word from that phrase
        for (i in 1:length(phrases)){
                ###################Function Status Printouts###################
                # Printout data so you can have the peace of mind of knowing
                # how close the function is to completing :)
                # Disable this part by setting argument log = False
                if(log){
                        if(i %% freqUpdate == 0){
                                percent = round(i/length(phrases), 4)*100
                                print(paste0("Analyzing: ", i, "/", length(phrases), ' (', 
                                            percent, '%) ', "complete"))
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
        
        print("Finishing things up...")
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