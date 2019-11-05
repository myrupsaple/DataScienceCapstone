mergeSets <- function(data1, data2, data3){
        debug = FALSE # Disables output log
        library(hash)
        wordCounts <- hash()
        
        phrases <- rbind(data1, data2, data3)
        freqUpdate <- ceiling(dim(phrases)[1]/100)
        # May need to change '100' to something smaller for data sets that
        # contain less than 100 observations.
        freqPercent <- 1
        for (i in 1:dim(phrases)[1]){
                if(i %% freqUpdate == 0 && debug == FALSE){
                        print(paste0("Analyzing: ",
                                     freqPercent, '%', " complete"))
                        freqPercent <- freqPercent + 1
                }
                currentPhrase <- phrases[i, 1]
                nextWord <- phrases[i, 2]
                count <- phrases[i, 3]
                currentKey <- wordCounts[[currentPhrase]]
                nextKey <- wordCounts[[currentPhrase]][[nextWord]]
                if(is.null(currentKey)){
                        wordCounts[[currentPhrase]] <- hash()
                        wordCounts[[currentPhrase]][[nextWord]] <- count
                }
                else{
                        if(is.null(nextKey)){
                                wordCounts[[currentPhrase]][[nextWord]] <- count
                        }
                        else{
                                wordCounts[[currentPhrase]][[nextWord]] <-
                                        wordCounts[[currentPhrase]][[nextWord]] +
                                        count
                        }
                }
        }
        
        keyPhrases <- keys(wordCounts)
        keyWords <- vector()
        followingWords <- vector()
        counts <- vector()
        index = 1
        
        freqPercent <- 10
        freqUpdate <- ceiling(length(keyPhrases)/10)
        for(i in 1:length(keyPhrases)){
                if(i %% freqUpdate == 0 && debug == FALSE){
                        print(paste0("Processing: ", 
                                     freqPercent, '%', " complete"))
                        freqPercent <- freqPercent + 10
                }
                keyPhrase <- keyPhrases[i]
                secondaryHash <- wordCounts[[keyPhrase]]
                nextWords <- keys(secondaryHash)
                for(j in 1:length(nextWords)){
                        nextWord <- nextWords[j]
                        keyWords[index] <- keyPhrase
                        followingWords[index] <- nextWord
                        counts[index] <- secondaryHash[[nextWord]]
                        index = index + 1
                }
        }
        
        tempCount <- vector()
        for (i in 1:length(counts)){
                tempCount[i] <- counts[[i]]
        }
        
        merged <- data.frame(keyWords, followingWords, tempCount)
        merged <- merged[order(-counts), ]
        rownames(merged) <- c()
        colnames(merged) <- c('Leading_Phrase', 'Next_Word', 'Count')
        merged
}