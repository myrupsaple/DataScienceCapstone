mergeSets <- function(data1, data2, data3){
        library(hash)
        wordCounts <- hash()
        
        phrases <- rbind(data1, data2, data3)
        for (i in 1:length(phrases)[1]){
                currentPhrase <- phrases[i, 1]
                nextWord <- phrases[i, 2]
                count <- phrases[i, 3]
                currentKey <- wordCounts[[currentPhrase]]
                nextKey <- wordCounts[[currentPhrase]][[nextWord]]
                if(is.null(currentKey)){
                        wordCounts[[currentKey]] <- hash()
                        wordCounts[[currentKey]][[nextWord]] <- count
                }
                else{
                        if(is.null(nextKey)){
                                wordCounts[[currentKey]][[nextWord]] <- count
                        }
                        else{
                                wordCounts[[currentKey]][[nextWord]] <-
                                        wordCounts[[currentKey]][[nextWord]] +
                                        count
                        }
                }
        }
        
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
        
        keyPhrases <- keys(wordCounts)
        keyWords <- vector()
        nextWords <- vector()
        counts <- vector()
        index = 1
        
        for(i in 1:length(keyPhrases)){
                keyPhrase <- keyPhrases[i]
                secondaryHash <- wordCount[[keyPhrase]]
                nextWords <- keys(secondaryHash)
                for(j in 1:length(nextWords)){
                        nextWord <- nextWords[j]
                        keyWords[index] <- keyPhrase
                        nextWords[index] <- nextWord
                        counts[index] <- secondaryHash[[nextWord]]
                        index = index + 1
                }
        }
        
        tempCount <- vector()
        for (i in 1:length(counts)){
                tempCount[i] <- counts[[i]]
        }
        
        merged <- data.frame(keyWords, nextWords, tempCount)
        merged <- merged[order(-counts)]
        rownames(merged) <- c()
        colnames(merged) <- c('Leading Phrase', 'Next Word', 'Count')
        merged
}