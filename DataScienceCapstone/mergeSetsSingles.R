mergeSetsSingles <- function(data1, data2, data3){
        library(hash)
        wordCounts <- hash()
        
        phrases <- rbind(data1, data2, data3)
        
        for (i in 1:dim(phrases)[1]){
                currentPhrase <- phrases[i, 1]
                currentCount <- phrases[i, 2]
                currentKey <- wordCounts[[currentPhrase]]
                if(is.null(currentKey)){
                        wordCounts[[currentPhrase]] <- currentCount
                }
                else{
                        wordCounts[[currentPhrase]] <- 
                                wordCounts[[currentPhrase]] + currentCount
                }
        }
        
        allWords <- keys(wordCounts)
        keyWords <- vector()
        counts <- vector()
        
        print('Finished removing leading whitespaces')
        
        for (i in 1:length(allWords)){
                currentWord <- allWords[i]
                keyWords[i] <- currentWord
                counts[i] <- wordCounts[[currentWord]]
        }
        
        freqUpdate <- ceiling(length(keyWords)/100)
        freqPercent <- 1
        # Remove leading spaces from datasets.
        for (i in 1:length(keyWords)){
                if(i %% freqUpdate == 0){
                        print(paste0("Removing Leading Spaces: ",
                                     freqPercent, '%', " complete"))
                        freqPercent <- freqPercent + 1
                }
                keyWords[i] <- gsub('^\\s', '', keyWords[i])
        }
        
        tempCount <- vector()
        for (i in 1:length(counts)){
                tempCount[i] <- counts[[i]]
        }
        merged <- data.frame(keyWords, counts, stringsAsFactors = FALSE)
        merged <- merged[order(-counts), ]
        rownames(merged) <- c()
        colnames(merged) <- c("Word", "Count")
        merged
}