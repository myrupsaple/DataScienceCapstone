mergeSetsSingles <- function(data1, data2, data3){
        library(hash)
        wordCounts <- hash()
        
        phrases <- rbind(data1, data2, data3)
        for (i in 1:length(phrases)){
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
        
        for (i in 1:length(allWords)){
                currentWord <- allWords[i]
                keyWords[i] <- currentWord
                counts[i] <- wordCounts[[currentWord]]
        }
        tempCount <- vector()
        for (i in 1:length(counts)){
                tempCount[i] <- counts[[i]]
        }
        merged <- data.frame(keyWords, counts)
        merged <- merged[order(-counts), ]
        rownames(merged) <- c()
        colnames(merged) <- c("Word", "Count")
        merged
}