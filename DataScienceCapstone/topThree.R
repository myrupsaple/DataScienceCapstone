topThree <- function(data){
        debug = FALSE # Disables output log
        len <- dim(data)[1]
        freqUpdate <- ceiling(len/10)
        freqPercent <- 10
        
        print('Finished removing leading whitespaces')
        freqPercent <- 10
        
        library(hash)
        wordCounts <- hash()
        countKey <- '#CountHash!'
        indexKey <- '#IndexHash!'
        hashkeys <- c('a', 'b', 'c', countKey)
        
        for (i in 1:len){
                if(i %% freqUpdate == 0 && debug == FALSE){
                        print(paste0("Analyzing: ",
                                     freqPercent, '%', " complete"))
                        freqPercent <- freqPercent + 10
                }
                currentPhrase <- data[i, 1]
                nextWord <- data[i, 2]
                freqCount <- data[i, 3]
                currentKey <- wordCounts[[currentPhrase]]
                
                # Data should already be sorted by frequency count, so the hash
                # values will be stored in order of number of appearances
                if(is.null(currentKey)){
                        wordCounts[[currentPhrase]] <- hash()
                        wordCounts[[currentPhrase]][['a']] <- nextWord
                        wordCounts[[currentPhrase]][[indexKey]] <- 1
                        wordCounts[[currentPhrase]][[countKey]] <- freqCount
                }
                else if (wordCounts[[currentPhrase]][[indexKey]] < 3){
                        index <- wordCounts[[currentPhrase]][[indexKey]] + 1
                        wordCounts[[currentPhrase]][[indexKey]] <- index
                        wordCounts[[currentPhrase]][[hashkeys[index]]] <- nextWord
                        count <- wordCounts[[currentPhrase]][[countKey]]
                        wordCounts[[currentPhrase]][[countKey]] <- paste(count, freqCount)
                }
        }
        
        firstPhrases <- keys(wordCounts)
        keyWords <- vector()
        firstPopular <- vector()
        secondPopular <- vector()
        thirdPopular <- vector()
        counts <- vector()
        
        freqUpdate <- ceiling(length(firstPhrases)/10)
        freqPercent <- 10
        for (i in 1:length(firstPhrases)){
                if(i %% freqUpdate == 0 && debug == FALSE){
                        print(paste0("Processing: ",
                                     freqPercent, '%', " complete"))
                        freqPercent <- freqPercent + 10
                }
                keyWords[i] <- firstPhrases[i]
                mostPopular <- wordCounts[[firstPhrases[i]]]
                # Minus 2 for the count and index holders
                nEmpty = 3 - (length(keys(mostPopular)) - 2)
                firstPopular[i] <- mostPopular[[hashkeys[1]]]
                if(nEmpty < 2){
                        secondPopular[i] <- mostPopular[[hashkeys[2]]]
                }
                else{
                        secondPopular[i] <- ''
                }
                if(nEmpty < 1){
                        thirdPopular[i] <- mostPopular[[hashkeys[3]]]
                }
                else{
                        thirdPopular[i] <- ''
                }
                counts[i] <- mostPopular[[hashkeys[4]]]
        }
                
        topThree <- data.frame(keyWords, firstPopular, secondPopular, 
                               thirdPopular, counts)
        topThree <- topThree[order(keyWords), ]
        rownames(topThree) <- c()
        colnames(topThree) <- c('Key_Words', 'Most_Popular', 'Second_Popular',
                                'Third_Popular', 'Freq_Counts')
        topThree
}