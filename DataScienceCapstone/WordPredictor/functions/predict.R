predict <- function(text, hashList, maxCalls = 3, mode = 'quick', calls = 1, log = character()){
        source('functions/autocorrect.R')
        
        if(maxCalls < 1){
                maxCalls <- 1
        }
        
        showThinking = TRUE;
        
        allText <- unlist(strsplit(text, ' '))
        allText <- gsub('[[:punct:]]|[[:space:]]', '', allText)
        allText <- allText[allText != '']
        allText <- tolower(allText)
        returnOriginal <- paste(allText, collapse = ' ')
        
        text <- allText[max(1, length(allText) - (2 + maxCalls)):length(allText)]
        
        returnUncorrected <- paste(text, collapse = ' ')
        
        # Autocorrect any words that do not match our single words table
        # See if the last word closely resembles one of the items in our single
        # word table. Re-attempt to find suggestions with the replaced word.
        text <- autocorrect(text, wordBank = hashList[[1]], minLength = 3,
                            mode = mode)
        
        # Resize in case any additional words were added in the correction process
        text <- text[max(1, length(text) - (2 + maxCalls)):length(text)]
        
        returnCorrected <- paste(text, collapse = ' ')
        
        lastWords <- tail(text, min(length(text), 4))
        len <- length(lastWords)
        lastWords <- paste(lastWords, collapse = ' ')
        
        nSuggestions <- 0
        suggestions <- c('', '', '')
        
        consoleIndex <- length(log) + 1
        returnConsole <- log
        if(calls == 1){
                returnConsole[1] <- 'First Call:'
                consoleIndex <- 2
        }
        
        # Predict using the 4 most recently typed words, then 3 most recent,
        # then two most recent, then single most recent. Stop if we have found
        # 3 suggestions.
        for (i in 5:2){
                if(len + 1 < i){
                        next
                }
                hashTable <- hashList[[i]]
                potentials <- hashTable[[lastWords]]
                if(showThinking){
                        
                        if(is.null(potentials)){
                                secondOutput <- ' | No Matches'
                        }
                        else{
                                if(is.na(potentials[2])){
                                        potentials[2] <- ''
                                }
                                if(is.na(potentials[3])){
                                        potentials[3] <- ''
                                }
                                if(nSuggestions == 0){
                                        secondOutput <- paste0(' | Suggestions: (1) ', 
                                                       potentials[1], ' (2) ', potentials[2], ' (3) ', potentials[3])
                                }
                                else if(nSuggestions == 1){
                                        secondOutput <- paste0(' | Suggestions: (1) ', 
                                                               suggestions[1], ' (2) ', potentials[1], ' (3) ', potentials[2])
                                }
                                else if(nSuggestions == 2){
                                        secondOutput <- paste0(' | Suggestions: (1) ', 
                                                               suggestions[1], ' (2) ', suggestions[2], ' (3) ', potentials[1])
                                }
                        }
                        thought <- paste0(paste0('[', i, ']: ', lastWords), secondOutput)
                        print(thought)
                        returnConsole[consoleIndex] <- thought
                        consoleIndex <- consoleIndex + 1
                        
                }
                # Potentials becomes null if a nonexisting hash key is used
                if(is.null(potentials)){
                        lastWords <- unlist(strsplit(lastWords, ' '))
                        lastWords <- lastWords[-1]
                        lastWords <- paste(lastWords, collapse = ' ')
                        next
                }
                # If we got a successful hash key, apply the suggestions
                first <- potentials[1]
                second <- potentials[2]
                third <- potentials[3]
                # Some will return NA if we have < 3 suggestions 
                if(nSuggestions < 3 && !(first %in% suggestions)){
                        suggestions[nSuggestions + 1] <- first
                        nSuggestions <- nSuggestions + 1
                }
                if(second != '' && nSuggestions < 3 && !(second %in% suggestions)){
                        suggestions[nSuggestions + 1] <- second
                        nSuggestions <- nSuggestions + 1
                }
                if(third != '' && nSuggestions < 3 && !(third %in% suggestions)){
                        suggestions[nSuggestions + 1] <- third
                        nSuggestions <- nSuggestions + 1
                }
                if(nSuggestions >= 3){
                        break
                }
                lastWords <- unlist(strsplit(lastWords, ' '))
                lastWords <- lastWords[-1]
                lastWords <- paste(lastWords, collapse = ' ')
        }
        
        # If we still have no suggestions, delete the most recent word and
        # attempt to fill it in, then attempt to predict once more. This
        # recursive call will cause the function to execute no more than
        # maxCalls times in total
        if(nSuggestions == 0 && length(text) > 1 && calls < maxCalls){
                text <- text[-length(text)]
                
                returnConsole[consoleIndex] <- ''
                returnConsole[consoleIndex + 1] <- paste0('Recursive Call #', calls, ':')
                consoleIndex <- consoleIndex + 2
                
                returnVals <- predict(text, hashList, calls = calls + 1, log = returnConsole)[4:5]
                suggestions <- returnVals[[1]]
                returnConsole <- returnVals[[2]]
        }
        
        if(suggestions[1] == '' && calls == 1){
                consoleIndex <- length(returnConsole) + 1
                returnConsole[consoleIndex] <- ''
                returnConsole[consoleIndex + 1] <- 'No matches found :('
        }
        
        returnSuggestions <- suggestions
        
        list(returnOriginal, returnUncorrected, returnCorrected, returnSuggestions, returnConsole)
        
}