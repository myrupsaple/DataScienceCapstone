predict <- function(text, hashList, calls = 1, maxCalls = 3, mode = 'quick'){
        source('autocorrect.R')
        
        allText <- unlist(strsplit(text, ' '))
        allText <- tolower(allText)
        text <- allText[max(1, length(allText) - 6):length(allText)]
        
        # Autocorrect any words that do not match our single words table
        # See if the last word closely resembles one of the items in our single
        # word table. Re-attempt to find suggestions with the replaced word.
        text <- autocorrect(text, wordBank = hashList[[1]], minLength = 3,
                            mode = mode)

        lastWords <- tail(text, min(length(text), 4))
        len <- length(lastWords)
        lastWords <- paste(lastWords, collapse = ' ')
        
        nSuggestions <- 0
        suggestions <- c('', '', '')
        
        # Predict using the 4 most recently typed words, then 3 most recent,
        # then two most recent, then single most recent. Stop if we have found
        # 3 suggestions.
        for (i in 5:2){
                if(len + 1 < i){
                        next
                }
                hashTable <- hashList[[i]]
                potentials <- hashTable[[lastWords]]
                print(paste0('[', i, ']: ', lastWords)) # Enable to see how the algorithm thinks
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
                if(!is.na(second) && nSuggestions < 3 && !(second %in% suggestions)){
                        suggestions[nSuggestions + 1] <- second
                        nSuggestions <- nSuggestions + 1
                }
                if(!is.na(third) && nSuggestions < 3 && !(third %in% suggestions)){
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
                calls = calls + 1
                suggestions <- predict(text, hashList, calls = calls)
        }
        
        
        suggestions
        ## Single word implementation
        
}