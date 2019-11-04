predict <- function(text, hashList, calls = 1, maxCalls = 3){
        text <- unlist(strsplit(text, ' '))
        text <- tolower(text)

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
                # print(lastWords) # Walkback to see prediction process
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
        # If we have no suggestions, delete the most recent word and attempt to
        # fill it in, then attempt to predict once more. This recursive call
        # will run the function no more than maxCalls times
        if(nSuggestions == 0 && calls < maxCalls){
                text <- text[-length(text)]
                calls = calls + 1
                suggestions <- predict(text, hashList, calls)
        }
        
        
        suggestions
        ## Single word implementation
        
}