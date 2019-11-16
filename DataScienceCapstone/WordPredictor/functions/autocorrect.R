autocorrect <- function(text, wordBank, minLength, mode = 'quick'){
        # Modes: 'full' will run through every possible filter
        # 'quick' will run the fast filters (will not cycle through the entire
        # alphabet in an effort to find a match)
        
        # additiveIndex keeps track of additional words we add to the string 
        # (in the case where the user concatenates two words)
        additiveIndex <- 0
        newText <- character()
        for (i in 1:length(text)){
                word <- text[i]
                characters <- unlist(strsplit(word, ''))
                # If word is valid or is too small to be autocorrected, skip
                if((word %in% wordBank) || (length(characters) <= minLength)){
                        newText[i + additiveIndex] <- word
                        next
                }
                # See if the user missed a character. Insert all letters into
                # each slot in the word and see if this creates a match.
                inserts <- tolower(LETTERS)
                inserts[27] <- "-"
                match <- FALSE
                
                len <- nchar(word)
                
                for (j in 1:len){
                        if(mode != 'full'){
                                break
                        }
                        lhs <- substr(word, 0, j - 1)
                        rhs <- substr(word, j, len)
                        for (k in 1:length(inserts)){
                                newWord <- paste0(lhs, inserts[k], rhs)
                                if(newWord %in% wordBank){
                                        match <- TRUE
                                        newText[i + additiveIndex] <- newWord
                                        break
                                }
                        }
                }
                if(match){
                        next
                }
                
                # See if the user swapped two characters. Swap all pairs of
                # adjacent characters and see if we get a match
                for (j in 3:len - 1){
                        lhs <- substr(word, 0, j - 1)
                        char1 <- substr(word, j, j)
                        char2 <- substr(word, j + 1, j + 1)
                        rhs <- substr(word, j + 2, len)
                        newWord <- paste0(lhs, char2, char1, rhs)
                        if(newWord %in% wordBank){
                                match <- TRUE
                                newText[i + additiveIndex] <- newWord
                                break
                        }
                }
                if(match){
                        next
                }
                
                # See if the user mistyped a character. Replace each chararacter
                # with all letters and see if this creates a match
                for (j in 1:len + 1){
                        if(mode != 'full'){
                                break
                        }
                        lhs <- substr(word, 0, j - 1)
                        rhs <- substr(word, j + 1, len)
                        for (k in 1:length(inserts)){
                                newWord <- paste0(lhs, inserts[k], rhs)
                                if(newWord %in% wordBank){
                                        match <- TRUE
                                        newText[i + additiveIndex] <- newWord
                                        break
                                }
                        }
                }
                if(match){
                        next
                }
                
                # See if the user added an extra character. Remove each
                # character and see if a match is found.
                for (j in 1:len + 1){
                        lhs <- substr(word, 0, j - 1)
                        rhs <- substr(word, j + 1, len)
                        newWord <- paste0(lhs, rhs)
                        if(newWord %in% wordBank){
                                match <- TRUE
                                newText[i + additiveIndex] <- newWord
                                break
                        }
                }
                if(match){
                        next
                }
                
                # See if the user concatenated two words (missed a space). If so,
                # add both words to the corrected string
                for (j in 1:len){
                        lhs <- substr(word, 0, j)
                        rhs <- substr(word, j + 1, len)
                        if(lhs %in% wordBank && rhs %in% wordBank){
                                newText[i + additiveIndex] <- lhs
                                additiveIndex <- additiveIndex + 1
                                newText[i + additiveIndex] <- rhs
                                match <- TRUE
                                break
                        }
                }
                if(match){
                        next
                }
                
                
                # Find matches starting from the front of the word (by taking
                # letters off of the back one by one). Corrects for typos near
                # the back of the word. Stops once we reach the minimum word
                # length
                forwards <- characters
                forwardIndex <- 0
                lenCharStart <- length(forwards)
                for (j in 1:(lenCharStart - minLength + 1)){
                        # print(forwards) # See how the algorithm thinks
                        indices <- which(grepl(paste0('^', paste(forwards, collapse = '')), 
                                               wordBank))
                        if(length(indices) > 0){
                                forwardIndex <- indices[1]
                                # print(wordBank[forwardIndex]) # Result
                                break
                        }
                        lenChar <- length(forwards)
                        forwards <- forwards[-lenChar]
                }
                
                # Find matches starting from the back of the word (by taking
                # letters off of the front one by one). Corrects for typos near
                # the front of the word. Stops once we reach the minimum word
                # length
                backwards <- characters
                backwardIndex <- 0
                lenCharStart <- length(backwards)
                for (j in 1:(lenCharStart - minLength + 1)){
                        # print(backwards) # See how the algorithm thinks
                        indices <- which(grepl(paste0(paste(backwards, collapse = ''), '$'), 
                                               wordBank))
                        if(length(indices) > 0){
                                backwardIndex <- indices[1]
                                # print(wordBank[backwardIndex]) # Result
                                break
                        }
                        backwards <- backwards[-1]
                }
                # If both matches are found, choose the word that matched more
                # characters
                if(forwardIndex == 0 && backwardIndex == 0){
                        match <- FALSE;
                }
                else if(length(forwards) > length(backwards)){
                        newText[i + additiveIndex] <- wordBank[forwardIndex]
                        match <- TRUE
                }
                else if(length(forwards) < length(backwards)){
                        newText[i + additiveIndex] <- wordBank[backwardIndex]
                        match <- TRUE
                }
                # If there is a tie for length, choose the word that was more
                # commonly used.
                else{
                        if(forwardIndex <= backwardIndex){
                                newText[i + additiveIndex] <- wordBank[forwardIndex]
                        }
                        else if(forwardIndex > backwardIndex){
                                newText[i + additiveIndex] <- wordBank[backwardIndex]
                        }
                        match <- TRUE
                }
                if(match){
                        next
                }
                
                # If no corrections could be made, return the value to the array
                newText[i + additiveIndex] <- word
                
        }
        newText
}