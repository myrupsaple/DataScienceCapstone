unpack <- function(data1, data2, data3, data4, data5){
        # Remove leading spaces from first column of datasets
        for (i in 1:5){
                combined[[i]][, 1] <- gsub('^\\s', '', combined[[i]][, 1])
        }
        
        hash1 <- data1[, 1]
        print('1/5 Unpacked')
        
        hash2 <- hash()
        keys <- data2[, 1]
        followups <- data2[, 2:4]
        for (i in 1:length(keys)){
                first <- followups[i, 1]
                second <- followups[i, 2]
                third <- followups[i, 3]
                nValid <- (1 + (second != '') + (third != ''))
                
                hashList <- character(nValid)
                hashList[1] <- first
                
                if(nValid > 1){
                        hashList[2] <- second
                }
                if(nValid > 2){
                        hashList[3] <- third
                }
                
                hash2[[keys[i]]] <- hashList
        }
        print('2/5 Unpacked')
        
        hash3 <- hash()
        keys <- data3[, 1]
        followups <- data3[, 2:4]
        for (i in 1:length(keys)){
                first <- followups[i, 1]
                second <- followups[i, 2]
                third <- followups[i, 3]
                nValid <- (1 + (second != '') + (third != ''))
                
                hashList <- character(nValid)
                hashList[1] <- first
                
                if(nValid > 1){
                        hashList[2] <- second
                }
                if(nValid > 2){
                        hashList[3] <- third
                }
                
                hash3[[keys[i]]] <- hashList
        }
        print('3/5 Unpacked')
        
        hash4 <- hash()
        keys <- data4[, 1]
        followups <- data4[, 2:4]
        for (i in 1:length(keys)){
                first <- followups[i, 1]
                second <- followups[i, 2]
                third <- followups[i, 3]
                nValid <- (1 + (second != '') + (third != ''))
                
                hashList <- character(nValid)
                hashList[1] <- first
                
                if(nValid > 1){
                        hashList[2] <- second
                }
                if(nValid > 2){
                        hashList[3] <- third
                }
                
                hash4[[keys[i]]] <- hashList
        }
        print('4/5 Unpacked')
        
        hash5 <- hash()
        keys <- data5[, 1]
        followups <- data5[, 2:4]
        for (i in 1:length(keys)){
                first <- followups[i, 1]
                second <- followups[i, 2]
                third <- followups[i, 3]
                nValid <- (1 + (second != '') + (third != ''))
                
                hashList <- character(nValid)
                hashList[1] <- first
                
                if(nValid > 1){
                        hashList[2] <- second
                }
                if(nValid > 2){
                        hashList[3] <- third
                }
                
                hash5[[keys[i]]] <- hashList
        }
        print('5/5 Unpacked')
        
        obj <- list(hash1, hash2, hash3, hash4, hash5)
        print('Unpacking Complete')
        obj
}