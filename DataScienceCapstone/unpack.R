unpack <- function(data5, data4, data3, data2, data1){
        # Remove leading spaces from first column of datasets
        combined <- list(data5, data4, data3, data2, data1)
        for (i in 1:5){
                combined[[i]][, 1] <- gsub('^\\s', '', combined[[i]][, 1])
        }
        data5 <- combined[[1]]
        data4 <- combined[[2]]
        data3 <- combined[[3]]
        data2 <- combined[[4]]
        data1 <- combined[[5]]
        
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
        print('Data5 process complete.')
        
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
        print('Data4 process complete.')
        
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
        print('Data3 process complete.')
        
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
        print('Data2 process complete.')
        
        hash1 <- hash()
        print('Data1 process complete.')
        
        list(hash1, hash2, hash3, hash4, hash5)
        
        
}