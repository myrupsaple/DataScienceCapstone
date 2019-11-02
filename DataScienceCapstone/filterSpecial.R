filterSpecial <- function(phrases, n = 10, log = TRUE){
        filtered <- vector()
        skipped = 0
        set.seed(1234)
        if (n == 1){
                indices = c(1:length(phrases))
        }
        else{
                indices = sample(c(1:length(phrases)), length(phrases)/n, replace = FALSE)
        }
        for (i in 1:length(indices)){
                ###########################Function Status Printouts###################
                # Printout data so you can have the peace of mind of knowing
                # how close the function is to completing :)
                # Disable this part by setting argument log = False
                if(i %% 25000 == 0){
                        print(paste("Processing:", i, "/", length(indices), ' (', 
                                    round(i/length(indices), 4)*100 , '%)' , "complete"), sep = '')
                }
                #######################################################################
                index = indices[i]
                if (class(phrases[index]) != 'character'){
                        skipped = skipped + 1
                        next
                }
                filtered[i - skipped] <- gsub("[[:punct:]]", "", phrases[index])
        }
        
        print("Filtration Complete!")
        filtered
}