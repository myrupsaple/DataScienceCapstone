filterSpecial <- function(phrases, n = 10){
        filtered <- vector()
        skipped = 0
        set.seed(1234)
        indices = sample(c(1:length(phrases)), length(phrases)/n, replace = FALSE)
        for (i in 1:length(indices)){
                if(i %% 10000 == 0){
                        print(paste("Processing:", i, "/", length(indices), "complete"))
                }
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