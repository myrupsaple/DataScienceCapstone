runApp <- function(data5, data4, data3, data2, data1){
        library(hash)
        
        source('unpack.R')
        source('predict.R')
        
        obj <- unpack(data5, data4, data3, data2, data1)
        while (TRUE){
                text <- readline('Enter text: ')
                predictions <- predict(text, obj)
                print(paste('Suggestions for your next word:', predictions))
        }
}