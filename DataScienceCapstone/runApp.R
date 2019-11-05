runApp <- function(){
        setwd('/Users/Riley 1/Documents/Data Science/John Hopkins/Course 10 - Capstone/DataScienceCapstone/DataScienceCapstone')
        classes <- c('character', 'numeric')
        data1 <- read.csv('Master Single Word Counts.csv', colClasses = classes)
        classes <- c('character', 'character', 'character', 'character', 'character')
        print('Loading required data...')
        data2 <- read.csv('Master Two Word Counts.csv', colClasses = classes)
        data3 <- read.csv('Master Three Word Counts.csv', colClasses = classes)
        data4 <- read.csv('Master Four Word Counts.csv', colClasses = classes)
        data5 <- read.csv('Master Five Word Counts.csv', colClasses = classes)
        
        library(hash)
        
        source('unpack.R')
        source('predict.R')
        
        print('Unpacking prediction model...')
        obj <- unpack(data1, data2, data3, data4, data5)
        while (TRUE){
                text <- readline('Enter text: ')
                predictions <- predict(text, obj, mode = 'full')
                concatenated <- paste(predictions, collapse = ' ')
                print(paste('Suggestions for your next word:', concatenated))
        }
}