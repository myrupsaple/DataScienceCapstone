runApp <- function(){
        setwd('/Users/Riley 1/Documents/Data Science/John Hopkins/Course 10 - Capstone/DataScienceCapstone/DataScienceCapstone')
        classes <- c('character', 'character', 'character', 'character', 'character')
        print('Loading required data...')
        data5 <- read.csv('Master Five Word Counts.csv', colClasses = classes)
        data4 <- read.csv('Master Four Word Counts.csv', colClasses = classes)
        data3 <- read.csv('Master Three Word Counts.csv', colClasses = classes)
        data2 <- read.csv('Master Two Word Counts.csv', colClasses = classes)
        classes <- c('character', 'numeric')
        data1 <- read.csv('Master Single Word Counts.csv', colClasses = classes)
        
        library(hash)
        
        source('unpack.R')
        source('predict.R')
        
        print('Unpacking prediction model...')
        obj <- unpack(data5, data4, data3, data2, data1)
        while (TRUE){
                text <- readline('Enter text: ')
                predictions <- predict(text, obj)
                print(paste('Suggestions for your next word:'))
                print(predictions)
        }
}