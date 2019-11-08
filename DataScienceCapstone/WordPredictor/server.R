#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    source('functions/unpack.R')
    source('functions/predict.R')
    data1 <- read.csv('data/Master Single Word Counts.csv', stringsAsFactors = FALSE)
    data2 <- read.csv('data/Master Two Word Counts.csv', stringsAsFactors = FALSE)
    data3 <- read.csv('data/Master Three Word Counts.csv', stringsAsFactors = FALSE)
    data4 <- read.csv('data/Master Four Word Counts.csv', stringsAsFactors = FALSE)
    data5 <- read.csv('data/Master Five Word Counts.csv', stringsAsFactors = FALSE)
    obj <- unpack(data1, data2, data3, data4, data5)
    
    observe({    
        userInput <- input$userInput
        returnUserInput <- paste0('Your input: ', userInput)
        output$text <- renderText(returnUserInput)
        result <- predict(userInput, obj, mode = 'full')
        original <- result[[1]]
        corrected <- result[[2]]
        suggestions <- result[[3]]
        if(original != corrected){
            output$correctedMsg <- renderText(paste0('This was autocorrected to: ', corrected))
        }
        else{
            output$correctedMsg <- renderText(' ')
        }
        output$original <- renderText(result[[1]])
        output$corrected <- renderText(result[[2]])
        output$suggestions <- renderText(paste0('Suggestions: ', suggestions[1],
                                          ' || ', suggestions[2], ' || ', suggestions[3]))
        
    })
    ({

        # generate bins based on input$bins from ui.R
        

    })

})
