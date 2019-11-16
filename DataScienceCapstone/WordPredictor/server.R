library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
        ptm <- proc.time()
        source('functions/unpack.R')
        source('functions/predict.R')
        data1 <- read.csv('data/Master Single Word Counts.csv', stringsAsFactors = FALSE)
        data2 <- read.csv('data/Master Two Word Counts.csv', stringsAsFactors = FALSE)
        data3 <- read.csv('data/Master Three Word Counts.csv', stringsAsFactors = FALSE)
        data4 <- read.csv('data/Master Four Word Counts.csv', stringsAsFactors = FALSE)
        data5 <- read.csv('data/Master Five Word Counts.csv', stringsAsFactors = FALSE)
        obj <- unpack(data1, data2, data3, data4, data5)
        
        getSuggestions <- reactive({
                acmode <- input$autoCorrectOn
                mode <- 'quick'
                if(acmode){
                        mode <- 'full'
                }
                predict(input$userText, obj, mode = mode)
        })
        
        observe({
                # Gather info
                input$userText
                input$autoCorrectOn
                result <- getSuggestions()
                original <- result[[1]]
                uncorrected <- result[[2]]
                corrected <- result[[3]]
                suggestions <- result[[4]]
                console <- result[[5]]
                
                # Output information to user
                output$originalTextHead <- renderText('Provided input (punctuation removed):')
                output$uncorrectedTextHead <- renderText('Input to be analyzed (first 6 words):')
                output$consoleTextHead <- renderText('Algorithm thought process:')
                output$correctedTextHead <- renderText('This was autocorrected to: ')
                
                if(length(unlist(strsplit(original, " "))) > 6){
                        uncorrected <- paste0('... ', uncorrected)
                        corrected <- paste0('... ', corrected)
                }
                if(corrected == uncorrected){
                        output$correctedTextHead <- renderText('')
                        corrected <- ''
                }
                
                output$originalText <- renderText(original)
                output$uncorrectedText <- renderText(uncorrected)
                output$correctedText <- renderText(corrected)
                returnConsoleText <- paste('<b>', paste(console, collapse = ' <br> '), '</b>')
                output$consoleText <- renderText(returnConsoleText)
                
                # Update buttons
                output$suggestion_1 <- renderUI({
                        actionButton('suggest1', label = suggestions[1])
                })
                output$suggestion_2 <- renderUI({
                        actionButton('suggest2', label = suggestions[2])
                })
                output$suggestion_3 <- renderUI({
                        actionButton('suggest3', label = suggestions[3])
                })
        })
        
        updateInput1 <- reactive({
                result <- getSuggestions()
                suggestions <- result[[4]]
                if(suggestions[1] != ''){
                        updateTextAreaInput(session, 'userText', value = paste(input$userText, suggestions[1]))
                }
        })
        updateInput2 <- reactive({
                result <- getSuggestions()
                suggestions <- result[[4]]
                if(suggestions[2] != ''){
                        updateTextAreaInput(session, 'userText', value = paste(input$userText, suggestions[2]))
                }
        })
        updateInput3 <- reactive({
                result <- getSuggestions()
                suggestions <- result[[4]]
                if(suggestions[3] != ''){
                        updateTextAreaInput(session, 'userText', value = paste(input$userText, suggestions[3]))
                }
        })
        
        observeEvent(input$suggest1, {
                updateInput1()
        })
        observeEvent(input$suggest2, {
                updateInput2()
        })
        observeEvent(input$suggest3, {
                updateInput3()
        })

})
