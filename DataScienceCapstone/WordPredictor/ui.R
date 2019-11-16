library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = "style.css",

    # Application title
    titlePanel(h1("Word Prediction Algorithm"), "Word Predictor"),
    
    fluidRow(
        column(12, h4("Created by Riley Matsuda"))
    ),
    # Sidebar with a slider input for number of bins
    fluidRow(
            column(12, 
                   h6("Welcome! Please allow up to 20 seconds for the app to start up. 
                   You'll know that it's loaded when some suggestions appear down below.
                   Once it has loaded, enter some text in the field below, or click one 
                   of the buttons to get started!"))
    ),
    
    fluidRow(
            column(4, wellPanel(
                    textAreaInput('userText', "Please enter some text:", value = 'Hello world', resize = "vertical"),
                    checkboxInput('autoCorrectOn', h6("Full autocorrect mode (sacrifice some time for some accuracy)"), value = FALSE)
            )),
            
            column(8, 
                   h5(textOutput('originalTextHead')),
                   h6(textOutput('originalText')),
                   h5(textOutput('uncorrectedTextHead')),
                   h6(textOutput('uncorrectedText')),
                   h5(textOutput('correctedTextHead')),
                   h6(textOutput('correctedText'))
            )
    ),
    
    fluidRow(
            column(4, 
                    h5(textOutput('consoleTextHead')),
                    h6(htmlOutput('consoleText'))
                ),
            column(8, 
                   h3('Suggestions: '),
                   fluidRow(
                           column(2, uiOutput('suggestion_1')),
                           column(2, uiOutput('suggestion_2')),
                           column(2, uiOutput('suggestion_3'))
                   )
                )
    ),
    fluidRow(
            column(12, 
                   h1(''),
                   h1(''),
                   h6('Thanks Teresa for helping with the color scheme!'))
    )
))
