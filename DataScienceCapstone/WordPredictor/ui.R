#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Word Prediction Algorithm"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            textInput('userInput', "Please enter some text:", 'Hello world...'),
            h6('Please allow up to 20 seconds for the application to start. Some
               suggestions will appear on the right once the application has finished loading.')
        ),

        # Show a plot of the generated distribution
        mainPanel(
            h4(textOutput('text')),
            h4(textOutput('correctedMsg')),
            h4(textOutput('suggestions'))
        )
    )
))
