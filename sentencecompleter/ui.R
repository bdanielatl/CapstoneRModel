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
  titlePanel("Sentence Predictor"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
            HTML("<p>This app is reading input data when it first loads. Please wait a moment before using.</p>"),
            HTML("<p>Enter your phrase input in the textbox below and press Go. 
                 The application will predict the next word in your sentence and print it out.
                 Other runner-up suggestions will appear in a table below the primary prediction.</p>"), 
        textInput(inputId="phraseInput", label="Phrase Input",value=""),
        actionButton(inputId="goButton", label="Go")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
           textOutput("txtRestate"),
           textOutput("txtPredictedPhrase"),
           dataTableOutput('mytable')
       #print the output of the top prediction from the backoff algorithm
            #also print the runner up phrase
    )
  )
))
