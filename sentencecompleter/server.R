#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
#read in the english files into objects
#requuire the tm library
require(NLP)
require(tm)
require(reader)
require(LaF)
#require(wordcloud)
require(RTextTools)
require(RWeka)
require(slam)
require(tau)
require(SnowballC)
require(dplyr)
require(stringi)
require(stringr)


source("textHelpers.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

        
        observeEvent(input$goButton,{
                
                progress <- shiny::Progress$new()
                # Make sure it closes when we exit this reactive, even if there's an error
                on.exit(progress$close())
               
                
                
                sl <- unlist(str_split(input$phraseInput," "))
                
                if(length(sl) < 2){
                        #message : phrase too short
                        output$txtRestate <- renderText({
                                paste0("The tested phrase is too short. Must be at least two words long.")
                        })
                        output$txtPredictedPhrase<- renderText({
                                paste0("")
                        }) 
                        return("")
                }   
                testGram<- paste(sl[length(sl)-1],sl[length(sl)])
                print("printing testgram")
                print(testGram)
                                progress$set(message = "Predicting new phrase", value = 0)
                                
                                print("predicting sentence")
                                drR<-predictSentence(myinput= testGram)
                                print("sentence predicted")
                                #get the first row of drR
                                output$txtRestate <- renderText({
                                        paste0("The tested phrase is", testGram,".")
                                })
                                output$txtPredictedPhrase<- renderText({
                                        paste0("The predicted phrase is", testGram,".")
                                })

                                output$mytable = renderDataTable({
                                        top_n(drR,10,prob_result)
                                })
                
                # printedPrediction <- tryCatch(
                #         {
                #                 progress$set(message = "Predicting new phrase", value = 0)
                #                 print("predicting sentence")
                #                 drR<-predictSentence(myinput= testGram)
                #                 print("sentence predicted")
                #                 #get the first row of drR
                #                 output$txtRestate <- renderText({
                #                         paste0("The tested phrase is", testGram,".")
                #                 })
                #                 output$txtPredictedPhrase<- renderText({
                #                         paste0("The predicted phrase is", testGram,".")
                #                 })
                #                 
                #                 output$mytable = renderDataTable({
                #                         top_n(drR,10,prob_result)
                #                 })
                #                 
                #         }, error = function(err){
                #                 #print to the screen that the phrase cannot be predicted
                #                 output$txtRestate <- renderText({
                #                         paste("Please wait, the application is still loading.",geterrmessage())
                #                         
                #                         
                #                 })
                #                 
                #                 output$txtPredictedPhrase<- renderText({
                #                         paste0("")
                #                 })
                #         }
                #         
                # )         
                       
        }
        )        
        
  # output$distPlot <- renderPlot({
  #   
  #   # generate bins based on input$bins from ui.R
  #   x    <- faithful[, 2] 
  #   bins <- seq(min(x), max(x), length.out = input$bins + 1)
  #   
  #   # draw the histogram with the specified number of bins
  #   hist(x, breaks = bins, col = 'darkgray', border = 'white')
  #   
  # })
  
})
