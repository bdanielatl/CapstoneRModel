
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




# Define server logic required to draw a histogram
shinyServer(function(input, output) {
        
        source("textHelpers.R") 
        
        observeEvent(input$goButton,{
                
                progress <- shiny::Progress$new()
                # Make sure it closes when we exit this reactive, even if there's an error
                on.exit(progress$close())
                
                
                printedPrediction <- tryCatch(
                        {
                                sl <- unlist(str_split(tolower(input$phraseInput)," "))
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
                                progress$set(message = "Predicting new phrase", value = 0)
                                
                                drR<-predictSentence(myinput= testGram)
                                
                                #get the first row of drR
                                output$txtRestate <- renderText({
                                        paste0("The tested phrase is ", testGram,".")
                                })
                                
                                tdd<-top_n(drR,1,prob_result)$term
                                
                                sl <- unlist(str_split(tolower(tdd)," "))
                                
                                predWord<-sl[length(sl)]
                                
                                output$txtPredictedPhrase<- renderText({
                                        paste0("The predicted word is ", predWord,".")
                                })
                                
                                output$mytable = renderDataTable({
                                        top_n(drR,10,prob_result)
                                })
                        }, error = function(err){
                                #print to the screen that the phrase cannot be predicted
                                output$txtRestate <- renderText({
                                        paste0("Please wait, the application is still 
                                               loading or the phrase was not found. 
                                               Try again or try another phrase, or refresh the browser 
                                               and try again.")
                                })

                                output$txtPredictedPhrase<- renderText({
                                        paste0(err)
                                })
                                dfEmpty<-NULL
                                output$mytable = renderDataTable({
                                       dfEmpty
                                })
                        }

                )
                
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