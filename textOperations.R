
#read in the english files into objects
#requuire the tm library
library(NLP)
library(tm)
library(reader)
#library(wordcloud)
library(RTextTools)
library(RWeka)
library(slam)
library(tau)
library(SnowballC)
library(dplyr)
library(stringi)
library(stringr)


source("textAssociations.R")


buildProbabilityTable<-function(testGram="case of",ngframe =tgf){
        #find probability of the testGram (a bigram) within the trigram
        dftgram <<- getMyWordProbability(testGram =testGram,ngframe=tgf,regexpr="regex"  )
        #build a list of the bigrams within the trigram set
        l<<-apply(dftgram[1],1,
                  function(params)
                          filterNGrams(x=(word(params[1],-2,-1)),
                                       ngDataFrame=bgf)
        )
        
        #find the probability of the bigram on the end of each located trigram
        #this is a list of data frames, each frame must be extracted as a row and appended
        dfbgram<<-apply(as.data.frame(l),1,function(params)getMyWordProbability(testGram=params[1],ngframe=bgf,regexpr="nonreg"))
        df<-NULL
        for(e in 1:length(dfbgram)) rbind(df,data.frame(term=dfbgram[[e]]$term,
                                                        occurrences = dfbgram[[e]]$occurrences,
                                                        source= dfbgram[[e]]$source,
                                                        tgsum = dfbgram[[e]]$tgsum),stringsAsFactors=FALSE)->df
        dfbgram<<-df
        
         
        ## get the last word of each bigram and then find its probability in the unigram set
        l<<-apply(dfbgram[1],1,
                  function(params)
                          filterNGrams(x=(word(as.character(params[1]),-1,-1)),
                                       ngDataFrame=ugf)
        )
        
        #
       
        #getting the unitgram
        dfugram<<-apply(data.frame(unlist(l),stringsAsFactors = FALSE),1,function(params)getMyWordProbability(testGram=params[1],ngframe=ugf,regexpr="nonreg"))
        
        dft<-NULL
        for(e in 1:length(dfugram)) rbind(dft,data.frame(term=as.character(dfugram[[e]]$term),
                                                                      occurrences = dfugram[[e]]$occurrences,
                                                                      source= as.character(dfugram[[e]]$source),
                                                                      tgsum = dfugram[[e]]$tgsum,stringsAsFactors=FALSE))->dft
        
        
        dfugram<<-dft
        dfugram$term<- as.character(dfugram$term)
        dfugram$source<-as.character((dfugram$source))
        
        #all word tables have been assembled merge them together into one table 
     
        dftgram$key<-word(dftgram$term,-2,-1) #get the bigram
        dfbgram$term <-as.character(dfbgram$term)
       
        dfuResult<<-left_join(dftgram,dfbgram,by=c("key"="term"))
        dfuResult$uKey <- word(dftgram$term,-1,-1)
        
        dfuResult<<-left_join(dfuResult,dfugram, by=c("uKey" = "term"))
        
        #group by bigram and sum,
        #first remove all incomplete cases
        dfuResult<<-dfuResult[complete.cases(dfuResult),]

        
          bgsum<<-group_by(dfuResult, source.y) %>% summarise(bigramsum=sum(tgsum.y))
        #  ugsum<<-group_by(dfuResult, source) %>% summarise(unigramsum=sum(tgsum))
        #  dfuResult<<-cbind(dfuResult,bigramsum=bgsum$bigramsum)%>%cbind(unigramsum=ugsum$unigramsum)
                
}

buildProbabilityTable()
#call to the result table for doing the model

        as.data.frame(dfuResult$term, (dfuResult$occurrences.x/dfuResult$tgsum.x)*.5+(dfuResult$occurrences.y/dfuResult$bigramsum)*.3+(dfuResult$occurrences/dfuResult$unigramsum)*.2, stringsAsFactors = FALSE)

