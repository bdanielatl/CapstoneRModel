#read in the english files into objects
#requuire the tm library
library(NLP)
library(tm)
library(reader)
library(wordcloud)
#get file if it does not already exist and unzip contents
fileName <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
destFile <- "data/Coursera-SwiftKey.zip"

#set code for processing files into vectors (this may be an input into the shiny app at the end)
lang_code <- "en_US"
#data source type (twitter, etc.) -- this also could be an input from the shiny app
dataSource<-".twitter.txt"  #other choice includes news.txt and blogs.txt

#set the seed to enable reproducible research
set.seed(1234)

if(!file.exists("data/Coursera-SwiftKey.zip"))
{
        download.file(fileName,destfile = destFile)
        unzip("data/Coursera-SwiftKey.zip", junkpaths = TRUE,overwrite = TRUE, exdir="data")
}
#make sure to get the bad-words file 
if(!file.exists("data/bad-words.txt")){
        download.file("http://www.cs.cmu.edu/~biglou/resources/bad-words.txt",destfile="data/bad-words.txt")
}

#reading files and writing out the samples to a temporary space
startLine <- sample(1:10000,size=1,replace=T)  

sampleAndWriteTexts<- function(dataSourcePath="data/final/en_US/en_US.blogs.txt",
                               startLine=sample(1:10000,size=1,replace=T),
                               readvector=1000){
        con<- file(dataSourcePath,open="r")
        
        for(i in 1:startLine){
                txtTmp<-readLines(con,1)
        }
        #now that the skip point has been reached, read the rest of the file
        #read in the profanity filter
        con_bw<-file("data/bad-words.txt")
        profanity<-readLines(con_bw)
        close(con_bw)
        #newfile <- readLines(con)
        
        #read vector //how many samples to use
        rv<-readvector
        
        #dataframe
        df<-data.frame(txt=character())
        #define function

        #now read in 1000 
        for(i in 1:rv){
                #read in the lines from the sample
                txtR<-readLines(con,sample(1:6,size=1,replace=T))
                txtR<-tolower(txtR)
                #remove any profanity and non-word characters such as punctuation
                txtR<-removeWords(txtR,profanity) #removeWords comes from the tm package
                txtR<-removeWords(txtR,stopwords("SMART")) #SMART is an argument to the tm.stopwords function 
                txtR<-removeNumbers(txtR)
                txtR<-removePunctuation(txtR)
                #remove all unnecessary trailing white space and any extra space from the cleanup 
                #activities above
                txtR<-gsub("^\\s+|\\s+$", "",  gsub("\\s{2,}"," ",txtR))
                
                df<-rbind(df, data.frame(txt=txtR,stringsAsFactors = FALSE))  
        }
        close(con) #done reading lines, now write lines
        
        write.table(df,paste0("temp/",strsplit(dataSourcePath,"/")[[1]][4]),col.names=FALSE)
        
}


sampleAndWriteTexts(dataSourcePath="data/final/en_US/en_US.blogs.txt",startLine=startLine,
                    readvector=1000)
sampleAndWriteTexts(dataSourcePath="data/final/en_US/en_US.twitter.txt",startLine=startLine,
                    readvector=1000)
sampleAndWriteTexts(dataSourcePath="data/final/en_US/en_US.news.txt",startLine=startLine,
                    readvector=1000)



# #us the tm packages VectorSource method to tranfrom the pasted, processed text into vectors
# corpus_source<-VectorSource(paste(df$txt,collapse= " "))
# #use the Corpus function to turn the vector into a corpus that fits into the 
# #data spec defined by the tm package
# corpus<-Corpus(corpus_source)
# 
# #sample from https://deltadna.com/blog/text-mining-in-r-for-term-frequency/
# 
# #now create a document term matrix
# dtm<-DocumentTermMatrix(corpus)
# 
# #covert to normal matrix
# dtm2<-as.matrix(dtm)
# #computer the frequency
# frequency<-colSums(dtm2)
# frequency<-sort(frequency,decreasing=TRUE)



