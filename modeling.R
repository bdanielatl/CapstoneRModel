#read in the english files into objects
#requuire the tm library
library(NLP)
library(tm)
library(reader)
library(RTextTools)
library(RWeka)

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

con<- file(paste0("data/",lang_code,dataSource),open="r")
#read in the profanity filter
        con_bw<-file("data/bad-words.txt")
        profanity<-readLines(con_bw)
        close(con_bw)
#newfile <- readLines(con)
#read vector //how many samples to use
rv<-1000

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
close(con) #done reading lines, now we can start analysis

#Code Samples
#sample from https://deltadna.com/blog/text-mining-in-r-for-term-frequency/

# Tasks to accomplish
# 
# Exploratory analysis - perform a thorough exploratory analysis of the data, understanding the distribution of words and relationship between the words in the corpora.
# Understand frequencies of words and word pairs - build figures and tables to understand variation in the frequencies of words and word pairs in the data.
# Questions to consider
# 
# 1) Some words are more frequent than others - what are the distributions of word frequencies?
# 2) What are the frequencies of 2-grams and 3-grams in the dataset?
# 3) How many unique words do you need in a frequency sorted dictionary to cover 50% of all word instances in the language? 90%?
# 4) How do you evaluate how many of the words come from foreign languages?
# 5) Can you think of a way to increase the coverage -- identifying words that may not be in the corpora or using a smaller number of words in the dictionary to cover the same number of phrases?

#us the tm packages VectorSource method to tranfrom the pasted, processed text into vectors
corpus_source<-VectorSource(paste(df$txt,collapse= " "))
#use the Corpus function to turn the vector into a corpus that fits into the 
#data spec defined by the tm package
corpus<-Corpus(corpus_source)

#now create a document term matrix
dtm<-DocumentTermMatrix(corpus)

#ngram matrix using the RTextTools package
#There's a bug in the RTextTools package; this thread shows how to do a workaround
#http://stackoverflow.com/questions/25054617/rtexttools-create-matrix-returns-non-character-argument-error
makeTokenMatrix<-function(min, max){
        nGramTokenizer <-function(x) NGramTokenizer(x, Weka_control(min = min, max = max))
        dtm<- DocumentTermMatrix(corpus,control=list(weighting=weightTf,tokenize=nGramTokenizer))
        ngramMatrix<-as.matrix(dtm)

        return (ngramMatrix)
}
ngFreq <- sort(colSums(makeTokenMatrix(2,2)),decreasing=TRUE)
# ngramx <- create_matrix(paste(df$txt,collapse= " "),ngramLength = 2)
# dtm2<-as.matrix(ngramx)
# ngFreq <- colSums(dtm2)
# ngFreq <- sort(ngFreq,decreasing = TRUE)

