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

#textAssociations is based off of the ExploratoryAnalysis.R file which I wrote for the second assignment.

#get file if it does not already exist and unzip contents
fileName <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
destFile <- "data/Coursera-SwiftKey.zip"

#set code for processing files into vectors (this may be an input into the shiny app at the end)
lang_code <- "en_US"
#data source type (twitter, etc.) -- this also could be an input from the shiny app
dataSource<-".twitter.txt"  #other choice includes news.txt and blogs.txt
#load profanity for filtering
con_bw<-file("data/bad-words.txt")
profanity<-readLines(con_bw)
close(con_bw)

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
#read in the profanity base
con_bw<-file("data/bad-words.txt")
profanity<-readLines(con_bw)
close(con_bw)

startLine <- 1

sampleAndWriteTexts<- function(dataSourcePath="data/final/en_US/en_US.blogs.txt",
                               startLine=sample(1:10000,size=1,replace=T),
                               readvector=1000){
        con<- file(dataSourcePath,open="r")

        for(i in 1:startLine){
                txtTmp<-readLines(con,1)
        }
        #now that the skip point has been reached, read the rest of the file
        #read in the profanity filter

        #newfile <- readLines(con)

        #read vector //how many samples to use
        rv<-readvector

        #dataframe
        df<-data.frame(txt=character())
        #define function

        txtR<-readLines(con,n=readvector,skipNul=TRUE)
        
        close(con) #done reading lines, now write lines

        # write the text to a file; the [[1]][[4]] gets the file name of the original document
        write.table(txtR,paste0("temp/",strsplit(dataSourcePath,"/")[[1]][4]),col.names=FALSE)
}

createTextFrequencyDF <- function (corpustext,controlArg,source=""){
        
        #create a document term matrix from the corpora for analysis
        dtm<-DocumentTermMatrix(corpustext,control=controlArg)
        #create a matrix and sort it 
        #decreasing each item in matrix will be a word with a nubmer value 
        freq<-sort(colSums(as.matrix(dtm)),decreasing = TRUE) 
        ord<-order(freq,decreasing=TRUE)
        length(freq) #how many terms do I have (tell me the lengt)
        
        #build pareto analysis of the terms
        wf=data.frame(term=names(freq),
                      occurrences=freq,
                      #cumfreqpct=cumsum((freq/sum(freq))*100),
                      source=source
        )
        return (wf)
}

sampleAndWriteTexts(dataSourcePath="data/final/en_US/en_US.blogs.txt",startLine=startLine,
                    readvector=50000)
sampleAndWriteTexts(dataSourcePath="data/final/en_US/en_US.twitter.txt",startLine=startLine,
                    readvector=50000)
sampleAndWriteTexts(dataSourcePath="data/final/en_US/en_US.news.txt",startLine=startLine,
                    readvector=50000)

(corpora <- VCorpus(DirSource("temp/"),readerControl=list(language="english")))

 corpora<-tm_map(corpora,content_transformer(tolower))
 corpora<-tm_map(corpora,removeNumbers)
 corpora<-tm_map(corpora,removePunctuation)
 #corpora<-tm_map(corpora, removeWords, stopwords("english"))
 #corpora<-tm_map(corpora, removeWords, stopwords("SMART"))
 corpora<-tm_map(corpora,removeWords, profanity) #removeWords comes from the tm package
 corpora<-tm_map(corpora,stripWhitespace)
 #corpora<-tm_map(corpora,stemDocument,lazy = TRUE)

#myCorpus <- corpus(corpora) #trying to create a corpus object with quanteda, this is giving a bug
#nrow(docvars) == length(x) is not TRUE

 # options(mc.cores=1)  #on MacOS you have to set the cores to single
 # BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
 # bgf<-createTextFrequencyDF(controlArg =  list(tokenize = BigramTokenizer),corpustext = corpora,source="All Docs")
 # 
 # TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
 # tgf<-createTextFrequencyDF(controlArg =  list(tokenize = TrigramTokenizer),corpustext = corpora, source="All Docs")

 # QuadgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 4))
 # qgf<-createTextFrequencyDF(controlArg =  list(tokenize =  QuadgramTokenizer),corpustext = corpora, source="All Docs")
 # 
#get the individual probablities of each quadgram and then 
 #multiply it by a lambda coefficient, then add them together.
 
 #https://www.coursera.org/learn/data-science-project/module/VNKmf/discussions/HmPU3OvyEeWfwAohgaM63Q
 


#tdm<- TermDocumentMatrix(corpora, control = list())


#create a data frame with one column for source

#repeat this for blogs
