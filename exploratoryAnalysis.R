#read in the english files into objects
#requuire the tm library
library(NLP)
library(tm)
library(reader)
library(wordcloud)
library(RTextTools)
library(RWeka)
library(slam)
library(tau)
library(SnowballC)


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

#load data
(copora <- VCorpus(DirSource("data/final/en_US/"),readerControl=list(language="en")))
#basic inspection to get character lengths

#get names of dcouments
metaCopora<-lapply(copora[1:length(copora)],meta)
#get their length in lines
originalLengths<-lapply( lapply(copora[1:length(copora)],as.character) , length)


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

        #write.table(df,paste0("temp/",strsplit(dataSourcePath,"/")[[1]][4]),col.names=FALSE)
        write.table(txtR,paste0("temp/",strsplit(dataSourcePath,"/")[[1]][4]),col.names=FALSE)

}


sampleAndWriteTexts(dataSourcePath="data/final/en_US/en_US.blogs.txt",startLine=startLine,
                    readvector=50000)
sampleAndWriteTexts(dataSourcePath="data/final/en_US/en_US.twitter.txt",startLine=startLine,
                    readvector=50000)
sampleAndWriteTexts(dataSourcePath="data/final/en_US/en_US.news.txt",startLine=startLine,
                    readvector=50000)

(copora <- VCorpus(DirSource("temp/"),readerControl=list(language="english")))

copora<-tm_map(copora,content_transformer(tolower))
copora<-tm_map(copora,removeNumbers)
copora<-tm_map(copora,removePunctuation)
copora<-tm_map(copora, removeWords, stopwords("english"))
copora<-tm_map(copora, removeWords, stopwords("SMART"))
copora<-tm_map(copora,removeWords, profanity) #removeWords comes from the tm package
copora<-tm_map(copora,stripWhitespace)
copora<-tm_map(copora,stemDocument,lazy = TRUE)


createTextFrequencyDF <- function (corpustext,controlArg,source=""){
       
        #create a document term matrix from the copora for analysis
        dtm<-DocumentTermMatrix(corpustext,control=controlArg)
        #create a matrix and sort it 
        #decreasing each item in matrix will be a word with a nubmer value 
        freq<-sort(colSums(as.matrix(dtm)),decreasing = TRUE) 
        ord<-order(freq,decreasing=TRUE)
        length(freq) #how many terms do I have (tell me the lengt)
        
        #build pareto analysis of the terms
        wf=data.frame(term=names(freq),
                      occurrences=freq,
                      cumfreqpct=cumsum((freq/sum(freq))*100),
                      source=source
        )
        return (wf)
}




##add charts of word counts here
#https://eight2late.wordpress.com/2015/05/27/a-gentle-introduction-to-text-mining-using-r/

wf<-createTextFrequencyDF(controlArg = list(wordLengths=c(4, 20)),corpustext = copora)

# Sets the default number of threads to use
options(mc.cores=1)  #on MacOS you have to set the cores to single
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
bgf<-createTextFrequencyDF(controlArg =  list(tokenize = BigramTokenizer),corpustext = copora)

TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
tgf<-createTextFrequencyDF(controlArg =  list(tokenize = TrigramTokenizer),corpustext = copora)

#compare twitter versus blogs word frequency
wfx<-createTextFrequencyDF(controlArg = list(wordLengths=c(4, 20)),
                           corpustext = copora[1], 
                           source = copora[[1]]$meta$id)


wfx<-createTextFrequencyDF(controlArg = list(wordLengths=c(4, 20)),
                           corpustext = copora[2], 
                           source = copora[[2]]$meta$id)


#create a data frame with one column for source

#repeat this for blogs
