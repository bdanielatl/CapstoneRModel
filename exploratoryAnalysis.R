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

#load data
(copora <- VCorpus(DirSource("data/final/en_US/"),readerControl=list(language="en")))
#basic inspection to get character lengths

#get names of dcouments
metaCopora<-lapply(copora[1:length(copora)],meta)
#get their length in lines
originalLengths<-lapply( lapply(copora[1:length(copora)],as.character) , length)



#reading files and writing out the samples to a temporary space
#startLine <- sample(1:10000,size=1,replace=T)
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
        # for(i in 1:rv){
        #         #read in the lines from the sample
        #         txtR<-readLines(con,sample(1:6,size=1,replace=T))
        #         # txtR<-tolower(txtR)
        #         # #remove any profanity and non-word characters such as punctuation
        #         # txtR<-removeWords(txtR,profanity) #removeWords comes from the tm package
        #         # txtR<-removeWords(txtR,stopwords("SMART")) #SMART is an argument to the tm.stopwords function
        #         # txtR<-removeNumbers(txtR)
        #         # txtR<-removePunctuation(txtR)
        #         #remove all unnecessary trailing white space and any extra space from the cleanup
        #         #activities above
        #         # txtR<-gsub("^\\s+|\\s+$", "",  gsub("\\s{2,}"," ",txtR))
        #         df<-rbind(df, data.frame(txt=txtR,stringsAsFactors = FALSE))
        #         print(i)
        # }
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


createTextFrequencyDF <- function (controlArg){
        #create a document term matrix from the copora for analysis
        dtm<-DocumentTermMatrix(copora,control=controlArg)
        #create a matrix and sort it 
        #decreasing each item in matrix will be a word with a nubmer value 
        freq<-sort(colSums(as.matrix(dtm)),decreasing = TRUE) 
        ord<-order(freq,decreasing=TRUE)
        length(freq) #how many terms do I have (tell me the lengt)
        
        #build pareto analysis of the terms
        wf=data.frame(term=names(freq),
                      occurrences=freq,
                      cumfreqpct=cumsum((freq/sum(freq))*100)
        )
        return (wf)
}




##add charts of word counts here
#https://eight2late.wordpress.com/2015/05/27/a-gentle-introduction-to-text-mining-using-r/

wf<-createTextFrequencyDF(controlArg = list(wordLengths=c(4, 20)))

########### Experiment Station
#tmpdtm<-DocumentTermMatrix(copora,control=list(wordLengths=c(4,20)))
#inspect(tmpdtm[1:2,1:10])

#tdm<-TermDocumentMatrix(copora,control=list(wordLengths=c(4,20)))

#matrix<-create_matrix(copora,ngramLength = 3)

#########
#create tokenizer
#http://jaydenmacrae.blogspot.com/2013/12/word-pair-and-triplet-frequencies.html
# ngt <- function(x) NGramTokenizer(x, Weka_control(min=1,max=2))
# tdm <- TermDocumentMatrix(copora, control= list(tokenize = ngt))
# 
# terms <- data.frame(term = tdm$dimnames$Terms, 
#                     freq=slam::row_sums(tdm), 
#                     row.names=NULL)
# 
# makeTokenMatrix<-function(min, max,textContent){
#         nGramTokenizer <-function(x) NGramTokenizer(x, Weka_control(min = min, max = max))
#         dtm<- DocumentTermMatrix(Corpus(textContent),control=list(weighting=weightTf,tokenize=nGramTokenizer))
#         ngramMatrix<-as.matrix(dtm)
#         return (ngramMatrix)
# }


# Sets the default number of threads to use
options(mc.cores=1)
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
ngf<-createTextFrequencyDF(controlArg =  list(tokenize = BigramTokenizer))

#refind the list by filtering

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

# #covert to normal matrix
# dtm2<-as.matrix(dtm)
# #computer the frequency
# frequency<-colSums(dtm2)
# frequency<-sort(frequency,decreasing=TRUE)


