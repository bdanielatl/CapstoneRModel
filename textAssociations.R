
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
# con_bw<-file("data/bad-words.txt")
# profanity<-readLines(con_bw)
# close(con_bw)
#
# startLine <- 1

 createTextFrequencyDF <- function (corpustext,controlArg,source="",transformToDataFrame=TRUE){
         if(is.null(controlArg)){
                 dtm<-DocumentTermMatrix(corpustext)
                 removeSparseTerms(dtm,.25)
                 #create a matrix and sort it
                 #decreasing each item in matrix will be a word with a nubmer value
                 freq<-sort(colSums(as.matrix(dtm)),decreasing = TRUE)
                 ord<-order(freq,decreasing=TRUE)
         }
         else{
                 #create a document term matrix from the corpora for analysis
                 dtm<-DocumentTermMatrix(corpustext,control=controlArg)
                 removeSparseTerms(dtm,.25)
                 #create a matrix and sort it
                 #decreasing each item in matrix will be a word with a nubmer value
                 freq<-sort(colSums(as.matrix(dtm)),decreasing = TRUE)
                 ord<-order(freq,decreasing=TRUE)
                 #length(freq) #how many terms do I have (tell me the lengt)
         }

         if(transformToDataFrame==TRUE)
         {
                 #build pareto analysis of the terms
                 wf=data.frame(term=names(freq),
                               occurrences=freq,
                               #cumfreqpct=cumsum((freq/sum(freq))*100),
                               source=as.character(source),stringsAsFactors = FALSE
                 )
                 wf$source = as.character(wf$source)
                 return (wf)
         }
         else{
                 return (dtm)
         }


 }


#read all three documents into memory random sample 1% of lines
 sampleAndWriteTexts<- function(dataSourcePath="data/final/en_US/en_US.twitter.txt"){

         if(!file.exists(paste0("temp/",strsplit(dataSourcePath,"/")[[1]][4]))){
                 ##read in the entire file and count the number of lines
                 con<- file(dataSourcePath,open="r")

                 fileLines<<-readLines(con)
                 close(con)
                 numLines<-length(fileLines)
                 sampleLines<-numLines*.05 #use 2% of the file to train
                 #sampleVector<-sample(1:numLines,round(sampleLines,0),replace=TRUE)

                 writeLines(sample_lines(filename=dataSourcePath,sampleLines,nlines=numLines),
                            con=paste0("temp/",strsplit(dataSourcePath,"/")[[1]][4]))
         }


 }

 sampleAndWriteTexts(dataSourcePath="data/final/en_US/en_US.blogs.txt")
 sampleAndWriteTexts(dataSourcePath="data/final/en_US/en_US.twitter.txt")
 sampleAndWriteTexts(dataSourcePath="data/final/en_US/en_US.news.txt")

  (corpora <- VCorpus(DirSource("temp/"),readerControl=list(language="english")))
 corpora<-tm_map(corpora,content_transformer(tolower))
 corpora<-tm_map(corpora,removeNumbers)
 corpora<-tm_map(corpora,removePunctuation)
 #corpora<-tm_map(corpora, removeWords, stopwords("english"))
 #corpora<-tm_map(corpora, removeWords, stopwords("SMART"))
 corpora<-tm_map(corpora,removeWords, profanity) #removeWords comes from the tm package
 corpora<-tm_map(corpora,stripWhitespace)


options(mc.cores=1)  #on MacOS you have to set the cores to single

 UnigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
 ugf<-createTextFrequencyDF(controlArg =  list(tokenize = UnigramTokenizer),corpustext = corpora,source="unigram",transformToDataFrame = TRUE)
 
 
 BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
 bgf<-createTextFrequencyDF(controlArg =  list(tokenize = BigramTokenizer),corpustext = corpora,source="bigram",transformToDataFrame = TRUE)
 
 TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
 tgf<-createTextFrequencyDF(controlArg =  list(tokenize = TrigramTokenizer),corpustext = corpora, source="trigram",transformToDataFrame = TRUE)


 
#only for creating the ngram files
ugf<-arrange(ugf,term)
bgf<-arrange(bgf,term)
tgf<-arrange(tgf,term)

saveRDS(ugf,"sentencecompleter/ugf.rds")
saveRDS(bgf,"sentencecompleter/bgf.rds")
saveRDS(tgf,"sentencecompleter/tgf.rds")

####
ugf<-readRDS("sentencecompleter/ugf.rds")
bgf<-readRDS("sentencecompleter/bgf.rds")
tgf<-readRDS("sentencecompleter/tgf.rds")

getMyWordProbability<-function(testGram="case of",ngframe=NULL,regexpr="regex"){
        #trigram probabilitiy
       
        if(regexpr == "regex"){
                df1<-as.data.frame(filter(ngframe, grepl(paste0("^",testGram,"\\s"),term)),stringsAsFactors=FALSE)
                
        }
        else{
                df1<-as.data.frame(filter(ngframe,term==testGram),stringsAsFactors=FALSE)
        }
        
        df1<-cbind(df1,tgsum = sum(df1$occurrences))#delete me
    
        return(df1)
 
}

filterNGrams<-function(x="",ngDataFrame=NULL){

        return(filter(ngDataFrame,term==x)$term)
}



