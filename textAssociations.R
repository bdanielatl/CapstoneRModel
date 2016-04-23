
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



sampleAndWriteTexts(dataSourcePath="data/final/en_US/en_US.blogs.txt",startLine=startLine,
                    readvector=40000)
sampleAndWriteTexts(dataSourcePath="data/final/en_US/en_US.twitter.txt",startLine=startLine,
                    readvector=40000)
sampleAndWriteTexts(dataSourcePath="data/final/en_US/en_US.news.txt",startLine=startLine,
                    readvector=40000)

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

options(mc.cores=1)  #on MacOS you have to set the cores to single

UnigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
ugf<-createTextFrequencyDF(controlArg =  list(tokenize = UnigramTokenizer),corpustext = corpora,source="unigram",transformToDataFrame = TRUE)

BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
bgf<-createTextFrequencyDF(controlArg =  list(tokenize = BigramTokenizer),corpustext = corpora,source="bigram",transformToDataFrame = TRUE)

TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
tgf<-createTextFrequencyDF(controlArg =  list(tokenize = TrigramTokenizer),corpustext = corpora, source="trigram",transformToDataFrame = TRUE)

TetgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
tetgf<-createTextFrequencyDF(controlArg =  list(tokenize =  TetgramTokenizer),corpustext = corpora, source="tetragram",transformToDataFrame = TRUE)




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



