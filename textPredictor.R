#read in the english files into objects
#requuire the tm library
#library(NLP)
library(tm)
library(LaF)
#library(reader)
#library(wordcloud)
#library(RTextTools)
library(RWeka)
#library(slam)
#library(tau)
#library(SnowballC)
library(dplyr)
#library(stringi)
library(stringr)
#library(data.table)
library(filehash)
set.seed(23456)

source(file="textPredictorLib.R")
#call initialization
initialization()
#only have to do this if the temp data files do not exist
sampleAndWriteTexts(dataSourcePath="data/final/en_US/en_US.blogs.txt")
sampleAndWriteTexts(dataSourcePath="data/final/en_US/en_US.news.txt")
sampleAndWriteTexts(dataSourcePath="data/final/en_US/en_US.twitter.txt")

#build the coprpus
corpora <- PCorpus(DirSource("temp/"),readerControl=list(reader=readPlain),dbControl=list(useDB=TRUE,dbName="./trainDB",dbType="DB1"))
corpora<-tm_map(corpora,content_transformer(tolower))
corpora<-tm_map(corpora,removeNumbers)
corpora<-tm_map(corpora,removePunctuation)
#corpora<-tm_map(corpora, removeWords, stopwords("english"))
#corpora<-tm_map(corpora, removeWords, stopwords("SMART"))
corpora<-tm_map(corpora,removeWords, profanity) #removeWords comes from the tm package
corpora<-tm_map(corpora,stripWhitespace)
corpora<-tm_map(corpora,PlainTextDocument)
#create the tokenizers


UnigramTokenizer <- function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 1, max = 1)) 
BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
#process unigrams
controls_U <- list(tokenize = UnigramTokenizer)
trainTDM_U<- TermDocumentMatrix(corpora,control=controls_U)
#trainTDM_U<-removeSparseTerms(trainTDM_U,.2) #keep only terms that are less than 20% sparse
freq_U <- rowSums(as.matrix(trainTDM_U))  #create a frequency count
#create a dataframe of the bigram ,its frequency, the start and end of each word
wf_U <- data.frame(word=names(freq_U),count=freq_U,stringsAsFactors=FALSE)
uniL<-str_split(wf_U$word," ")
wf_U$start <- sapply(uniL,FUN=function(x) x[1]) #starting unigram #this is for consistency
wf_U$end <- sapply(uniL,FUN=function(x) x[1]) #ending unigram
#process bigrams

#create the toeknization fucntion
controls_B <- list( stemming = function(word) wordStem(word, language = "english"),tokenize=BigramTokenizer) 
#tell the tm package to create a termdocumentmtraix with the bigram control function
trainTDM_B<- TermDocumentMatrix(corpora,control = controls_B)
#trainTDM_B<-removeSparseTerms(trainTDM_B,.2) #keep only terms that are less than 20% sparse
freq_B <- rowSums(as.matrix(trainTDM_B))  #create a frequency count
#create a dataframe of the bigram ,its frequency, the start and end of each word
wf_B <- data.frame(word=names(freq_B),count=freq_B,stringsAsFactors=FALSE)
#find the start and end of bigrams 
biL <- str_split(wf_B$word," ")
wf_B$start <- sapply(biL,FUN=function(x) x[1]) #starting unigram
wf_B$end <- sapply(biL,FUN=function(x) x[2]) #ending unigram

#process trigrams
#controls_T <- list(stemming = function(word) wordStem(word, language = "english"),tokenize=TrigramTokenizer) 
controls_T <- list(tokenize=TrigramTokenizer) 
trainTDM_T<- TermDocumentMatrix(corpora,controls_T)
#trainTDM_T<-removeSparseTerms(trainTDM_T,.2)

freq_T <- rowSums(as.matrix(trainTDM_T))  #we use rowSums as the terms are along the rows
#create a dataframe
wf_T <- data.frame(word=names(freq_T),count=freq_T,stringsAsFactors=FALSE)
#expland the wf_T DF -- store the starting bigram and the ending word
triL <- str_split(wf_T$word," ")
wf_T$start <- sapply(triL,FUN=function(x) paste(x[1],x[2]))
wf_T$end <- sapply(triL,FUN=function(x) x[3])

############# KN Smoothing ################
#get the freq of ferq of n-gram to get D for smoothing
#frequencies have been loaded in the respective data frames
predictKN <- function(input,maxResult = 3){
        #calculate discount factors by getting the number of ngrams from each set by getting total number
        #of ngrams that occur exactly once and twice
        D1<<-sum(filter(wf_U,count == 1 | count ==2)$count)
        D2<<-sum(filter(wf_B,count == 1 | count ==2)$count)
        D3<<-sum(filter(wf_T,count == 1 | count ==2)$count)
        
        input2 <<- unlist(strsplit(input," "))[2]
        ##trigram processing##
        #get the number of occurrences of a trigram given the start
        #of a input bigram
        #will return true or false vector given the input phrase
        subtri<<-filter(wf_T, start == input)
        cw1w2<<-sum(subtri$count)
        nw1w2<<-sum(grepl(paste0("^",input,"$"),wf_T$start))
        W2 <<- sum(grepl(paste0(input2,"$"),wf_T$start))
        p3 <<- D3*nw1w2/cw1w2
        
        ##bigram processing##
        subbi<<-filter(wf_B,start==input2)
        cw2 <<- sum(subbi$count)
        nw2<<-sum(grepl(input2,wf_B$start))
        nw<<-nrow(wf_B)
        p2<<-D3*nw2/cw2/nw
        p1<<-D2*nw2/cw2/nw
        
        ##conditionally handle 2-gram model
        
         cp<<- unique(subbi$word)
         pkn<<-rep(NA,length(cp))
         nw3df<<-subbi%>%group_by(word)%>%summarize(nw3c=sum(count))
         #nw2w3df<-wf_T%>%filter(start=="of")%>%group_by(word)
         #build up a list of the words in to a regular expression
         
         #nw2w3df<-wf_T%>%filter(grepl(past))
         # for(i in 1:length(cp)){
         #        print(i)
         #         nw3<<- sum(grepl(cp[i],wf_B$word))
         #         nw2w3<<-sum(grepl(paste0(input2,'',cp[i],'$'),wf_T$word))
         #         cw1w2w3 <<- subtri[subtri$start == cp[i],2]
         #         pkn[i]<<-max((cw1w2w3-D3),0)/cw1w2 + p3*(max((nw2w3-D3),0)/W2+p2*nw3)
         # }
        # 
         #predictWord<<-data.frame(next_word = cp, probability = pkn, stringsAsFactors = FALSE)
        # predictWord<<-predictWord[order(predictWord$probability, decreasing= T), ]
         #return(predictWord)
}

df<-predictKN(input="case of")