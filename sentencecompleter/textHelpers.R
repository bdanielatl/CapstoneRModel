print("initializing helper code")

#read in the profanity base
# con_bw<-file("data/bad-words.txt")
# profanity<-readLines(con_bw)
# close(con_bw)

startLine <- 1

##create data frequency routines

# print("reading corpus")
# 
# #read in corpora texts
# (corpora <- VCorpus(DirSource("temp/"),readerControl=list(language="english")))
# 
# corpora<-tm_map(corpora,content_transformer(tolower))
# corpora<-tm_map(corpora,removeNumbers)
# corpora<-tm_map(corpora,removePunctuation)
# #corpora<-tm_map(corpora, removeWords, stopwords("english"))
# #corpora<-tm_map(corpora, removeWords, stopwords("SMART"))
# corpora<-tm_map(corpora,removeWords, profanity) #removeWords comes from the tm package
# corpora<-tm_map(corpora,stripWhitespace)
# print("corpora loaded")

print("createTextFrequencyDF")
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
print("createTextFrequencyDF loaded")
# options(mc.cores=1)  #on MacOS you have to set the cores to single
# print("creating tokenizer functions")
# UnigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
# ugf<-createTextFrequencyDF(controlArg =  list(tokenize = UnigramTokenizer),corpustext = corpora,source="unigram",transformToDataFrame = TRUE)
# 
# BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
# bgf<-createTextFrequencyDF(controlArg =  list(tokenize = BigramTokenizer),corpustext = corpora,source="bigram",transformToDataFrame = TRUE)
# 
# TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
# tgf<-createTextFrequencyDF(controlArg =  list(tokenize = TrigramTokenizer),corpustext = corpora, source="trigram",transformToDataFrame = TRUE)
# 
# print("creating tokenizer created")

print("reading RDS files")
ugf<-readRDS("ugf.rds")
bgf<-readRDS("bgf.rds")
tgf<-readRDS("tgf.rds")
print("reading RDS files complete")
#testgf<-readRDS("tetgf.rds")

print("getMyWordProbability")
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
print("buildProbabilityTable")
buildProbabilityTable<-function(testGram="case of",ngframe =tgf){
        #find probability of the testGram (a bigram) within the trigram
        dftgram <- getMyWordProbability(testGram =testGram,ngframe=tgf,regexpr="regex"  )
        #build a list of the bigrams within the trigram set
        l<-apply(dftgram[1],1,
                 function(params)
                         filterNGrams(x=(word(params[1],-2,-1)),
                                      ngDataFrame=bgf)
        )
        
        #find the probability of the bigram on the end of each located trigram
        #this is a list of data frames, each frame must be extracted as a row and appended
        dfbgram<-apply(as.data.frame(l),1,function(params)getMyWordProbability(testGram=params[1],ngframe=bgf,regexpr="nonreg"))
        dft<-NULL
        for(e in 1:length(dfbgram)) rbind(dft,data.frame(term=dfbgram[[e]]$term,
                                                         occurrences = dfbgram[[e]]$occurrences,
                                                         source= dfbgram[[e]]$source,
                                                         tgsum = dfbgram[[e]]$tgsum),stringsAsFactors=FALSE)->dft
        dfbgram<-dft
        
        
        ## get the last word of each bigram and then find its probability in the unigram set
        l<-apply(dfbgram[1],1,
                 function(params)
                         filterNGrams(x=(word(as.character(params[1]),-1,-1)),
                                      ngDataFrame=ugf)
        )
        
        #
        
        #getting the unitgram
        
        dfugram<-apply(data.frame(unlist(l),stringsAsFactors = FALSE),1,function(params)getMyWordProbability(testGram=params[1],ngframe=ugf,regexpr="nonreg"))
        
        dft<-NULL
        for(e in 1:length(dfugram)) rbind(dft,data.frame(term=as.character(dfugram[[e]]$term),
                                                         occurrences = dfugram[[e]]$occurrences,
                                                         source= as.character(dfugram[[e]]$source),
                                                         tgsum = dfugram[[e]]$tgsum,stringsAsFactors=FALSE))->dft
        
        
        dfugram<-dft
        dfugram$term<- as.character(dfugram$term)
        dfugram$source<-as.character((dfugram$source))
        
        #all word tables have been assembled merge them together into one table 
        
        dftgram$key<-word(dftgram$term,-2,-1) #get the bigram
        dfbgram$term <-as.character(dfbgram$term)
        
        dfuResult<-left_join(dftgram,dfbgram,by=c("key"="term"))
        dfuResult$uKey <- word(dftgram$term,-1,-1)
        
        
        
        
        dfuResult2<-left_join(dfuResult,dfugram, by=c("uKey" = "term"))
        
        #group by bigram and sum,
        #first remove all incomplete cases
        
        dfuResult2<-dfuResult2[complete.cases(dfuResult2),]
        bgsum<-group_by(dfuResult2, source.y) %>% summarise(bigramsum=sum(tgsum.y))
        ugsum<-group_by(dfuResult2, source) %>% summarise(unigramsum=sum(tgsum))
        dfuResult2<-cbind(dfuResult2,bigramsum=bgsum$bigramsum)%>%cbind(unigramsum=ugsum$unigramsum)
        
        
}

print("predictSentence")
predictSentence<-function(myinput = "house is")
{
        dfuResult2<-buildProbabilityTable(testGram=myinput)
        #call to the result table for doing the model
        
        drR<-data.frame(term=dfuResult2$term, prob_result=(dfuResult2$occurrences.x/dfuResult2$tgsum.x)*.6+
                                (dfuResult2$occurrences.y/dfuResult2$bigramsum)*.3+
                                (dfuResult2$occurrences/dfuResult2$unigramsum)*.1) %>% arrange(desc(prob_result))
        return(drR)        
}

filterNGrams<-function(x="",ngDataFrame=NULL){
        
        return(filter(ngDataFrame,term==x)$term)
}
print("predictSentence loaded")

print("textHelpers.R sourced")