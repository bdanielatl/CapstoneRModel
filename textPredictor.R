require(tm)
require(SnowballC)
require(RWeka)
require(data.table)
require(stringr)


source(file="textPredictorLib.R")

set.seed(23456)

#call initialization
initialization()
#only have to do this if the temp data files do not exist
sampleAndWriteTexts(dataSourcePath="data/final/en_US/en_US.blogs.txt")
sampleAndWriteTexts(dataSourcePath="data/final/en_US/en_US.news.txt")
sampleAndWriteTexts(dataSourcePath="data/final/en_US/en_US.twitter.txt")


corpora <- PCorpus(DirSource("temp/"),readerControl=list(reader=readPlain),dbControl=list(useDB=TRUE,dbName="./trainDB",dbType="DB1"))


corpora <- tm_map(corpora,content_transformer(removePunctuation))
corpora <- tm_map(corpora,content_transformer(removeNumbers))
corpora <- tm_map(corpora,content_transformer(stemDocument))
corpora <- tm_map(corpora,content_transformer(stripWhitespace))
corpora <- tm_map(corpora,removeWords, profanity)
corpora <- tm_map(corpora, PlainTextDocument)


UnigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1)) 
BigramTokenizer <- function(x) NGramTokenizer(x,Weka_control(min = 2, max = 2)) 
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3)) 

#bigrams
print("processing bigrams")


controls_B <- list( stemming = function(word) wordStem(word, language = "english"),tokenize=BigramTokenizer) 
trainTDM_B<- TermDocumentMatrix(corpora,controls_B)
removeSparseTerms(trainTDM_B,.2)

#analyze the trainTDM 
#first compute the total number of frequencies of each word across all documents
freq_B <- rowSums(as.matrix(trainTDM_B))  #we use rowSums as the terms are along the rows
#create a dataframe
wf_B <- data.frame(word=names(freq_B),count=freq_B,stringsAsFactors=FALSE)
#find the start and end of bigrams 
biL <- str_split(wf_B$word," ")
wf_B$start <- sapply(biL,FUN=function(x) x[1]) #starting unigram
wf_B$end <- sapply(biL,FUN=function(x) x[2]) #ending unigram
#setkey(wf_B,key="start")

#trigrams
print("processing trigrams")

controls_T <- list(stemming = function(word) wordStem(word, language = "english"),tokenize=TrigramTokenizer) 
trainTDM_T<- TermDocumentMatrix(corpora,controls_T)
removeSparseTerms(trainTDM_T,.2)

#analyze the trainTDM 
#first compute the total number of frequencies of each word across all documents
freq_T <- rowSums(as.matrix(trainTDM_T))  #we use rowSums as the terms are along the rows
#create a dataframe
wf_T <- data.frame(word=names(freq_T),count=freq_T,stringsAsFactors=FALSE)
#expland the wf_T DF -- store the starting bigram and the ending word
triL <- str_split(wf_T$word," ")
wf_T$start <- sapply(triL,FUN=function(x) paste(x[1],x[2]))
wf_T$end <- sapply(triL,FUN=function(x) x[3])
#setkey(wf_T,key="start")


simpleGT <- function(table_N){
        #Simple Good Turing Algorithm - Gale And Simpson
        #Good Turing Smoothing
        
        # table_U is a table of frequency of frequencies
        # The frequencies are stored as names of the list in the table structure
        # the values are the frequency of frequencies.
        # In Good Turing Smoothing, we are concerned with the frequency of frequencies
        # So, to extract the number of times that words of frequency n occur in the training set, we need to do:
        # table(freq_B)[[as.character(pairCount)]]
        # In a tables with a number of holes or non-contiguous sequence of frequency of words,
        # we can compute N(c+1) as the mean of all frequencies that occur more than Nc times
        # to do this, create a vector that is in the numerical form of the names of the table
        
        # create a data table
        # r is the frequencies of various trigrams
        #n is the frequency of frquencies
        SGT_DT <- data.frame(r=as.numeric(names(table_N)),n=as.vector(table_N),Z=vector("numeric",length(table_N)), 
                             logr=vector("numeric",length(table_N)),
                             logZ=vector("numeric",length(table_N)),
                             r_star=vector("numeric",length(table_N)),
                             p=vector("numeric",length(table_N)))
        #p=vector("numeric",length(table_N)),key="r")
        
        str(SGT_DT)
        
        num_r <- nrow(SGT_DT)
        for (j in 1:num_r) {
                if(j==1) {r_i<-0} else {r_i <- SGT_DT$r[j-1]}
                if(j==num_r){r_k<-SGT_DT$r[j]} else {r_k <- SGT_DT$r[j+1]}
                SGT_DT$Z[j] <- 2*SGT_DT$n[j] / (r_k-r_i)
                #print(paste(r_i,j,r_k))
        }
        SGT_DT$logr <- log(SGT_DT$r)
        SGT_DT$logZ <- log(SGT_DT$Z)
        linearFit <- lm(SGT_DT$logZ ~ SGT_DT$logr)
        print(linearFit$coefficients)
        c0 <- linearFit$coefficients[1]
        c1 <- linearFit$coefficients[2]
        
        plot(SGT_DT$logr, SGT_DT$logZ)
        abline(linearFit,col="red")
        
        use_y = FALSE
        for (j in 1:(num_r-1)) {
                r_plus_1 <- SGT_DT$r[j] + 1
                
                s_r_plus_1 <- exp(c0 + (c1 * SGT_DT$logr[j+1]))
                s_r <- exp(c0 + (c1 * SGT_DT$logr[j]))
                y<-r_plus_1 * s_r_plus_1/s_r
                
                if(use_y) {
                        SGT_DT$r_star[j] <- y
                } else { 
                        n_r_plus_1 <- SGT_DT$n[SGT_DT$r == r_plus_1]
                        n_r <- SGT_DT$n[j]
                        x<-(r_plus_1) * n_r_plus_1/n_r
                        
                        if (abs(x-y) > 1.96 * sqrt(((r_plus_1)^2) * (n_r_plus_1/((n_r)^2))*(1+(n_r_plus_1/n_r)))) {
                                SGT_DT$r_star[j] <- x
                        }else {
                                SGT_DT$r_star[j] <- y
                                use_y = TRUE
                        }
                }
                if(j==(num_r-1)) {
                        SGT_DT$r_star[j+1] <- y
                }
                
        }
        N <- sum(SGT_DT$n * SGT_DT$r)
        Nhat <- sum(SGT_DT$n * SGT_DT$r_star)
        Po <- SGT_DT$n[1] / N
        SGT_DT$p <- (1-Po) * SGT_DT$r_star/Nhat
        
        return(SGT_DT)
        
}

tri_SGT_DT <- simpleGT(table(freq_T))
bi_SGT_DT <- simpleGT(table(freq_B))

predict <- function(sentence) {
        
        
        # given a sentence/phrase, extract the last two words
        sl <- unlist(str_split(sentence," "))
        len <- length(sl)
        
        bigram <- paste(sl[len-1],sl[len])
        
        # get the subset of the trigram data table witha matching bigram start
        swf_T <- wf_T[wf_T$start == bigram,]
        #check if bigram was found in the trigram table
        if(nrow(swf_T) > 0) {
                # use the counts in the Simple GT table to extract the probability
                swf_T$p <- sapply(swf_T$count,FUN=function(x) tri_SGT_DT$p[tri_SGT_DT$r==x])
                # order by probability
                #swf_T <- swf_T[with(swf_T,order(-p))]
                # find the largest probability
                maxP <-max(swf_T$p)
                #get the end words with the highest probability
                predictList <- swf_T$end[swf_T$p == maxP]
                predictions <- vector()
                for(i in 1:length(predictList)) {
                        predictions[i] <- paste(sentence,predictList[i])
                }
                return(predictions)
                #pl_T <- data.frame(words=swf_T$end,probs=swf_T$p)
                #return(pl_T)
        } else {
                print(paste("No match for bigram",bigram,"in",sentence,"--looking for unigram match"))
                unigram <- sl[len]
                swf_B <- wf_B[wf_B$start == unigram]
                if(nrow(swf_B) > 0) {
                        # use the counts in the Simple GT table to extract the probability
                        swf_B$p <- sapply(swf_B$count,FUN=function(x) bi_SGT_DT$p[bi_SGT_DT$r==x])
                        # order by probability
                        swf_B <- swf_B[with(swf_B,order(-p))]
                        # find the largest probability
                        maxP <-max(swf_B$p)
                        #get the end words with the highest probability
                        predictList <- swf_B$end[swf_B$p == maxP]
                        for(i in 1:length(predictList)) {
                                predictions[i] <- paste(sentence,predictList[i])
                        }
                        pl_B <- data.frame(words=swf_B$end,probs=swf_B$p)
                        return(pl_B[1:10,])
                } else {
                        print(paste("No match for unigram",unigram,"in",sentence))
                }
        }
        
}