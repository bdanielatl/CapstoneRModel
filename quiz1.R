#read in the english files into objects
#requuire the tm library
library(NLP)
library(tm)
library(reader)
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
#read in data sources
twitterFileCon<-file("data/en_US.twitter.txt",open="r")
blogsFileCon<-file("data/en_US.blogs.txt",open="r")
newsFileCon<-file("data/en_US.news.txt",open="r")


twitterfile <- readLines(twitterFileCon)
blogsfile<-readLines(blogsFileCon)
newsfile<-readLines(newsFileCon)

close(twitterFileCon)
close(blogsFileCon)
close(newsFileCon)

#write function to consume the different files
#findLongestLine(twitterfile)
findLongestLine<-function (sourceFile){
        m<-0
        for (i in 1:length(sourceFile)){
                # print(i)
                # print(twitterfile[i])
                if (nchar(sourceFile[i]) > m ){
                        m <-nchar(sourceFile[i]) 
                        mchar<-sourceFile[i]
                }
        }
        return (c(m,mchar))
}






# while(current_line < twitter_length ){
#            #use a fair coin flip to determine if we will actually read this particular line
#                  if(rbinom(1,1,.5)==1){
#                          if(current_line == 1){
#                                  current_line=2
#                          }
#                          line<-scan(con,skip = current_line-1,
#                                     nlines=1,blank.lines.skip=TRUE,what=character)
# 
#                          df<-rbind(df,data.frame(corpusdata<-data.frame(line)))
#                  }
#         current_line = current_line+1
# }


#close(con)

