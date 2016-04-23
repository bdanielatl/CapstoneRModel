
#initilziation 
# - makes sure that the profanity file is in the right place
initialization<-function(){
        #make sure to get the bad-words file 
        if(!file.exists("data/bad-words.txt")){
                download.file("http://www.cs.cmu.edu/~biglou/resources/bad-words.txt",destfile="data/bad-words.txt")
        }
        #read in the profanity base
        con_bw<-file("data/bad-words.txt")
        profanity<<-readLines(con_bw)
        close(con_bw)
        
}

#read all three documents into memory random sample 1% of lines
sampleAndWriteTexts<- function(dataSourcePath="data/final/en_US/en_US.blogs.txt"){
        
        if(!file.exists(paste0("temp/",strsplit(dataSourcePath,"/")[[1]][4]))){
                ##read in the entire file and count the number of lines
                con<- file(dataSourcePath,open="r")
                
                fileLines<<-readLines(con)
                close(con)
                numLines<-length(fileLines)
                sampleLines<-numLines*.02 #use 2% of the file to train
                #sampleVector<-sample(1:numLines,round(sampleLines,0),replace=TRUE)

                writeLines(sample_lines(filename=dataSourcePath,sampleLines),
                           con=paste0("temp/",strsplit(dataSourcePath,"/")[[1]][4]))
        }
        
        
}


