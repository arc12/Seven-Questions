## ***Made available using the The MIT License (MIT)***
# Copyright (c) 2011, Adam Cooper
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
# 
# The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
## ************ end licence ***************
## ----------------------------------------
## Process responses from the Seven Questions approach to eliminate stopwords,
## including additional stopwords applicable to the scope of the study.
## Output type A is text for feeding into wordle "advanced" in the form word:weight
## e.g. fruitbats:133
## Weightings are the number of times the word appears.
## Output type B is XML for tagcloud.swf and HTML embedding the SWFs
## (see http://www.roytanck.com/2008/05/19/how-to-repurpose-my-tag-cloud-flash-movie/)
## No stemming!
## Minimum threshold of occurrences for display in the tag cloud is the median
## Output is created separately for each question aggregated over all responses 
## and for an aggregation of all responses for all questions.
## ----------------------------------------
library("tm")
library("slam")
library("brew")

##
## Run parameters
##
extra.stopwords<- c("learning", "educational", "technology","technologies") #these dominated. adjust according to the source data
data.name <- "ALT-C 2011"
base.dir <- "/home/arc1/R Projects"

##
## preliminaries
##
# directory names
brew.dir<-paste(base.dir,"Seven Questions",sep="/")
output.dir<-paste(base.dir,"Seven Questions Output",data.name,sep="/")
data.dir<-paste(base.dir,"Seven Questions Data",sep="/")
wordle.dir<-paste(output.dir,"Wordle",sep="/")
flash.dir<-paste(output.dir,"Flash",sep="/")
# ensure output directories exist (inside the root output dirs)
dir.create(output.dir, showWarnings=FALSE)
dir.create(wordle.dir, showWarnings=FALSE)
dir.create(flash.dir, showWarnings=FALSE)

##
## Read in responses
##
# column headings are assumed to be as follows (NB questions must be cols 2:8, not using column names)
#Timestamp,Questions for the Oracle about 2025,A Favourable Outcome by 2025,An Unfavourable Outcome,How Will Culture and Institutions Need to Change,Lessons from the Past,Decisions to be Made and Actions Taken,If You Had a “Magic Wand”...,Which of the following do you see yourself as being,In which sectors do you work
setwd(data.dir)
responses<-read.csv(paste(data.name,".csv", sep=""),header=TRUE,sep=",",quote="\"", stringsAsFactors=FALSE)

##
## Process responses
##
stop.words<-c(stopwords(language = "en"),extra.stopwords)
# build a vector of doc-term-matrices, each one being for all responses against a given question
dtm.tf.v<-list()
for(i in 1:7){
   dtm.tf.v[[i]]<-DocumentTermMatrix(Corpus(VectorSource(responses[,i+1])),
  control=list(stemming=FALSE, stopwords=stop.words, minWordLength=3, removeNumbers=TRUE, removePunctuation=TRUE))
}
#and the same for an aggregation over all questions
dtm.tf.all<-c(dtm.tf.v[[1]],dtm.tf.v[[2]],dtm.tf.v[[3]],dtm.tf.v[[4]],dtm.tf.v[[5]],
              dtm.tf.v[[6]],dtm.tf.v[[7]])
#sum the columns to obtain term occurrences per set of responses
occur.v<-lapply(dtm.tf.v, col_sums)
# quesion no 8 is the union over all actual questions
occur.v[[8]]<-col_sums(dtm.tf.all)
# members of occur.v get the question title from the CSV or "all questions"
names(occur.v)<-c(gsub("[.]"," ",colnames(responses)[2:8]),"All Questions")
# filter out terms that do not occur the medium number of times
for(i in 1:8){
   occur.v[[i]]<-occur.v[[i]][occur.v[[i]]>median(occur.v[[i]])]  
}

##
## Write out A: wordle input data in tag:weight form
##
setwd(wordle.dir)
for(i in 1:8){
   write.table(occur.v[i],
               paste(names(occur.v)[i],".txt",sep=""),
               quote=FALSE,sep=":", col.names=FALSE)
}

##
## Write out B: XML for tagcloud.swf and wrapping HTML
##
# tagcloud.swf and swfobject.js are assumed to be in ../../{data.name}/Flash/
setwd(flash.dir)
# build data frames for each tag cloud
# use the same scaling across all single-question clouds such that the largest tag is drawn 24pt
scale<-24/max(unlist(lapply(occur.v[1:7],max)))
for(i in 1:7){
   len=length(occur.v[[i]])
   tags.df<-data.frame(tag=names(occur.v[[i]]), weight=scale*as.numeric(occur.v[[i]]),
                        link=rep("#",len), color=rep("#000000",len),
                        hicolor=rep("#ff00ff",len),stringsAsFactors=FALSE)
   brew(file=paste(brew.dir,"Brew Template - tagcloud.xml",sep="/"),
        output=paste("tagcloud_",i,".xml",sep=""),run=TRUE)
}
# for the all-questions case, use a different scaling that again => max 24pt
occur.all<-occur.v[[8]]
scale<-24/max(occur.all)
len<-length(occur.all)
tags.df<-data.frame(tag=names(occur.all), weight=scale*as.numeric(occur.all),
                        link=rep("#",len),
                        color=rep("#000000",len), hicolor=rep("#ff00ff",len),
                        stringsAsFactors=FALSE)
brew(file=paste(brew.dir,"Brew Template - tagcloud.xml",sep="/"),
     output="tagcloud_8.xml",run=TRUE)
#HTML     
brew(file=paste(brew.dir,"Brew Template - tagcloud.html",sep="/"),
     output="index.html",run=TRUE)