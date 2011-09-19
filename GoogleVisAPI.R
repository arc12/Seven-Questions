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
## Essentially a combination of text processing code from PrepTagClouds.R and
## Sentiments.R with the output changed to create an interactive "app" combining
## both term clouds and radar plots based on the Google VisAPI
## ---------- term clouds
## Process responses from the Seven Questions approach to eliminate stopwords,
## including optional additional stopwords applicable to the scope of the study.
## Weightings are the number of times the word appears.
## No stemming!
## Minimum threshold of occurrences for display in the term cloud is the median
## Output is created separately for each question aggregated over all responses 
## and for an aggregation of all responses for all questions.
## --------- sentiment analysis
## Use the Harvard Inquirer word lists to score sets of responses against several sentiments.
## Do this per-question (for all respondants)
## Also do for an aggregate of all responses to all questions
## No stopwords used
## Count each occurrence of each word as a score of 1 for every word in the sentiment list.
## Standardised according to the number of words in each sentiment and normalised so that each plot has a max of 1
## ----------------------------------------
library("tm")
library("slam")
library("brew")
library("rjson")

##
## Run parameters
##
extra.stopwords<-NULL
#c("learning", "educational", "technology","technologies") #these were v frequent but not evenly distributed over questions to kept. adjust according to the source data. 
data.name <- "ALT-C 2011"
base.dir <- "/home/arc1/R Projects"
sentiments.file<-"Sentiments_8.csv"

##
## preliminaries
##
# directory names
brew.dir<-paste(base.dir,"Seven Questions",sep="/")
base.output.dir<-paste(base.dir,"Seven Questions Output",data.name,sep="/")
output.dir<-paste(base.output.dir,"VisAPI",sep="/")
data.dir<-paste(base.dir,"Seven Questions Data",sep="/")
# ensure output directories exist (inside the root output dirs)
dir.create(base.output.dir, showWarnings=FALSE)
dir.create(output.dir, showWarnings=FALSE)

##
## Read in the sentiment word lists (cols extracted from the Harvard Inquirer spreadsheet)
## and process to obtain a list of dictionaries. The first col is the word and the rest are
## the sentiments.
##
inquirer.table<-read.csv(paste(base.dir,"Seven Questions",sentiments.file,sep="/"),
                         header=TRUE,sep=",",quote="\"", stringsAsFactors=FALSE)
sentiment.dics<-list()
# for each sentiment, find out which words are relevant and for cases where there is more
# than one usage (denoted #1 in the word), select only the first one as this is the most frequent in general
for(i in 2:length(inquirer.table[1,])){
   dic<-inquirer.table[,"Entry"]
   dic<-dic[inquirer.table[,i]!=""]#limit to words for sentiment
   dic<-sub("#1","",dic)#remove '#1' from any words containing it
   dic<-dic[-grep("#",dic)]#remove all words still containing #
   sentiment.dics[[i-1]]<-dic
   names(sentiment.dics)[[i-1]] <- colnames(inquirer.table)[i]
}
sentiment.count<-length(sentiment.dics)

##
## Read in responses
##
# column headings are assumed to be as follows (NB questions must be cols 2:8, not using column names)
#Timestamp,Questions for the Oracle about 2025,A Favourable Outcome by 2025,An Unfavourable Outcome,How Will Culture and Institutions Need to Change,Lessons from the Past,Decisions to be Made and Actions Taken,If You Had a “Magic Wand”...,Which of the following do you see yourself as being,In which sectors do you work
setwd(data.dir)
responses<-read.csv(paste(data.name,".csv", sep=""),header=TRUE,sep=",",quote="\"", stringsAsFactors=FALSE)
n.responses<-length(responses[,1])
# !!!!!!! should probably clean up the responses, although if the source is via Google spreadsheets they seem OK without
#for output
question.titles<-c(gsub("[.]"," ",colnames(responses)[2:8]),"All Questions")

##
## Process responses A - for term clouds
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
names(occur.v)<-question.titles
# filter out terms that do not occur the medium number of times
displayed.terms<-NULL#used to pass term-response tookup to JavaScript
for(i in 1:8){
   occur.v[[i]]<-occur.v[[i]][occur.v[[i]]>median(occur.v[[i]])]  
   displayed.terms<-c(displayed.terms,names(occur.v[[i]]))
}
#de-dupe
displayed.terms<-levels(as.factor(displayed.terms))
   
##
## Process responses B - for sentiment analysis
##
# array (later a data frame) to store sentiment scores,
#     one row for each Q and then one more for the union; one column for each sentiment
scores<-array(0,c(8,sentiment.count))
# OUTER LOOP over sentiments
for(sent.i in 1:sentiment.count){
   dic<-tolower(sentiment.dics[[sent.i]])
   # INNER LOOP creates doc-term-matrices for each question. NB the dictionary is limited according to the sentiment word list
   for(q.i in 1:7){
      dtm.tf<-DocumentTermMatrix(Corpus(VectorSource(responses[,q.i+1])),
      control=list(stemming=FALSE, stopwords=NULL, minWordLength=3,
                   removeNumbers=TRUE, removePunctuation=TRUE,
                   dictionary=dic))
      #the sentiment score is simply a sum over all responses and all terms for each question divided by the number if terms in the dictionary (so that sentiments with many terms do not dominate)
      scores[q.i,sent.i]<-sum(row_sums(dtm.tf))/length(dic)
   }
   #and the same for an aggregation over all questions, which is the "eighth question"
   scores[8,sent.i]<-sum(scores[1:7,sent.i])
}
print("Before normalisation such that the max sentiment score for any question is 1")
for(q.i in 1:7){
   print(colnames(responses)[q.i])
   print(summary(scores[q.i,]))
}
#scale each ROW separately so that the plots show a sentiment balance per Q
for(q.i in 1:8){
   scores[q.i,]<-scores[q.i,]/max(scores[q.i,])
}
# #make the scores a dataframe to carry names into the plot routine
# df.scores<-as.data.frame(scores)
# #set the column names to match the sentiment CSV headings
# colnames(df.scores)<-names(sentiment.dics)
# #and the row names as the question text
# rownames(df.scores)<-question.titles

##
## Now stuff it all into an HTML page and Google Gadget, mostly as JavaScript
##
setwd(output.dir)
isGadget=FALSE
brew(file=paste(brew.dir,"Brew Template - visapi.html",sep="/"),
     output="index.html",run=TRUE)
isGadget=TRUE
brew(file=paste(brew.dir,"Brew Template - visapi.html",sep="/"),
     output="gadget.xml",run=TRUE)
