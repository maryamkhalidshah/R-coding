###Assignment 2

###Load required packages
#-------------------------------------------
library(tm)
library(stringr)
library(wordcloud)
library(quanteda)
library(SnowballC)
library(arules)
library(proxy)
library(cluster)
library(stringi)
library(Matrix)
library(tidytext) 
library(plyr) 
library(ggplot2)
library(factoextra) 
library(mclust) 
library(gofastr) 
#-------------------------------------------

#-------------------------------------------
###Load in the documents (the Corpus)
FedPapersCorpus <- Corpus(DirSource("FedPapers")) 
(getTransformations())

#checking the number of documents in our corpus
(ndocs <- length(FedPapersCorpus)) #there are 85 docs in our folder

summary(FedPapersCorpus) #to check whether all the docs have been read in

#inspect a particular document
writeLines(as.character(FedPapersCorpus[[3]]))

#ignore extremely rare words i.e. words that appear in less than 1% of
#the documents. 
(minTermFreq <- ndocs * 0.0001)

# ignore overly common words i.e. terms that appear in more than 50%
#of the documents
(maxTermFreq <- ndocs * 1)

(MyStopwords <- c("federalist","author","constitution","newyork",
                  "government","state","power","people",
                  "nation","must","states","upon","every",
                  "might","shall","time","less","number",
                  "part","york","new")) #stopwords
#-------------------------------------------

###Pre-processing the data (data cleaning)
#-------------------------------------------
toSpace <- content_transformer(function(x,pattern){
  return(gsub(pattern,"",x))
})

FedPapersCorpus <- tm_map(FedPapersCorpus, toSpace, "-")
FedPapersCorpus <- tm_map(FedPapersCorpus, toSpace, ":")
FedPapersCorpus <- tm_map(FedPapersCorpus, toSpace, ";")
FedPapersCorpus <- tm_map(FedPapersCorpus, toSpace, "'")
FedPapersCorpus <- tm_map(FedPapersCorpus, toSpace, " -")

#Good practice to check after each step
writeLines(as.character(FedPapersCorpus[[3]]))

#Remove punctuation
FedPapersCorpus <- tm_map(FedPapersCorpus, removePunctuation)

#Transform to lower case (because R is case sensitive)
FedPapersCorpus <- tm_map(FedPapersCorpus,
                          content_transformer(tolower))

#Strip digits
FedPapersCorpus <- tm_map(FedPapersCorpus, removeNumbers)

#Remove stopwords from standard stopword list 
FedPapersCorpus <- tm_map(FedPapersCorpus, removeWords,stopwords("english"))

#Remove custom stopwords
FedPapersCorpus <- tm_map(FedPapersCorpus, removeWords,MyStopwords)

#Strip whitespace
FedPapersCorpus <- tm_map(FedPapersCorpus, stripWhitespace)

#Inspect output
writeLines(as.character(FedPapersCorpus[[3]]))
#-------------------------------------------

###Part 1 - Create Document Term Matrix & DF & Inspect segment
#-------------------------------------------
FedPapers_dtm <- DocumentTermMatrix(FedPapersCorpus,control = list(
  wordLengths=c(4,10), 
  bounds=list(global=c(minTermFreq,maxTermFreq),
              stemming=F,stemWords=TRUE,
              remove_separators=TRUE,stem=TRUE)
))

#Inspect segment of dtm - have to disable arules package for inspect
#function to work and not throw an error.
inspect(FedPapers_dtm[1:4,1:5])


## Convert to DF
FedPapers_DF <- as.data.frame(as.matrix(FedPapers_dtm))
str(FedPapers_DF)
(FedPapers_DF$said)
(nrow(FedPapers_DF))
#-------------------------------------------

###Part 2 - Print top 10 most/least frequent words
#-------------------------------------------
#Total count of each term
freq <- colSums(as.matrix(FedPapers_dtm))
freq

#Length should be total number of terms
length(freq)

#Sort freqs in desc order. Create sort order
ord <- order(freq,decreasing = TRUE)

#Print top 10 most frequent words
(freq[head(ord, n=10)])

#Print top 10 least frequent words
(freq[tail(ord, n=10)])
#-------------------------------------------

###Part 3 - Print number of words in each document
#-------------------------------------------
#Row Sums
(Row_Sum_Per_doc <- rowSums((as.matrix(FedPapers_dtm))))
length(Row_Sum_Per_doc) #to check if all docs included

#Sort in desc order. Create sort order
row_order <- order(Row_Sum_Per_doc,decreasing = TRUE)

#Print top 10 documents with the most words
(Row_Sum_Per_doc[head(row_order, n=10)])

#Print top 10 documents with the least words
(Row_Sum_Per_doc[tail(row_order, n=10)])
#-------------------------------------------

###Part 4 - Creating wordclouds
#-------------------------------------------
#Convert to matrix and view
FedPapers_dtm_matrix = as.matrix(FedPapers_dtm)
str(FedPapers_dtm_matrix)
(FedPapers_dtm_matrix[c(1:5),c(2:6)])

rownames(FedPapers_dtm_matrix) #to check Hamilton files
FedPapers_dtm_matrix[12:65,1] #Hamilton Files

rownames(FedPapers_dtm_matrix) #to check Madison files
FedPapers_dtm_matrix[71:85,1] #Madison Files

#Create wordcloud for Hamilton
wordcloud(colnames(FedPapers_dtm_matrix),
          FedPapers_dtm_matrix[12:65, ],
          scale = c(4, 0.2),max.words = 100,
          min.freq = 10,
          vfont=c("sans serif","plain"),colors=palette())

#Create wordcloud for Madison
wordcloud(colnames(FedPapers_dtm_matrix),
          FedPapers_dtm_matrix[71:85, ],
          scale = c(3, 0.2),max.words = 100,
          min.freq = 10,
          vfont=c("sans serif","plain"),colors=palette())

##Looking at the two wordclouds, we can see that Hamilton
##repeats words less often than Madison, who seems to be a
##big fan of repetition. 
#-------------------------------------------

