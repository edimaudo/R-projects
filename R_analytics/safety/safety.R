#objective
#build a text classifier in ( R ) for safety observations to classify the (object) and (failure mode).

# remove old data
rm(list=ls())

#libraries
library(corrplot)
library(tidyverse)
library(lubridate)
library(wordcloud2)
library(data.table)
library(stringr)
library(mlbench)
library(caret)
library(xgboost)
library(tidytext)

#load safety data
df <- read.csv(file.choose(), stringsAsFactors = FALSE)

#check for missing data
missing_data <- apply(df, 2, function(x) any(is.na(x))) #no missing data

#==============
#visualization
#==============

#visualize object
df %>%
  count(Object, sort = TRUE) %>%
  filter(n > 50) %>%
  ggplot(aes(Object, n)) +
  geom_col() +
  ylab("Count") +
  xlab("Object information") + theme_classic()

#visualize failure
df %>%
  count(Failure, sort = TRUE) %>%
  filter(n > 50) %>%
  ggplot(aes(Failure, n)) +
  geom_col() +
  ylab("Count") +
  xlab("Failure information") + theme_classic()



library(tm)
library(SnowballC)
library(wordcloud)
library(topicmodels)
library(slam)
library(tidyr)

#clean text
#text cleaning function
text.clean = function(x)                    # text data
{ 
  x  =  gsub("<.*?>", " ", x)               # regex for removing HTML tags
  x  =  iconv(x, "latin1", "ASCII", sub="") # Keep only ASCII characters
  x  =  gsub("[^[:alnum:]]", " ", x)        # keep only alpha numeric 
  x  =  tolower(x)                          # convert to lower case characters
  x  =  removeNumbers(x)                    # removing numbers
  x  =  stripWhitespace(x)                  # removing white space
  x  =  gsub("^\\s+|\\s+$", "", x)          # remove leading and trailing white space
  return(x)
}

df$Text <- text.clean(df$Text)

#==============
#world cloud
#==============
review_corpus = Corpus(VectorSource(df$Text))
review_corpus = tm_map(review_corpus, content_transformer(tolower))
review_corpus = tm_map(review_corpus, removeNumbers)
review_corpus = tm_map(review_corpus, removePunctuation)
review_corpus = tm_map(review_corpus, removeWords, c("the", "and", stopwords("english")))
review_corpus =  tm_map(review_corpus, stripWhitespace)

review_dtm <- DocumentTermMatrix(review_corpus)

review_dtm = removeSparseTerms(review_dtm, 0.99)
findFreqTerms(review_dtm, 1000)
freq = data.frame(sort(colSums(as.matrix(review_dtm)), decreasing=TRUE))
wordcloud(rownames(freq), freq[,1], max.words=200, colors=brewer.pal(1, "Dark2"))

#==============
#topic clustering
#==============





#==============
#build classifier
#==============
