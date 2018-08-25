#objective
#analyze safety information

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
library(tm)
library(SnowballC)
library(wordcloud)
library(topicmodels)
library(slam)
library(tidyr)

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
# create tf-idf matrix
tdm <- review_dtm
term_tfidf <- tapply(tdm$v/row_sums(tdm)[tdm$i], tdm$j, mean) * log2(nDocs(tdm)/col_sums(tdm > 0))
summary(term_tfidf)
tdm <- tdm[,term_tfidf >= 0.1]
tdm <- tdm[row_sums(tdm) > 0,]
summary(col_sums(tdm))
#Deciding best K value using Log-likelihood method
best.model <- lapply(seq(2, 50, by = 1), function(d){LDA(tdm, d)})
best.model.logLik <- as.data.frame(as.matrix(lapply(best.model, logLik)))
#calculating LDA
k = 50;#number of topics
SEED = 7000;
CSC_TM <-list(VEM = LDA(tdm, k = k, control = list(seed = SEED)),
              VEM_fixed = LDA(tdm, k = k,control = list(estimate.alpha = FALSE, seed = SEED)),
              Gibbs = LDA(tdm, k = k, method = 'Gibbs',control = list(seed = SEED, burnin = 1000,
                                                                      thin = 100, iter = 1000)),
              CTM = CTM(tdm, k = k,control = list(seed = SEED,var = list(tol = 10^-4), 
                                                  em = list(tol = 10^-3))))
#To compare the fitted models we first investigate the values of the models
#fitted with VEM and estimated and with VEM and fixed 
sapply(CSC_TM[1:2], slot, 'alpha')
sapply(CSC_TM, function(x) mean(apply(posterior(x)$topics, 1, function(z) - sum(z * log(z)))))
Topic <- topics(CSC_TM[['VEM']], 1)
Terms <- terms(CSC_TM[['VEM']], 8)
