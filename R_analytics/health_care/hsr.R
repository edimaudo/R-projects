# 2.4K wordsInfo on US health care reform
# Labeled by humans as either positive or negative relative to a certain entity:
#   hcr, liberals, other, stupak, dems, teaparty, conservatives, gop, obama
# Possible task: Predict whether a tweet supports health care reform?
#   For building your model:
#   Can use the labels for training a classifier
# Can ignore the labels and use a lexicon
# Or a combination
# For assessing your model:
#   Use the labels to compute confusion matrix
# Goal of this project will be twofold:
#   To explain what makes a great model
# To show how people feel about HCR

# remove old data
rm(list=ls())

#load data
hcr <- read.csv(file.choose())

#add headers
colnames(hcr) <- c("tweet_id","other_info","sentiment","entity","tweet_info")

#create backup
hcr.backup <- hcr

#=================
#exploratory analysis
#=================
#entity
ggplot(hcr, aes(x=reorder(as.factor(entity), -table(entity)[entity]))) + 
  geom_bar(fill="steelblue")+
  theme_minimal()

#sentiment
ggplot(hcr, aes(x=reorder(as.factor(sentiment), -table(sentiment)[sentiment]))) + 
  geom_bar(fill="steelblue")+
  theme_minimal()

#------------------
#sentiment analysis
#------------------
library(RTextTools)
library(e1071)
library(tm)
library(SnowballC)

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

#clean text
hcr$tweet_info <- text.clean(hcr$tweet_info)

library(tidyverse)

#sentiment analysis
positive_words <- hcr %>%
  filter(sentiment == "positive") %>%
  select(tweet_info,sentiment)

negative_words <- hcr %>%
  filter(sentiment == "negative") %>%
  select(tweet_info,sentiment)

test <- hcr %>%
  sample_frac(size = 0.2, replace = FALSE, weight = NULL, .env = NULL) %>%
  select(tweet_info,sentiment)

wordsInfo = rbind(positive_words, negative_words, test)

#build document term matrix
# build dtm
matrix= create_matrix(wordsInfo[,1], language="english", 
                      removeStopwords=FALSE, removeNumbers=TRUE, 
                      stemWords=FALSE)

#naive bayes model
# train the model
mat = as.matrix(matrix)
classifier = naiveBayes(mat[1:10,], as.factor(wordsInfo[1:10,2]) )
# test the validity
predicted = predict(classifier, mat[11:15,]); predicted
table(wordsInfo[11:15, 2], predicted)
recall_accuracy(wordsInfo[11:15, 2], predicted)

# build the data to specify response variable, training set, testing set.
container = create_container(matrix, as.numeric(as.factor(wordsInfo[,2])),
                             trainSize=1:10, testSize=11:15,virgin=FALSE)


#Second, to train the model with multiple machine learning algorithms:
models = train_models(container, algorithms=c("MAXENT" , "SVM", "RF", "BAGGING", "TREE"))

results = classify_models(container, models)

  
# accuracy table
table(as.numeric(as.factor(wordsInfo[11:15, 2])), results[,"FORESTS_LABEL"])
table(as.numeric(as.factor(wordsInfo[11:15, 2])), results[,"MAXENTROPY_LABEL"])

# recall accuracy
recall_accuracy(as.numeric(as.factor(wordsInfo[11:15, 2])), results[,"FORESTS_LABEL"])
recall_accuracy(as.numeric(as.factor(wordsInfo[11:15, 2])), results[,"MAXENTROPY_LABEL"])
recall_accuracy(as.numeric(as.factor(wordsInfo[11:15, 2])), results[,"TREE_LABEL"])
recall_accuracy(as.numeric(as.factor(wordsInfo[11:15, 2])), results[,"BAGGING_LABEL"])
recall_accuracy(as.numeric(as.factor(wordsInfo[11:15, 2])), results[,"SVM_LABEL"])

  
# model summary
analytics = create_analytics(container, results)
summary(analytics)
head(analytics@document_summary)
#analytics@ensemble_summar

#To cross validate the results:
N=4
set.seed(2014)
cross_validate(container,N,"MAXENT")
cross_validate(container,N,"TREE")
cross_validate(container,N,"SVM")
cross_validate(container,N,"RF")