# •50K movie reviews from IMDB–No more than 30 reviews for a given movie–Reviews contain many sentences
# •Labeled (from the author of the review)–Positive = 7 or higher–Negative = 4 or lower–25K positive reviews, 
# 25K negative reviews
# •Possible task: doc classification/sentiment analysis (predict the scores)?–For building your model:
# •Can use the labels for training a classifier
# •Can ignore the labels and use a lexicon
# •Or a combination–For assessing your model:
#   •Use the labels to compute accuracy, confusion matrix, etc.
# •Possible task: clustering

# remove old data
rm(list=ls())

#load data
temp <- suppressWarnings(read_delim(file.choose(),delim = "||",col_names = FALSE))

#drop columns
temp[,6] <- NULL
temp[,4] <- NULL
temp[,2] <- NULL

#add headers
colnames(temp) <- c("movie_id","rating","sentiment","text_info")

# #clean headers
# temp$rating <- NULL
# temp$text_info <-NULL
# colnames(temp)[colnames(temp)=="sentiment"] <- "rating"
# names(temp)[3]<-"sentiment"
# names(temp)[5]<-"text_info"
# temp[,4] <- NULL

#=====================
#exploratory analysis
#=====================
library(ggolot2)
library(tidyverse)
#barplot of ratings
#sorted
ggplot(temp, aes(x=reorder(as.factor(rating), -table(rating)[rating]))) + geom_bar() + theme_classic()

#barplot of sentiment
ggplot(temp, aes(x=reorder(as.factor(sentiment), -table(sentiment)[sentiment]))) + geom_bar() + theme_classic()

#====================
#analysis
#====================

library(tidytext)
library(tm)
#function to clean text_info

#sentiment analysis

#doc classification

#clustering based on text info to give possibly type of movie and rating

#word cloud