# 8K loan applications on kiva.org
# Country, gender, loan amount, en, status
# Labeled, historical training data
# Possible tasks:
# Predict whether loan will be repaid
# Topic Modeling

#libraries
library(corrplot)
library(tidyverse)
library(lubridate)

# remove old data
rm(list=ls())

#load data
kiva_info <- read.csv(file.choose())

#backup file
kiva.backup <- kiva_info

#====================
#exploratory analysis
#====================
#status
# ggplot(kiva_info, aes(x=reorder(as.factor(status), -table(status)[status]))) + 
#   geom_bar(fill="steelblue")+
#   theme_minimal()
# 
# #sector
# ggplot(kiva_info, aes(x=reorder(as.factor(sector), -table(sector)[sector]))) + 
#   geom_bar(fill="red")+
#   theme_minimal()
# 
# #country
# ggplot(kiva_info, aes(x=reorder(as.factor(country), -table(country)[country]))) + 
#   geom_bar(fill="green")+
#   theme_minimal()
# 
# #gender
# ggplot(kiva_info, aes(x=reorder(as.factor(gender), -table(gender)[gender]))) + 
#   geom_bar(fill="purple")+
#   theme_minimal()
# 
# #loan amount
# ggplot(kiva_info, aes(loan_amount)) + geom_histogram() + theme_minimal()
# 
# #non payment
# ggplot(kiva_info, aes(x=reorder(as.factor(nonpayment), -table(nonpayment)[nonpayment]))) + 
#   geom_bar(fill="steelblue")+
#   theme_minimal()


#---------------------
#prediction
#---------------------
#create new data frame
kiva_no_text <- kiva_info

#-----------
#recode data
#-----------
#recode to integer
recodeKIVA <- function (kiva_text){
  kiva_no_text <- kiva_text
  #recode status
  kiva_no_text$status <- as.integer(recode_factor(kiva_no_text$status, "defaulted" = "0", "paid" = "1"))
  #recode country
  kiva_no_text$country <- as.integer(recode_factor(kiva_no_text$country, "Dominican Republic" = "0",
                                                   "Ecuador" = "1", "Kenya" = "2"))
  #recode gender
  kiva_no_text$gender <- as.integer(recode_factor(kiva_no_text$gender, "M" = "0", "F" = "1"))
  #recode nonpayment
  kiva_no_text$nonpayment <- as.integer(recode_factor(kiva_no_text$nonpayment, "lender" = "0", "partner" = "1"))
  #recode sector
  kiva_no_text$sector <- as.integer(recode_factor(kiva_no_text$sector, 
                                                  "Agriculture" = "0", "Food" = "1","Services" = "2", 
                                                  "Construction" = "3", 
                                                  "Clothing" = "4", "Retail" = "5","Arts" = "6", "Food" = "7",
                                                  "Manufacturing" = "8", "Housing" = "9",
                                                  "Transportation" = "10", 
                                                  "Wholesale" = "11","Education" = "12", 
                                                  "Entertainment" = "13","Personal Use" = "14"))
 
  return (kiva_no_text)
}

#----------------
#recode to factors
#----------------
recodetoFactor <- function(df) {
  kiva_no_text <- df
  kiva_no_text$status <- as.factor(kiva_no_text$status)
  kiva_no_text$gender <- as.factor(kiva_no_text$gender)
  kiva_no_text$sector <- as.factor(kiva_no_text$sector)
  kiva_no_text$country <- as.factor(kiva_no_text$country)
  kiva_no_text$nonpayment <- as.factor(kiva_no_text$nonpayment)
  
  return (kiva_no_text)
  
}

#-----------
#prediction without text column
#-----------
kiva_no_text <- recodeKIVA(kiva_no_text)
#remove text column
kiva_no_text[,3] <- NULL
#check for correlation
corrplot(cor(kiva_no_text),method="number")
#convert to factors
kiva_no_text <- recodetoFactor(kiva_no_text)

#-----------------
#models
#-----------------
library(mlbench)
library(caret)
#cross fold validation
control <- trainControl(method="repeatedcv", number=10, repeats=3)
set.seed(123)
#logistic regression
fit.glm <- train(status~., data=kiva_no_text, method="glm", trControl=control)
#decision trees
fit.cart <- train(status~., data=kiva_no_text, method="rpart", trControl=control)
#LDA
fit.lda <- train(status~., data=kiva_no_text, method="lda", trControl=control)
#svm
fit.svm <- train(status~., data=kiva_no_text, method="svmRadial", trControl=control)
#random forest
fit.rf <- train(status~., data=kiva_no_text, method="rf", trControl=control)
#bagged cart
fit.treebag <- train(status~., data=kiva_no_text, method="treebag", trControl=control)
#boosting algorithm - Stochastic Gradient Boosting (Generalized Boosted Modeling)
fit.gbm <- train(status~., data=kiva_no_text, method="gbm", trControl=control)

#------------------
#compare models
#------------------
results <- resamples(list(logistic = fit.glm, cart = fit.cart, lda = fit.lda, 
                          svm = fit.svm, randomforest = fit.rf, 
                          baggedcart = fit.treebag, gradboost = fit.gbm))
summary(results)

# boxplot comparison
bwplot(results)
# Dot-plot comparison
dotplot(results)

# density plots of accuracy
scales <- list(x=list(relation="free"), y=list(relation="free"))
densityplot(results, scales=scales, pch = "|")

# pair-wise scatterplots of predictions to compare models
splom(results)

#statistical significance for algorithms
# difference in model predictions
diffs <- diff(results)
# summarize p-values for pair-wise comparisons
summary(diffs)

#-----------------
#text analysis
#-----------------
library(tm)
library(SnowballC)
library(wordcloud)
library(topicmodels)

#create new data frame
kiva_text <- kiva_info
#recode to int values
kiva_text <- recodeKIVA(kiva_text)
#convert to factors
kiva_text <- recodetoFactor(kiva_text)

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

kiva_text$en <- text.clean(kiva_text$en)

#==================
#topic clustering & word cloud
#==================
library(tidytext)
library(stringr)
library(tidyr)
library(tm)
library(topicmodels)
library(slam)

review_corpus = Corpus(VectorSource(kiva_text$en))
review_corpus = tm_map(review_corpus, content_transformer(tolower))
review_corpus = tm_map(review_corpus, removeNumbers)
review_corpus = tm_map(review_corpus, removePunctuation)
review_corpus = tm_map(review_corpus, removeWords, c("the", "and", stopwords("english")))
review_corpus =  tm_map(review_corpus, stripWhitespace)

review_dtm <- DocumentTermMatrix(review_corpus)

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
              Gibbs = LDA(tdm, k = k, method = 'Gibbs',control = list(seed = SEED, burnin = 1000,thin = 100, iter = 1000)),
              CTM = CTM(tdm, k = k,control = list(seed = SEED,var = list(tol = 10^-4), em = list(tol = 10^-3))))
#To compare the fitted models we first investigate the values of the models fitted with VEM and estimated and with VEM and fixed 
sapply(CSC_TM[1:2], slot, 'alpha')
sapply(CSC_TM, function(x) mean(apply(posterior(x)$topics, 1, function(z) - sum(z * log(z)))))
Topic <- topics(CSC_TM[['VEM']], 1)
Terms <- terms(CSC_TM[['VEM']], 8)

#word cloud
review_dtm = removeSparseTerms(review_dtm, 0.99)
findFreqTerms(review_dtm, 1000)
freq = data.frame(sort(colSums(as.matrix(review_dtm)), decreasing=TRUE))
wordcloud(rownames(freq), freq[,1], max.words=50, colors=brewer.pal(1, "Dark2"))




