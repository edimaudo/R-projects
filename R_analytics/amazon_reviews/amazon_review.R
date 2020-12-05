# =======================================================
# packages
# =======================================================
rm(list=ls())
packages <- c('ggplot2', 'corrplot','tidyverse','dplyr','tidyr',
              'caret','mlbench','scales','proxy','reshape2',
              'caTools','doParallel','scales','catboost', 'Matrix',
              'stringr','reshape2','purrr','lubridate','tidytext','gridExtra',
              'wordcloud2','stringr','tm','performanceEstimation')
# load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#load data
df <- read.csv(file.choose(),sep = ",")

#check columns
unique(df$Category)
unique(df$PAccuracyuct)
unique(df$Country)
unique(df$Date)
unique(df$Year)
unique(df$Verified)

#select only needed columns
df <- df %>%
  select(PAccuracyuct,Country,Month,Year, Verified, Helpful,Title, Body, Rating) %>%
  na.omit()

#check for missing values
missing_data <- apply(df, 2, function(x) any(is.na(x)))
print(missing_data)

New_df <- df %>%
  select(Body, Rating)

#Label Encoder
labelEncoder <-function(x){
  as.numeric(factor(x))-1
}

New_df2 <- df %>%
  select(Country, Month, Year)

New_df2 <- as.data.frame(lapply(New_df2, labelEncoder))

New_df <- cbind(New_df2, New_df)

#split into train and test
set.seed(123)
sample <- sample.split(New_df,SplitRatio = 0.75)
train <- subset(New_df,sample ==TRUE)
test <- subset(New_df, sample==FALSE)

train_df <- train %>%
  select(Body, Rating)

test_df <- test %>%
  select(Body, Rating)

clean_data <- function(df){
  corpus_df <- VCorpus(VectorSource(df$Body))
  ##Removing Punctuation
  corpus_df <- tm_map(corpus_df, content_transformer(removePunctuation))
  ##Removing numbers
  corpus_df <- tm_map(corpus_df, removeNumbers)
  ##Converting to lower case
  #corpus_df <- tm_map(corpus_df, content_transformer(tolower))
  ##Removing stop words
  corpus_df <- tm_map(corpus_df, content_transformer(removeWords), stopwords('english'))
  ##Stemming
  corpus_df <- tm_map(corpus_df, stemDocument)
  ##Whitespace
  corpus_df <- tm_map(corpus_df, stripWhitespace)
  # Create Document Term Matrix
  dtm_df <- DocumentTermMatrix(corpus_df)
  corpus_df <- removeSparseTerms(dtm_df, 0.8)
  dtm_df_matrix <- as.matrix(corpus_df)
  dtm_df_matrix <- cbind(dtm_df_matrix,df$Rating)
  colnames(dtm_df_matrix)[ncol(dtm_df_matrix)] <- "y"
  final_df <- as.data.frame(dtm_df_matrix)
  final_df$y <- as.factor(final_df$y)
  return (final_df)
}

train_df <- clean_data(train_df)
test_df <- clean_data(test_df)

train <- cbind(train[,c(1,2,3)], train_df)
test <- cbind(test[,c(1,2,3)], test_df)

#model training
cl <- makePSOCKcluster(4)
registerDoParallel(cl)

#cross fold validation
control <- trainControl(method="repeatedcv", number=10, repeats=3, classProbs = FALSE)

#glm
fit.glm <- train(as.factor(y)~., data=train, method="multinom", metric = "Accuracy", 
                 trControl = control)
#random forest
fit.rf <- train(as.factor(y)~., data=train, method="rf", metric = "Accuracy", 
                trControl = control)
#boosting algorithm - Stochastic Gradient Boosting (Generalized Boosted Modeling)
fit.gbm <- train(as.factor(y)~., data=train, method="gbm", metric = "Accuracy", 
                 trControl = control)
#svm
fit.svm <- train(as.factor(y)~., data=train, method="svmRadial", metric = "Accuracy", 
                 trControl = control)
#nnet
fit.nnet <- train(as.factor(y)~., data=train, method="nnet", metric = "Accuracy", 
                  trControl = control)
#naive
fit.naive <- train(as.factor(y)~., data=train, method="naive_bayes", metric = "Accuracy", 
                   trControl = control)
#extreme gradient boosting
fit.xgb <- train(as.factor(y)~., data=train, method="xgbTree", metric = "Accuracy", 
                 trControl = control)
#bagged cart
fit.bg <- train(as.factor(y)~., data=train, method="treebag", metric = "Accuracy", 
                trControl = control)
#decision tree
fit.dtree <- train(as.factor(y)~., data=train, method="C5.0", metric = "Accuracy", 
                   trControl = control)
#knn
fit.knn <- train(as.factor(y)~., data=train, method="kknn", metric = "Accuracy", 
                 trControl = control)

stopCluster(cl)

#------------------
#compare models
#------------------
results <- resamples(list(randomforest = fit.rf, 
                          `gradient boost` = fit.gbm, 
                          `support vector machine` = fit.svm,
                          baggedCart = fit.bg, 
                          neuralnetwork = fit.nnet,
                          xgboost = fit.xgb, 
                          logisticregression = fit.glm, 
                          `decision tree` = fit.dtree, 
                          `naive bayes` = fit.naive))

summary(results)
# boxplot comparison
bwplot(results)
# Dot-plot comparison
dotplot(results)

# Model accuracy
mean(predicted.classes == test$y)

# test data accuracy
# Make predictions
predicted.classes <- fit.xgb %>% predict(test)
output <- confusionMatrix(data = predicted.classes, reference = test$y, mode = "everything")