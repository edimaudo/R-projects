# =======================================================
# packages
# =======================================================
rm(list=ls())
packages <- c('ggplot2', 'corrplot','tidyverse','dplyr','tidyr',
              'caret','mlbench','scales','proxy','reshape2',
              'caTools','doParallel','scales','catboost', 'Matrix',
              'stringr','reshape2','purrr','lubridate','tidytext','gridExtra',
              'wordcloud2','stringr','tm','SnowballC')
# load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#load data
df <- read.csv(file.choose(),sep = ",")

glimpse(df)

#drop columns
df <- df%>%
  select(-c(X,X.1,X.2,X.3))

#check for missing data
missing_data <- apply(df, 2, function(x) any(is.na(x)))
print(missing_data)

#check columns
unique(df$attraction_name)
unique(df$category)

#columns to drop attraction_name, category, reviewer_name, 
df <- df %>%
  select(reviewer_location, review_total_contributions,
         date_of_experience, review_language, review_text, rating) %>%
  na.omit()

table(df$rating) # lots of 4 and 5

#clean up date
df$date_of_experience <- lubridate::mdy(df$date_of_experience)
df$year <- lubridate::year(df$date_of_experience)
df$month <- lubridate::month(df$date_of_experience)

review_ratings_df <- df %>%
  select(review_text, rating)

cts_df <- df %>%
  select(review_total_contributions)

cat_df <- df %>%
  select(reviewer_location, review_language)

other_cat_df <- df %>%
  select(year,month)

#Label Encoder
labelEncoder <-function(x){
  as.numeric(factor(x))-1
}
#normalize data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

cts_df <- as.data.frame(lapply(cts_df, normalize))
cat_df  <- as.data.frame(lapply(cat_df , labelEncoder))

#combine information
new_df <- cbind(cat_df,cts_df,other_cat_df,review_ratings_df)

#split into train and test
set.seed(123)
sample <- sample.split(new_df,SplitRatio = 0.75)
train <- subset(new_df,sample ==TRUE)
test <- subset(new_df, sample==FALSE)

train_df <- train %>%
  select(review_text, rating)

test_df <- test %>% 
  select(review_text, rating)

clean_data <- function(df){
  corpus_df <- VCorpus(VectorSource(df$review_text))
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
  dtm_df_matrix <- cbind(dtm_df_matrix,df$rating)
  colnames(dtm_df_matrix)[ncol(dtm_df_matrix)] <- "y"
  final_df <- as.data.frame(dtm_df_matrix)
  final_df$y <- as.factor(final_df$y)
  return (final_df)
}

train_df <- clean_data(train_df)
test_df <- clean_data(test_df)

train <- cbind(train[,c(1,2,3,4,5)], train_df)
test <- cbind(test[,c(1,2,3,4,5)], test_df)

train <- train %>%
  na.omit()

test <- test %>%
  na.omit()

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

# test data accuracy
# Make predictions
predicted.classes <- fit.xgb %>% predict(test)
output <- confusionMatrix(data = predicted.classes, reference = test$y, mode = "everything")