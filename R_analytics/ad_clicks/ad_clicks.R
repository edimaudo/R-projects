#Problem Statement
#A leading affiliate network company from Europe wants to leverage machine learning to 
#improve (optimise) their conversion rates and eventually their topline. 
#Their network is spread across multiple countries in europe such as Portugal, 
#Germany, France, Austria, Switzerland etc.

#Affiliate network is a form of online marketing channel where an intermediary promotes 
#products / services and earns commission based on conversions (click or sign up). 
#The benefit companies sees in using such affiliate channels is that, 
#they are able to reach to audience which doesn’t exist in their marketing reach.
#The company wants to improve their CPC (cost per click) performance. 
#A future insight about an ad performance will give them enough headstart 
#to make changes (if necessary) in their upcoming CPC campaigns.

#Objective
#In this challenge, you have to predict the probability whether an ad will get clicked or not.


#Data Description
#You are given three files to download: train.csv, test.csv and sample_submission.csv 
#Variables in this data set are anonymized due to privacy. 
#The training data is given for 10 days ( 10 Jan 2017 to 20 Jan 2017). 
#The test data is given for next 3 days.

# Variable	Description
# ID	Unique ID
# datetime	timestamp
# siteid	website id
# offerid	offer id (commission based offers)
# category	offer category
# merchant	seller ID
# countrycode	country where affiliates reach is present
# browserid	browser used
# devid	device used
# click	target variable

#clear old data
rm(list=ls())

#load libraries
packages <- c('ggplot2', 'corrplot','tidyverse','caret','mlbench','mice', 'caTools', 
              'MASS','Metrics','randomForest','lars','xgboost','Matrix','methods', 'lubridate',
              'data.table')

for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#load data
train <- fread(file.choose())
test <- fread(file.choose())
sample_submission <- fread(file.choose())

#create backup
train.backup <- train

#review train data
glimpse(train)

#perform data cleaning and transformation
# Impute Missing values
colSums(is.na(train))
train[is.na(siteid), siteid := -999]
test[is.na(siteid), siteid := -999]

train[is.na(browserid), browserid := "None"]
test[is.na(browserid), browserid := "None"]

train[is.na(devid), devid := "None"]
test[is.na(devid), devid := "None"]

# Create Date Features
train[,datetime := as.Date(datetime, format = "%Y-%m-%d %H:%M:%S")]
test[,datetime := as.Date(datetime, format = "%Y-%m-%d %H:%M:%S")]

train[,tweekday := weekdays(datetime)]
test[,tweekday := weekdays(datetime)]

train[,thour := hour(datetime)]
test[,thour := hour(datetime)]

train[,tminute := minute(datetime)]
test[,tminute := minute(datetime)]


# Label Encoding

feats <- setdiff(colnames(train), c("ID"))

for ( f in feats)
{
  if (class(train[[f]]) == 'character')
  {
    levels <- unique(c(train[[f]], test[[f]]))
    train[[f]] <- as.numeric(factor(train[[f]], levels = levels))
    test[[f]] <- as.numeric(factor(test[[f]], levels = levels))
  }
}

# Some Aggregate Features
train[,agg1 := .N, .(siteid, offerid)]
test[,agg1 := .N, .(siteid, offerid)]

train[,agg2 := .N, .(siteid, category)]
test[,agg2 := .N, .(siteid, category)]

train[,agg3 := .N, .(siteid, merchant)]
test[,agg3 := .N, .(siteid, merchant)]




# To avoid memory troubles, lets train the model on 10% of the data - you can always increase it
trainX <- train[sample(.N, 1e6)]


# Stacking ----------------------------------------------------------------

#The following script works this way:
# 1. For every seed and a different nround value, it creates a prediction for train and test
# 2. All the train and test predictions are stored in oof_train and of_test respectively.
# 3. Finally, a logistic regression model is trained on L1 predictions.
# 4. For understanding, comments are added to help you understand what's going on. 

folds <- createFolds(trainX$click, k = 10, list = T)

#set seeds
coseed <- c(1,13,999,10001,9009)

num_rounds <- c(50,100,150,200,250)

params <- list(
  
  objective = 'binary:logistic',
  base_score = mean(trainX$click == 0),
  max_depth = 10,
  eta = 0.03,
  subsample = 0.5,
  colsample_bytree = 0.5,
  min_child_weight = 1,
  eval_metric = "auc"
  
)

# create data frame to store predictions
oof_train <- data.frame(ID = numeric(), actual = numeric(), predict = numeric(), seed = numeric())
oof_test <- data.frame(ID = test$ID)

# Start Stacking
for (i in seq(coseed))
{
  cat('Building on seed: ',coseed[i],'\n')
  
  feature.names <- setdiff(colnames(trainX), c('ID','datetime','click'))
  
  for(j in seq(folds))
  {
    cat('\n')
    cat(sprintf('Training on Fold: %d ',j),'\n')
    
    idex <- folds[[j]]
    
    X_train <- trainX[!idex]
    X_valid <- trainX[idex]
    
    cat(sprintf("Training model on: %d rows ", nrow(X_train)),'\n')
    cat(sprintf("Testing model on: %d rows", nrow(X_valid)),'\n \n')
    
    dtrain <- xgb.DMatrix(data = as.matrix(X_train[,feature.names,with=F]), label = X_train$click)
    dvalid <- xgb.DMatrix(data = as.matrix(X_valid[,feature.names,with=F]), label = X_valid$click)
    
    watchlist <- list(train = dtrain, valid = dvalid)
    
    bst <- xgb.train(params = params
                     ,data = dtrain
                     ,nrounds = as.integer(num_rounds[i])
                     ,print_every_n = 20
                     ,watchlist = watchlist
                     ,early_stopping_rounds = 40
                     ,maximize = T
                     ,nthreads = -1)
    
    preds <- predict(bst, dvalid)
    df = data.frame(ID = X_valid$ID, actual = X_valid$click, predict = preds, seed = i)
    oof_train <- rbind(oof_train, df)
    
    #cat(sprintf("Now, oof_train has %d rows and %d columns",nrow(oof_train), ncol(oof_train)),'\n')
    
    rm(bst, dtrain, dvalid, preds,df)
    gc()
    
  }
  
  cat('\n')
  cat(sprintf("After %s seed, oof_train has %s rows and %s columns",i,nrow(oof_train),ncol(oof_train)),'\n \n')
  cat("Now Training on full data........",'\n')
  
  dtest <- xgb.DMatrix(data = as.matrix(test[,feature.names,with=F]))
  dtrain <- xgb.DMatrix(data = as.matrix(trainX[,feature.names,with=F]), label = trainX$click)
  
  print ('Training model...')
  bst <- xgb.train(params = params
                   ,data = dtrain
                   ,nrounds = as.integer(num_rounds[i])
                   ,print_every_n = 20
                   ,maximize = T
                   ,nthreads = -1)
  
  print('predicting....')
  
  preds <- predict(bst, dtest)
  oof_test[[paste0("pred_seed_",i)]] <- preds
  
  cat(sprintf("oof_test has %s rows and %s columns",nrow(oof_test),ncol(oof_test)),'\n \n')
  rm(df, bst, preds)
  
}



# Now creating Level 1 Data -----------------------------------------------

oof_train$seed <- as.factor(oof_train$seed)
mastertrain <- split(oof_train, oof_train$seed)

for(x in seq(mastertrain))
{
  assign(paste0("oof_train_",x), data.table(mastertrain[[x]]))
}

setnames(oof_train_1,"predict","pred_seed_1")
setnames(oof_train_2,"predict","pred_seed_2")
setnames(oof_train_3,"predict","pred_seed_3")
setnames(oof_train_4,"predict","pred_seed_4")
setnames(oof_train_5,"predict","pred_seed_5")

mastertrain <- merge(x = oof_train_1, y = oof_train_2[,.(ID, pred_seed_2)], by = 'ID', all.x = T)
mastertrain <- merge(x = mastertrain, y = oof_train_3[,.(ID, pred_seed_3)], by = 'ID', all.x = T)
mastertrain <- merge(x = mastertrain, y = oof_train_4[,.(ID, pred_seed_4)], by = 'ID', all.x = T)
mastertrain <- merge(x = mastertrain, y = oof_train_5[,.(ID, pred_seed_5)], by = 'ID', all.x = T)
head(mastertrain)

rm(oof_train, oof_train_1, oof_train_2, oof_train_3, oof_train_4, oof_train_5,trainX,X_train,X_valid)
gc()

setnames(mastertrain,"actual","click")


# Training Model ----------------------------------------------------------

# Logistic Regression
mastertrain[,click := as.factor(click)]

lr_model <- glm(click ~ ., data = mastertrain[,-c('ID','seed'),with=F], family = binomial(link = 'logit'),maxit = 500)
summary(lr_model)

preds <- predict(lr_model, oof_test, type = 'response')

