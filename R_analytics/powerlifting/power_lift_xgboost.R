#remove old data
rm(list=ls())
#load libraries
library(tidyverse) # Makes life easy 
library(caTools) # Data Splitting
library(caret) # Modeling toolkit
library(xgboost) # Model
#load data
df <- read.csv(file.choose(), sep= ",")

#######
# Clean 
#######

# Column Names to Lower Case
colnames(df) <- tolower(colnames(df))

# Select Columns for Model 
df <- df %>% select(name,
                    sex,
                    equipment, 
                    age, 
                    bodyweightkg, 
                    bestsquatkg, 
                    bestbenchkg, 
                    bestdeadliftkg,
                    totalkg, 
                    wilks)

# Clean feature names
df <- df %>% rename(bodyweight = bodyweightkg,
                    squat = bestsquatkg,
                    bench = bestbenchkg,
                    deadlift = bestdeadliftkg,
                    total = totalkg,
                    observation_name = name,
                    response_variable = sex)


#######################################
# Preprocess: Dealing With Missing Data
#######################################

#-------------
# Are there any NaN values?
#-------------
sapply(df, function(y) sum(length(which(is.na(y)))))

#-------------
# Remove Incomplete Observations 
#-------------
df <- df[complete.cases(df), ]


#-------------
# Save to join af end
#-------------
df_final <- df

####################################
# Preprocess: Column identification
####################################
#-------------
# Observation Name Variables
#-------------
df_observation_name <- df %>% select(observation_name)

#-------------
# Categorical (Factor) Variables
#-------------
# Create a vector of all columns that are factors
categorical_features <- names(Filter(is.factor, df))

# Create a vector of all columns that are factors
df_categorical_features <- df %>% select_if(is.factor)

# Remove observation from factors vector. 
df_categorical_features <- categorical_features[categorical_features != "observation_name"]
categorical_features <- categorical_features[categorical_features != "observation_name"]

#-------------
# Numerical Variables Names
#-------------
numerical_features <- names(Filter(is.numeric, df))
df_numeric_columns <- df %>% select_if(is.numeric) 


####################################
# Preprocess: Categorical Variables
####################################

dummies <- dummyVars(~., df[categorical_features], fullRank=T)
categorical_one_hot_encoding <- predict(dummies, df[categorical_features])


####################################
# Preprocess: Numerical Variables
####################################

#-------------
# Define Numeric Scaler
#-------------
scaler <- preProcess(x = df_numeric_columns)

#-------------
# Scale Numeric Columns
#-------------
df_numeric_columns <- predict(scaler, df_numeric_columns)


####################################
# Preprocess: Ordinal Variables
####################################



####################################
# Preprocess: Bind to one data frame
####################################
df <- cbind(df_observation_name,df_numeric_columns, categorical_one_hot_encoding)


####################################
# Preprocess: Clean Before Model
####################################

#-------------
# Clean feature names
#-------------
df <- df %>% rename(response_variable = response_variable.M)

####################################
# Preprocess: Split Data
####################################

#-------------
# Split between train/test
#-------------

set.seed(123)
split <- sample.split(df$response_variable, SplitRatio = 0.80)
train <- subset(df, split == TRUE)
test <- subset(df, split == FALSE)

#-------------
# column identification
#-------------

train_y <- train %>% select(response_variable)
train_x <- train %>% select( -response_variable, -observation_name)

test_y <- test %>% select(response_variable)
test_x <- test %>% select( -response_variable, -observation_name)

#train xgboost model
dtrain <- xgb.DMatrix(data = as.matrix(train_x), label = as.matrix(train_y))
dtest = xgb.DMatrix(data =  as.matrix(test_x), label = as.matrix(test_y))


#-------------
# these are the datasets the rmse is evaluated for at each iteration
#-------------

watchlist = list(train=dtrain, test=dtest)



#-------------
# try 1 - off a set of paramaters I know work pretty well for most stuff
#-------------
xgb_model_1 = xgb.train(data = dtrain, 
                        max.depth = 5, 
                        eta = 0.01, 
                        nthread = 2, 
                        nround = 10000, 
                        watchlist = watchlist, 
                        objective = "binary:logistic", 
                        early_stopping_rounds = 50,
                        print_every_n = 500)


#feature importance
#-------------
# Print Feature Importance
#-------------
# Calculate
importance <- xgb.importance(feature_names = colnames(train_x), model = xgb_model_1)


# Create a rank variable based on importance
rankImportance <- importance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Gain))))

# Print
print(rankImportance)

#plot importance
xgb.plot.importance(importance_matrix = importance)

#validation set
#-------------
# Create validation sets
#-------------

set.seed(123)
sample = sample.int(n = nrow(train), size = floor(.8*nrow(train)), replace = F)
train_t = train[sample, ] #just the samples
valid  = train[-sample, ] #everything but the samples

#-------------
# column identification
#-------------

train_y <- train_t %>% select(response_variable)
train_x <- train_t %>% select( -response_variable, -observation_name)

valid_y <- valid %>% select(response_variable)
valid_x <- valid %>% select( -response_variable, -observation_name)

#-------------
# Put into the xgb matrix format. Will not work with a data frame.
#-------------
gb_train = xgb.DMatrix(data = as.matrix(train_x), label = as.matrix(train_y ))
gb_valid = xgb.DMatrix(data = as.matrix(valid_x), label = as.matrix(valid_y ))

#-------------
# these are the datasets the rmse is evaluated for at each iteration
#-------------

watchlist = list(train = gb_train, valid = gb_valid)



bst_slow = xgb.train(data= gb_train, 
                     max.depth = 10, 
                     eta = 0.01, 
                     nthread = 2, 
                     nround = 10000, 
                     watchlist = watchlist, 
                     objective = "binary:logistic", 
                     early_stopping_rounds = 50,
                     print_every_n = 500)

#grid search to find best tuned models
###
# Grid search first principles 
###

max.depths = c(7, 9)
etas = c(0.01, 0.001)

best_params = 0
best_score = 0

count = 1
for( depth in max.depths ){
  for( num in etas){
    
    bst_grid = xgb.train(data = gb_train, 
                         max.depth = depth, 
                         eta=num, 
                         nthread = 2, 
                         nround = 10000, 
                         watchlist = watchlist, 
                         objective = "binary:logistic", 
                         early_stopping_rounds = 50, 
                         verbose=0)
    
    if(count == 1){
      best_params = bst_grid$params
      best_score = bst_grid$best_score
      count = count + 1
    }
    else if( bst_grid$best_score < best_score){
      best_params = bst_grid$params
      best_score = bst_grid$best_score
    }
  }
}

#tuned model
bst_tuned = xgb.train( data = gb_train, 
                       max.depth = 9, 
                       eta = 0.01, 
                       nthread = 2, 
                       nround = 10000, 
                       watchlist = watchlist, 
                       objective = "binary:logistic", 
                       early_stopping_rounds = 50,
                       print_every_n = 500)

#save and load model
# Save final Model
#xgb.save(bst_tuned, 'bst_tuned.model')

# Load final model
#bst_tuned <- xgb.load('bst_tuned.model')

#test accuracy of the model using test set
# Generate predicted classes using the model object
pred_model_bst_tuned <- predict(object = bst_tuned, 
                                newdata = dtest, 
                                type="class")



# Transform the regression in a binary classification
pred_model_bst_tuned <- as.numeric(pred_model_bst_tuned > 0.5)


# Calculate Accuracy on Test set (Model has not seen this yet)
err <- mean(as.numeric(pred_model_bst_tuned> 0.5) != test_y$response_variable)
err <- 100 - err
print(paste("test-accuracy = ", err, '%'))

#perform prediction on the full dataset
#-------------
# Put into the xgb matrix format. Will not work with a data frame.
#-------------

df_features <- df %>%  select(-observation_name)
df_observation_name <- df %>%  select(observation_name)
final_x <- xgb.DMatrix(data = as.matrix(df_features), label = as.matrix(df_observation_name))

#-------------
# Generate predictions on entire data set
#-------------

final_prediction <- predict(object = bst_tuned, 
                            newdata = final_x, 
                            type="class")

#-------------
# Add prediction to entire data set
#-------------

# Add Binary Prediction
final_prediction_binary <- as.numeric(final_prediction > 0.5)
solution <- data.frame(name = df_observation_name$observation_name, 
                       predicted_binary_response_variable = final_prediction_binary)
solution <- solution %>% rename(observation_name = name)

# Join with delographc data about observation 
solution <- merge(x = solution, y = df_final, by = "observation_name", all.x = TRUE)

# Move from 0/1 to M/F
solution$predicted_binary_response_variable[solution$predicted_binary_response_variable %in% "1"] <- "M"
solution$predicted_binary_response_variable[solution$predicted_binary_response_variable %in% "0"] <- "F"
