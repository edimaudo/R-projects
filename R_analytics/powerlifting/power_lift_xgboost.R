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

