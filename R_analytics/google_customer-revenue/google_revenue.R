#remove old data
rm(list=ls())

#load libraries
packages <- c('ggplot2', 'corrplot','tidyverse','caret','mlbench','mice', 'caTools', 
              'MASS','Metrics','randomForest','lars','xgboost','Matrix','methods', 
              'data.table', 'lubridate','jsonlite', 'stringr')

#load libraries
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#load data
train <- fread(file.choose())

#get summary
print(summary(train))

#check for missing data
missing_data <- apply(train, 2, function(x) any(is.na(x)))
print(missing_data) #no missing data

glimpse(train)

#convert date column to real date
train$date <- ymd(train$date)

#convert visit start time
train$visitStartTime <- as.POSIXct(train$visitStartTime, origin="1970-01-01")

#function to clean total column
Clean_String <- function(extransaction){
  temp <- extransaction
  #' Remove everything that is not a number or letter 
  temp <- stringr::str_replace_all(temp,"[^$()-*#0123456789]", " ")
  #remove white space on both sides
  temp <- trimws(temp,"b")
  # Shrink down to just one white space
  temp <- stringr::str_replace_all(temp,"[\\s]+", " ")
  return (temp)
}

#extract data from totals column
df_train_info <- data.frame()
n <- nrow(train)
for (i in 1:n){
  train_info <- stringr::str_split_fixed(Clean_String(train$totals[i])," ", 5)
  df_train_info <- rbind(df_train_info,train_info)
}

#rename df_train_info column
colnames(df_train_info) <- c("visits", "hits","pageviews", "bounces","newvisits")

#extract data from traffic source

#extract data from geonetwork columns and create new columns

#extract data from device column and create new columns

#combine data from new data frames

#drop totals, traffic, geonetwork, device columns

#create dummy variables

#normalize data

#drop core dummy variables to prevent mullticollinearity

#load test data
test <- fread(file.choose())

#recode test data similar to train data

#create model

#test model

#load sample submission
sample_submission <- read_csv(file.choose())

#update submission using new model

#tweak model based on ranking


#======================others
#string split
#stringr::str_split_fixed(x$y, "", 4)
#df %>% separate(Clean_String(train$totals[5]), c("visits", "hits","pageviews", "bounces","newvisits"))
#Separate(data,column,into=” “,sep=” ” )
#train$visits <- 
#train$hits <-
#train$pageviews <-
#train$bounces <-
#train$newVisits <-

# model <- #some processing
#   df <- data.frame(col1=model)
# 
# for (i in 2:17)
# {
#   model <- # some processing
#     nextcol <-  data.frame(model)
#   colnames(nextcol) <- c(paste("col", i, sep="")) # rename the comlum
#   df <- cbind(df, nextcol)
# }


# df_total = data.frame()
# n = nrow(train)
# for (i in 1:10){
#   test <- Clean_String(train$totals[i])
#   as.data.frame(test) %>% separate(test, into = paste("V", 1:5, sep = " "))
#   #df <- test
#   #df_total <- rbind(df_total,df)
#   
#   
# }
# 