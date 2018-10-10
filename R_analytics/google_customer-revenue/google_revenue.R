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

Clean_String <- function(extransaction){
  temp <- extransaction
  #' Remove everything that is not a number or letter 
  temp <- stringr::str_replace_all(temp,"[^$()-*#0123456789\\s]", " ")
  #remove white space on both sides
  temp <- trimws(temp,"b")
  # Shrink down to just one white space
  temp <- stringr::str_replace_all(temp,"[\\s]+", " ")
  return (temp)

}

#extract data from total column and create new columns
#train$visits <- 
#train$hits <-
#train$pageviews <-
#train$bounces <-
#train$newVisits <-

#extract data from traffic source

#extract data from geonetwork columns and create new columns

#extract data from device column and create new columns

#load test data
test <- fread(file.choose())

#load sample submission
sample_submission <- read_csv(file.choose())