#objective
#-Summarise and visualise the data.
#-Prepare the dataset for analysis (data cleansing, choose appropriate features to model)
#-Build model to predict final result

#data dictionary
# code_module – an identification code for a module on which the student is registered.
# code_presentation - the identification code of the presentation during which the 
#                      student is registered on the module.
# id_student – a unique identification number for the student.
# gender – the student’s gender.
# region – identifies the geographic region, where the student lived 
#          while taking the module-presentation.
# highest_education – highest student education level on entry to the module presentation.
# imd_band – specifies the Index of Multiple Depravation band of the place where the 
#            student lived during the module-presentation.
# age_band – band of the student’s age.
# num_of_prev_attempts – the number times the student has attempted this module.
# studied_credits – the total number of credits for the modules the student is currently studying.
# disability – indicates whether the student has declared a disability.
# final_result – student’s final result in the module-presentation.

# remove old data
rm(list=ls())

#libraries
library(corrplot)
library(tidyverse)
library(lubridate)
library(mlbench)
library(caret)
library(xgboost)
library(ggplot2)

#read csv file
df <- read.csv(file.choose())

#check for missing data
missing_data <- apply(df, 2, function(x) any(is.na(x)))
print(missing_data) #no missing data

#keep backup of data
df.old <- df

#remove na's
df <- na.omit(df)

#================
#visualize data
#================

#gender vs. region
ggplot(data=df, aes(x=region, y=gender, fill=gender)) +
  geom_bar(stat="identity") + theme_classic()

#gender vs. highest education
ggplot(data=df, aes(x=region, y=final_result, fill=highest_education)) +
  geom_bar(stat="identity") + theme_classic()


#================
#business review
#================
#things to remove - #- code module, #-id_student
df$code_module <- NULL
df$id_student <- NULL

#normalize columns
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

df$studied_credits <- normalize(df$studied_credits)


#create dummy variables
#- code_presentation,gender, region, highest_education imd_band, age_band, disability
library(dummies)
colinfo <- colnames(df)

df.new <- dummy.data.frame(df[,1:9], sep = ".")
df.new <- cbind(df.new,df[,10])

df.new <- df.new %>%
  rename (final_result=`df[, 10]`)

#drop columns
#presentation
df.new$ode_presentation.2013B <- NULL
#gender
df.new$gender.F <- NULL
#region
df.new$`region.East Anglian Region` <- NULL
#education
df.new$`highest_education.No Formal quals` <- NULL
#imb band
df.new$imd_band. <- NULL
#age band
df.new$`age_band.55<=` <- NULL
#disability 
df.new$disability.N <- NULL


# #find which columns are more important
# control <- trainControl(method="repeatedcv", number=10, repeats=3)
# # train the model
# model <- train(final_result~., data=df.new, method="lvq", preProcess="scale", trControl=control)
# # estimate variable importance
# importance <- varImp(model, scale=FALSE)
# # summarize importance
# print(importance)
# # plot importance
# plot(importance)

#split data into training and test
library(caTools)
set.seed(123)
df.new$final_result <- as.factor(df.new$final_result)
sample <- sample.split(df,SplitRatio = 0.75)
train <- subset(df.new,sample ==TRUE)
test <- subset(df.new, sample==FALSE)


#================
#predictive model
#================

library( 'e1071' )
model <- svm( final_result~., train )
res <- predict( model, newdata=train )
res1 <- predict( model, newdata=test )

res1.new <- as.data.frame(res1)
res.new <- as.data.frame(res)

#library(SDMTools)
#confusion.matrix(as.factor(train$final_result), as.factor(res.new$res), threshold = 0.5)
#accuracy(train$final_result, res.new, threshold = 0.5)

#confusion matrix
table(train$final_result, res.new$res)
table(test$final_result, res1.new$res1)