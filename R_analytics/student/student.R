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
library(mlbench)
library(caret)
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
#data exploration
#================

#gender vs. region
ggplot(data=df, aes(x=region, y=gender, fill=gender)) +
  geom_bar(stat="identity") + theme_classic()

#gender vs. highest education
ggplot(data=df, aes(x=region, y=final_result, fill=highest_education)) +
  geom_bar(stat="identity") + theme_classic()

#output
ggplot(data=df, aes(x=factor(final_result))) + geom_bar() + theme_classic()

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

#combine data
df.new <- cbind(df.new,df[,10])

#rename predicted column
df.new <- df.new %>%
  rename (final_result=`df[, 10]`)

#find redundant features
set.seed(7)
# calculate correlation matrix
correlationMatrix <- cor(df.new[,1:42])
# summarize the correlation matrix
#print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)

#drop columns
df.new[,41] <- NULL
df.new[,36] <- NULL

#convert final result to factors
df.new <- df.new %>%
  mutate(final_result = recode(final_result, "Distinction"= '1', 'Fail' = '2', 
                               'Pass' = '3', "Withdrawn" = '4'))

#split data into training and test
library(caTools)
set.seed(7)
df.new$final_result <- as.factor(df.new$final_result)
sample <- sample.split(df,SplitRatio = 0.75)
train <- subset(df.new,sample ==TRUE)
test <- subset(df.new, sample==FALSE)

#================
#predictive model
#================
predictors <- df.new[,1:40]
predict <- df.new[,41]
model_gbm<-train(predictors,predict,method='gbm')
predictions<-predict.train(object=model_gbm,test,type="raw")
table(predictions)
confusionMatrix(predictions,test[,41])
