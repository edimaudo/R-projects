# Write script for quick exploratory analysis to the data. In particular:
# -	Class distribution in the dataset
# -	Other relevant information about the dataset
# -	Carry out pre-processing (i.e features relation…)
# -	Drop irrelevant features (should be clearly explained and justified)
# Codes must be commented, and steps explained. Produce at least two diagrams that explains 
#the dataset in the link.
# 
# 2. Build a classification model to classify/predict the class label in the dataset 
#using any classification model 
# (any of these SVM, Random Forest, Neural Networks or any other less complex classification model).
# You must justify your choice of the method used, write, explain and comment the code produced 
#to complete this task.
# The codes must:
# 1.	Divide the set into training and testing subsets
# 2.	Build a model of choice using the training set
# 3.	Test and evaluate the model
# 4.	Report and discuss results
# Now, you are required to improve the performance of the above model and try to get better results. 
# There are different ways to improve and fine tune the model. More specifically, 
#try to explore one or more of these options below:
# 1.Fine tune the parameters of the model, or search for some existing techniques to 
#optimise these parameters
# 2.Use different metrics for evaluating the model 
# (i.e perhaps the data is not balanced, and you might want to keep trying some other methods
# 3.Use different models and compare the results
# 4.Change the partitioning of the dataset. In some cases, more training data may help
# 5.Use cross-validation if not used already
# 6.Any other methods you think might be appropriate to use


#HTRU dataset - http://archive.ics.uci.edu/ml/datasets/HTRU2

#objective

#clear old data
rm(list=ls())

#load libraries
library(tidyverse)
library(caret)
library(mlbench)
library(ggplot2)
library(corrplot)

#load HTRU data
mydata <- read.csv(file.choose())

mydata <- as.data.frame(mydata)

#name columns
names(mydata) <- c("mean_ip", "standard_deviation_ip", "excess_kurtosis_ip",
                   "skewness_ip","mean_DMSNR","standard_deviation_DMSNR",
                 "excess_kurtoisis_DMSNR","skewness_DMSNR","class")

#backup data
mydata.orig <- mydata

#summary
summary(mydata)

#correlation plot
corrplot(cor(mydata), method="number")

#find relevant features
highlyCorrelated <- findCorrelation(cor(mydata), cutoff=0.5)


mydata$class <- as.factor(mydata$class)

#find important features
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(class~., data=mydata, method="lvq", preProcess="scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)


#RFE for feature selection
set.seed(7)
# load the library
library(mlbench)
library(caret)
# load the data
data(PimaIndiansDiabetes)
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(mydata[,1:8], mydata[,9], sizes=c(1:8), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))

#modelling approach


#fine tune model
