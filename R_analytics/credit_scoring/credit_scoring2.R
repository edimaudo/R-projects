#goal build credit scoring model
# The class variable (i.e. the variable we want to predict) is called BAD. 
# There are 9 more variables in the dataset you were provided with in addition 
# to these described in the table. Each of these variables is encoded as M and 
# the name of one of the main variables: for example, M MORTDUE, or M DEBTINC. 
# All the M variables are binary (i.e. take values in {0, 1}). 
# They were created because the original dataset contained a large number of missing values. 
# For each variable that had missing values in the original data (e.g. MORTDUE) the missing 
# values were replaced, and a binary variable (M MORTDUE) was created what indicates whether 
# the value of the variable was missing in the original dataset (M MORTDUE=1) or 
# not (M MORTDUE=0). In other words, the value of a variable like DEBTINC is the actual, 
# observed, value when M DEBTINC=0. When M DEBTINC=1 the value of DEBTINC has 
# been predicted (and therefore does not correspond to the actual value of this variable 
#                 for that customer).
# # 
# data dictionary
# name
# BAD
# LOAN 
# MORTDUE 
# VALUE 
# REASON 
# JOB
# YOJ 
# DEROG 
# DEBTINC 
# CLAGE 
# NINQ 
# CLNO 
# DELINQ
# 
# Definition
# 1=applicant defaulted on loan or seriously delinquent, 0=applicant paid loan 
#Amount of the loan request
# Amount due on existing mortgage
# Value of current property
# Not Provided; DebtCon=debt consolidation; HomeImp=home improvement Occupational categories
# Years at present job
# Number of major derogatory reports
# Debt-to-income ratio
# Age of oldest credit line in months 
# Number of recent credit inquiries 
# Number of credit lines
# Number of delinquent credit lines

#remove all data
rm(list=ls())

mydata <- read.csv(file.choose()) #using file ending with .dat

mydata.orig <- mydata



#exploratory analysis for some variables
library(ggplot2)
ggplot(mydata, aes(x=factor(BAD)))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  theme_classic()

ggplot(mydata, aes(x=factor(REASON)))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  theme_classic()

ggplot(mydata, aes(x=factor(JOB)))+
  geom_bar(stat="count", width=0.7, fill="steelblue")+
  theme_classic()

#recode reason and job
library(tidyverse)

mydata$REASON <- as.integer(recode_factor(mydata$REASON, "NotProvided" = "1",
                                                "DebtCon" = "2","HomeImp"="3" ))

mydata$JOB <- as.integer(recode_factor(mydata$JOB, "NotProvided" = "1",
                                       "Other" = "2","Self"="3",
                                       "Mgr" = "4","ProfExe"="5",
                                       "Sales" = "6","Office"="7"))


#correlation plot
library(corrplot)

corrplot(cor(mydata),method="number")

#-----------------
#models
#-----------------
library(mlbench)
library(caret)
#cross fold validation
control <- trainControl(method="repeatedcv", number=10, repeats=3)
set.seed(123)
mydata$BAD <- as.factor(mydata$BAD)
#logistic regression
fit.glm <- train(BAD~., data=mydata, method="glm", trControl=control)
#decision trees
fit.cart <- train(BAD~., data=mydata, method="rpart", trControl=control)
#LDA
fit.lda <- train(BAD~., data=mydata, method="lda", trControl=control)
#svm
fit.svm <- train(BAD~., data=mydata, method="svmRadial", trControl=control)
#random forest
fit.rf <- train(BAD~., data=mydata, method="rf", trControl=control)
#bagged cart
fit.treebag <- train(BAD~., data=mydata, method="treebag", trControl=control)
#boosting algorithm - Stochastic Gradient Boosting (Generalized Boosted Modeling)
fit.gbm <- train(BAD~., data=mydata, method="gbm", trControl=control)

#------------------
#compare models
#------------------
results <- resamples(list(logistic = fit.glm, cart = fit.cart, lda = fit.lda, 
                          svm = fit.svm, randomforest = fit.rf, 
                          baggedcart = fit.treebag, gradboost = fit.gbm))
summary(results)

# boxplot comparison
bwplot(results)
# Dot-plot comparison
dotplot(results)

# density plots of accuracy
scales <- list(x=list(relation="free"), y=list(relation="free"))
densityplot(results, scales=scales, pch = "|")

# pair-wise scatterplots of predictions to compare models
splom(results)

#statistical significance for algorithms
# difference in model predictions
diffs <- diff(results)
# summarize p-values for pair-wise comparisons
summary(diffs)