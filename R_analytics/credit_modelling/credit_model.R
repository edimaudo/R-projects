#remove all data
rm(list=ls())

#load libraries
library(tidyverse)
library(lubridate)
library(stringr)
library(ggplot2)
library(colorspace)

#list of files
file_names = c(
  "LoanStats_2016Q1.csv",
  "LoanStats_2016Q2.csv",
  "LoanStats_2016Q3.csv",
  "LoanStats_2016Q4.csv",
  "LoanStats_2017Q1.csv",
  "LoanStats_2017Q2.csv",
  "LoanStats_2017Q3.csv",
  "LoanStats3a.csv",
  "LoanStats3b.csv",
  "LoanStats3c.csv",
  "LoanStats3d.csv"
)

#update working directory
setwd("//Users/edima/Documents/Coding/R/R_analytics/credit_modelling/") 

#clean filenames
filenames <- gsub("\\.csv$","", list.files(pattern="\\.csv$"))
#load the files
for(i in filenames){
  assign(i, read.csv(paste(i, ".csv", sep="")))
}

#combine data
lending_club <- rbind(LoanStats_2016Q1,LoanStats_2016Q2,LoanStats_2016Q3,
                      LoanStats_2016Q4,LoanStats_2017Q1,LoanStats_2017Q2,
                      LoanStats_2017Q3,LoanStats3a,LoanStats3b,LoanStats3c,LoanStats3d)
#backup
lending_club.orig <- lending_club

#clean data
glimpse(lending_club)

lending_club <- lending_club %>%
  select(loan_status, loan_amnt,term, int_rate, grade, emp_length, 
         home_ownership, annual_inc, application_type)

#recode columns
lending_club <- lending_club%>%
  filter(loan_status %in% c("Fully Paid","Default"))

lending_club$loan_status <- as.integer(recode_factor(lending_club$loan_status, "Default" = "1",
                                                     "Fully Paid" = "2"))

lending_club$term <- as.integer(recode_factor(lending_club$term, "36 months" = "36",
                                              "60 months" = "60"))
lending_club$int_rate <- as.integer(lending_club$int_rate)
lending_club$grade <- as.integer(recode_factor(lending_club$grade, "A" = "1", "B" = "2",
                                               "C" = "3", "D" = "4",
                                               "E" = "5", "F" = "6", "G" = "7"))
lending_club$emp_length <- as.integer(recode_factor(lending_club$emp_length, 
                                                    "n/a" = "1","< 1 year" = "2",
                                               "1 year" = "3", "2 years" = "4",
                                               "3 years" = "5", "4 years" = "6", 
                                               "5 years" = "7", 
                                               "6 years" = "8", "7 years" = "9",
                                               "8 years" = "10", "9 years" = "11",
                                               "10+ years" = "12"))
lending_club$home_ownership <- as.integer(recode_factor(lending_club$home_ownership, "NONE" = "1",
                                              "OTHER" = "2","RENT" = "3",
                                              "MORTGAGE" = "4","OWN" = "5",
                                              "ANY" = "6"))
lending_club$application_type <- as.integer(recode_factor(lending_club$application_type,
                                                          "Individual" = "1",
                                              "Joint App" = "2"))

#recode as factor
lending_club$loan_status <- as.factor(lending_club$loan_status)
lending_club$term <- as.factor(lending_club$term)
lending_club$grade <- as.factor(lending_club$grade) 
lending_club$emp_length <- as.factor(lending_club$emp_length)
lending_club$home_ownership <- as.factor(lending_club$home_ownership) 
lending_club$application_type <- as.factor(lending_club$application_type) 


#-----------------
#models
#-----------------
library(mlbench)
library(caret)
#cross fold validation
control <- trainControl(method="repeatedcv", number=10, repeats=3)
set.seed(123)
#logistic regression
fit.glm <- train(loan_status~., data=lending_club, method="glm", trControl=control)
#decision trees
fit.cart <- train(loan_status~., data=lending_club, method="rpart", trControl=control)
#LDA
fit.lda <- train(loan_status~., data=lending_club, method="lda", trControl=control)
#svm
fit.svm <- train(loan_status~., data=lending_club, method="svmRadial", trControl=control)
#random forest
fit.rf <- train(loan_status~., data=lending_club, method="rf", trControl=control)
#bagged cart
fit.treebag <- train(dloan_status~., data=lending_club, method="treebag", trControl=control)
#boosting algorithm - Stochastic Gradient Boosting (Generalized Boosted Modeling)
fit.gbm <- train(loan_status~., data=lending_club, method="gbm", trControl=control)

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
