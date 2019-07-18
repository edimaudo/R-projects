#remove old data
rm(list=ls())
#packages
packages <- c('ggplot2', 'corrplot','tidyverse','caret','mlbench','mice', 'caTools','dummies','readxl',
              'cluster','factoextra','psy','lattice','nFactors','scales','NbClust')
#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#load data
df_train <- read.table(file.choose(),sep = ",",header = TRUE)
df_test <- read.table(file.choose(),sep = ",",header = TRUE)

#get summary
summary(df_train)
summary(df_test)

#remove missing data
df_train <- na.omit(df_train)
df_test <- na.omit(df_test)

#drop unncessary columns
df_train[1] <- NULL
df_test[1] <- NULL

#recode categorical data
df_train_cat <- df_train[,c(1,2,3,4,5,11)]
df_train_cat_new <- dummy.data.frame(as.data.frame(df_train_cat), sep = "_")

df_train_cts <- df_train[,c(6,7,8,9,10)]

Loan_status <- as.factor(df_train$Loan_Status)

df_train_new <- cbind(df_train_cat_new, df_train_cts,Loan_status)

#TEST
df_test_cat <- df_test[,c(1,2,3,4,5,11)]
df_test_cat_new <- dummy.data.frame(as.data.frame(df_test_cat), sep = "_")
df_test_cts <- df_test[,c(6,7,8,9,10)]
df_test_new <- cbind(df_test_cat_new, df_test_cts)