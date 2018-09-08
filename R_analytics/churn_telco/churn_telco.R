#remove old data
rm(list = ls())

#objective build predict churn

#load libraries
for (package in c('ggplot2', 'corrplot','tidyverse',
                  "cowplot",'lubridate','data.table','caret','mlbench','xgboost','plotrix')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}


#load data
df <- read_csv(file.choose())

#data backup
df.backup <- df

#summary 
print(summary(df))

#check for missing data
missing_data <- apply(df, 2, function(x) any(is.na(x))) #no missing data
print(missing_data)

#check for balanced data
ggplot(data=df, aes(x=factor(`Churn?`))) +
  geom_bar() + theme_classic() + xlab("Churn Amount") #data is imbalanced

#TRY data balancing - SKIP DATA BALANCE
# 
# library(unbalanced)
# 
# n<-ncol(df)
# output<-df$`Churn?`
# output<-as.factor(output)
# input<-  df[ ,-n]
# #View(input)
# 
# #Balance the Dataset using ubSMOTE#
# data<-ubBalance(X= input, Y=output, type="ubSMOTE", percOver=200, percUnder=200, k = 5,verbose=TRUE)
# #View(data)
#                 
# #Balanced Data#
# balancedData<-cbind(data$X,data$Y)
# #View(balancedData)
# 
# #check data balance again
# ggplot(data=balancedData, aes(x=factor(`data$Y`))) +
#   geom_bar() + theme_classic() + xlab("Churn Amount")

