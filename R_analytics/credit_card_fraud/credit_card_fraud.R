
# Fraud Model - These models are being used to know if a particular transaction 
# is a fraudulent transaction. Historical data having details of fraud and non-fraud 
# transactions can be used to build a classification model that would predict chances of 
# fraud happening in a transaction.


#remove old data
rm(list=ls())

#load libraries
for (package in c('ggplot2','caret','mlbench', 'dplyr', 'caTools')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#load data
df <- read.csv(file.choose())

df.backup <- df

#summary 
print(summary(df))

#check for missing data
missing_data <- apply(df, 2, function(x) any(is.na(x))) #no missing data
print(missing_data)

#check class
ggplot(data=df, aes(x=factor(Class))) +
  geom_bar() + theme_classic() + xlab("Class Information")

#fix imbalanced data
library(unbalanced)
n<-ncol(df)
output<- df$Class
output<-as.factor(output)
input<- df[ ,-n]

#Balance the Dataset using ubSMOTE#
data<-ubBalance(X= input, Y=output, type="ubSMOTE", percOver=300, percUnder=150, verbose=TRUE)
                
#Balanced Data#
balancedData<-cbind(data$X,data$Y)
View(balancedData)

balancedData <- as.data.frame(balancedData)

#update class name
balancedData <- balancedData %>%
  rename(Class = `data$Y`)

#View new data
ggplot(balancedData, aes(Class)) +
  geom_bar() + theme_classic() + xlab("Class Information")

df <- balancedData
df$Time <- NULL

#normalize data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}


df$Amount <- normalize(df$Amount)

df <- as.data.frame(df)
library(gbm)

inTrainingset<-createDataPartition(df$Class, p=.75, list=FALSE)
training<-df[inTrainingset,]
rest<-df[-inTrainingset,]
set.seed(123)

gbmfit<-gbm(Class~., data=df,
            distribution="bernoulli",n.trees=5,interaction.depth=7,shrinkage=.01)

ctrl<-trainControl(method="repeatedcv",repeats=5)
gbmTune<-train(Class~.,data=training, 
               method="gbm", 
               verbose=FALSE, 
               trControl=ctrl)


