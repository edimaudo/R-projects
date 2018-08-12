#Business background
#We have a list of existing customers which contains various data points for each of them. 
#The data for each customers consists of 3 categorical attributes and two numerical attributes. 
#The list currently consists of 800+ customers but will grow over time to 80k. 

#The provided list is structured as followed:
#Customer ID, Categorical Attribute #1, Categorical Attribute #2, 
#Categorical Attribute #3, Numeric Attribute #1, Numeric Attribute #2, 
#Belongs to Group #1, Belongs to Group #2

#Objective
#Goal is to predict who would be in the different categories

#remove old data
rm(list = ls())

#load libraries
for (package in c('ggplot2', 'corrplot','tidyverse',
                  "cowplot",'lubridate','data.table','caret','mlbench','xgboost','plotrix')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

df <- read.csv(file.choose())

#data summary
print(summary(df))

#rename columns
names(df) <- c('CustomerID', 'DocumentType', 'ExpectedVolume', 'Industry', 
               'FileFirstDays', 'Stacks', 'SubscriptionPlan', 'Subscription')

print(summary(df))

#drop columns due to lack of information
df$SubscriptionPlan <- NULL
df$Industry <- NULL

#drop customer information
df$CustomerID <- NULL

#correlation between cts variables
corinfo <- cor(df[,4:6])
corrplot(corinfo,method='number')

#check for balanced data
ggplot(data=df, aes(x=factor(Subscription))) +
  geom_bar() + theme_classic() + xlab("Type of subscription") #lots more non subscriptions
#model would definitely lean towards that

#recode categorical variables
library(dummies)
df_main_category <- df %>%
  select(DocumentType, ExpectedVolume)
df_main_category.new <- dummy.data.frame(df_main_category, sep = ".")

#drop value from one of the newly created columns to prevent multicollinearity
df_main_category.new$ExpectedVolume.NULL <- NULL
df_main_category.new$DocumentType.others <- NULL

df <- df %>%
  select(FileFirstDays, Stacks, Subscription)

df_main <- cbind(df_main_category.new, df)



#normalize data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

df_main$FileFirstDays <- normalize((df_main$FileFirstDays))
df_main$Stacks <- normalize((df_main$Stacks))

#feature selection
#Remove Redundant Feature remove when absolute correlation >= 0.75
set.seed(7)
# # calculate correlation matrix
correlationMatrix <- cor(df_main[,1:21])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)
#no columns to be dropped

#machine learning

df_main$Subscription <- as.factor(df_main$Subscription) #change to factor
#split data in train and test
library(caTools)
set.seed(123)

sample <- sample.split(df_main,SplitRatio = 0.75)
train <- subset(df_main,sample ==TRUE)
test <- subset(df_main, sample==FALSE)

#models
model_gbm<-train(train[,1:21],train[,22],method='gbm')
model_rf<-train(train[,1:21],train[,22],method='rf')
model_nnet<-train(train[,1:21],train[,22],method='nnet')

#finetune model
fitControl <- trainControl(
   method = "repeatedcv",
   number = 5,
   repeats = 5)

modelLookup(model='gbm')
#Creating grid
grid <- expand.grid(n.trees=c(10,20,50,100,500,1000),shrinkage=c(0.01,0.05,0.1,0.5),n.minobsinnode = c(3,5,10),
interaction.depth=c(1,5,10))

#Training the model using grid
model_gbm2<-train(train[,1:21],train[,22],method='gbm',
                 trControl=fitControl,tuneGrid=grid)
#Summarizing the model
print(model_gbm2)

#using tune length
model_gbm3<-train(train[,1:21],train[,22],method='gbm',trControl=fitControl,tuneLength=10)
print(model_gbm3)

#Checking variable importance for GBM
#Variable Importance
varImp(object=model_gbm)
varImp(object=model_gbm2)
varImp(object=model_gbm3)

#Evaluation of trained Algorithm (or Model) and result
#Predictions
predictions<-predict.train(object=model_gbm,test[,22],type="raw")
table(predictions)
confusionMatrix(predictions,test[,22])
