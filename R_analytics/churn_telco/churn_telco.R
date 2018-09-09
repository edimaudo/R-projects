#remove old data
rm(list = ls())

#objective build predict churn

#load libraries
for (package in c('ggplot2', 'corrplot','tidyverse','caret','mlbench')) {
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

#check churn data
ggplot(data=df, aes(x=factor(`Churn?`))) +
  geom_bar() + theme_classic() + xlab("Churn Amount")

#transform data

#drop Phone column as not needed
df$Phone <- NULL

#glimpse(df)

#split columns into categorical and cts data and predicted variable
df_category <- df %>%
  select(State, `Area Code`,`Int'l Plan`,`VMail Plan`)

#continuous
df_continuous <- select(df, -col(df_category))
df_continuous$`VMail Plan` <- NULL
df_continuous$`Churn?` <- NULL

#normalize data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

df_continuous <- as.data.frame(lapply(df_continuous, normalize))

#update column names
df_category <- df_category %>%
  rename(Area_Code = `Area Code`, Intl_Plan = `Int'l Plan`, VMail_Plan = `VMail Plan`) %>%
  mutate(State = as.factor(State), Area_Code = as.factor(Area_Code), 
         Intl_Plan = as.factor(Intl_Plan), VMail_Plan = as.factor(VMail_Plan))

library(dummies)
df_category.new <- dummy.data.frame(as.data.frame(df_category), sep = "_")

#churn column
df_churn <- df$`Churn?`

#combine data
df_combine <- cbind(df_continuous, df_category.new, df_churn)

#find which features are not important
set.seed(1)
# # calculate correlation matrix
correlationMatrix <- cor(df_combine[,1:72])
# # summarize the correlation matrix
# print(correlationMatrix)
# # find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# # print indexes of highly correlated attributes
print(highlyCorrelated)

#drop columns
df_combine <- df_combine[,-c(highlyCorrelated)]

#build model
df_combine$df_churn <- recode_factor(df_combine$df_churn, "False." = 0, "True." = 1)

#split data into train and test
set.seed(1)
library(caTools)
sample <- sample.split(df_combine,SplitRatio = 0.75)
training <- subset(df_combine,sample ==TRUE)
test <- subset(df_combine, sample==FALSE)

#model
predictor <- training[,1:64]
predicted <- training[,65]
model_gbm<-train(predictor,predicted,method='gbm')

#Predictions
predictions<-predict.train(object=model_gbm,test,type="raw")
#table(predictions)
confusionMatrix(predictions,test[,65])
