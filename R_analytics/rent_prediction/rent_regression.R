#rent prediction

#remove old data
rm(list=ls())
#packages
packages <- c('ggplot2', 'corrplot','tidyverse','caret','mlbench','mice', 'caTools','dummies',
              'lubridate','randomForest')
#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#load data
df <- read_csv(file.choose())

df.backup <- df

#check for missing data
missing_data <- apply(df, 2, function(x) any(is.na(x)))
print(missing_data)

#remove missing data
df <- na.omit(df)

summary(df)

#drop columns
df$PostalCode <- NULL
df$`Street 1` <- NULL
df$City <- NULL

#data transformation
df$PlaceAge <-as.integer(format(Sys.Date(), "%Y")) - df$`Year Built`

#drop year build
df$`Year Built` <- NULL

#update lease date
df$LeaseEndDate <- mdy(df$LeaseEndDate)

df$LeaseDiffDays <- as.numeric((Sys.Date() - df$LeaseEndDate))
  
#drop lease Date
df$LeaseEndDate <- NULL

#convert catgeorical
df_cat <- df[,c(1,2)]
df_cat <- lapply(df_cat, function(x) as.factor(as.character(x)))
df_cat_new <- dummy.data.frame(as.data.frame(df_cat), sep = "_")

df_cts <- df[,c(4,5,6)]

#convert chr to float
df_cts <- lapply(df_cts, function(x) as.double(as.character(x)))

#normalize data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
#df_cts <- as.data.frame(lapply(df_cts, normalize))

Target <- df$RecurringCharges

df_new <- cbind(df_cat_new, df_cts, Target)

#drop 1 and 3
df_new[1] <- NULL
df-new[3] <- NULL

# #remove redundant columns
# # # calculate correlation matrix
# correlationMatrix <- cor(df_new[,1:length(df_new)-1])
# # # summarize the correlation matrix
# # print(correlationMatrix)
# # # find attributes that are highly corrected (ideally >0.75)
# highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)
# # # print indexes of highly correlated attributes
# print(highlyCorrelated)
# 
# df_new <- df_new[,-c(highlyCorrelated)]

#split data
set.seed(123)
sample <- sample.split(df_new,SplitRatio = 0.75)
train <- subset(df_new,sample ==TRUE)
test <- subset(df_new, sample==FALSE)


rmse <- function(error)
{
  sqrt(mean(error^2))
}

reg1<- lm(Target~., data = train)
summary(reg1)
#check performance of model
Prediction_1<- predict(reg1, newdata= test)
rmse(test$Target - Prediction_1)

#random forest model
reg2<- randomForest(Target~.,data= train)
summary(reg2)
#check performance
Prediction_2 <- predict(reg2, newdata= test)
rmse(Prediction_2 - test$Target)