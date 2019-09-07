#remove old data
rm(list=ls())
#packages
packages <- c('ggplot2', 'corrplot','tidyverse','caret','mlbench','mice', 'caTools','dummies')
#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

df <- read.csv(file.choose(), header = FALSE) #adult data
test <- read.csv(file.choose(), header = FALSE)

nameinfo <- c('age','workclass','fnlwgt','education','education_num',
              'marital_status','occupation','relationship','race',
              'sex','capital_gain','capital_loss','hrs_week','native_country',
              'Target')

colnames(df) <- nameinfo
colnames(test) <- nameinfo

df <- rbind(df,test)

#check for missing data
missing_data <- apply(df, 2, function(x) any(is.na(x))) #no missing data
print(missing_data)


df$workclass[df$workclass=="?"] <- NA
df$occupation[df$occupation=="?"] <- NA
df$native_country[df$native_country=="?"] <- NA


#remove missing data
df <- na.omit(df)

Target <- df$Target
df_cat <- df[,c(2,4,6,7,8,9,10,14)]
df_cts <- df[,c(1,3,5,11,12,13)]

df_cat_new <- dummy.data.frame(as.data.frame(df_cat), sep = "_")

#normalize data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
df_cts <- as.data.frame(lapply(df_cts, normalize))

df_new <- cbind(df_cat_new, df_cts, Target)

#remove redundant columns
# # calculate correlation matrix
correlationMatrix <- cor(df_new[,1:length(df_new)-1])
# # summarize the correlation matrix
# print(correlationMatrix)
# # find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)
# # print indexes of highly correlated attributes
print(highlyCorrelated)

df_new <- df_new[,-c(highlyCorrelated)]

#split data
set.seed(123)
sample <- sample.split(df_new,SplitRatio = 0.75)
train <- subset(df_new,sample ==TRUE)
test <- subset(df_new, sample==FALSE)

#cross fold validation
control <- trainControl(method="repeatedcv", number=10, repeats=3)

#random forest
fit.rf <- train(Target~., data=train, method="rf", trControl=control)
#boosting algorithm - Stochastic Gradient Boosting (Generalized Boosted Modeling)
fit.gbm <- train(Target~., data=train, method="gbm", trControl=control)
#svm
fit.svm <- train(Target~., data=train, method="svmRadial", trControl=control)

#------------------
#compare models
#------------------
results <- resamples(list(gradboost = fit.gbm))

summary(fit.gbm)
# boxplot comparison
bwplot(results)
# Dot-plot comparison
dotplot(results)

test_scores <- predict(fit.gbm, test)
confusionMatrix(test_scores, test$Target)