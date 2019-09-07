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

df <- read.csv(file.choose(), header = FALSE) #autism data

glimpse(df)

#rename columns
colnames(df) <- c('A1','A2','A3','A4','A5','A6','A7','A8','A9','A10',
                  'AGE','GENDER','ETHNICITY','JUNDICE','AUTISM','COUNTRY',
                  'USED_APP','RESULT','AGE_DESC','RELATION','TARGET')

df.backup <- df

#check for missing data
missing_data <- apply(df, 2, function(x) any(is.na(x))) #no missing data
print(missing_data)

ggplot(df.backup,aes(x=as.factor(TARGET))) + geom_bar()

#age
df$AGE[df$AGE == "?"] <- NA
#Ethnicity
df$ETHNICITY[df$ETHNICITY == "?"] <- NA
#relation
df$RELATION[df$RELATION == "?"] <- NA

df <- na.omit(df)


#recode
Target <- df$TARGET
Target <- as.data.frame(Target)
Target[,1] <- ifelse(Target[,1] == "NO", '0', ifelse(Target[,1] == "YES", '1', '99'))
Target <- lapply(Target, function(x) as.factor(as.character(x)))


df_cat <- df[,c(1:10,12:17,19:20)]
df_cat <- lapply(df_cat, function(x) as.factor(as.character(x)))
df_cat_new <- dummy.data.frame(as.data.frame(df_cat), sep = "_")

df_cts <- df[,c(11,18)]
df_cts$AGE <- as.double(df_cts$AGE)
df_cts$RESULT <- as.double(df_cts$RESULT)
#normalize data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
df_cts <- as.data.frame(lapply(df_cts, normalize))

df_new <- cbind(df_cts,df_cat_new, Target)

#remove unnecessary columns
# # calculate correlation matrix
correlationMatrix <- cor(df_new[,1:length(df_new)-1])
# # summarize the correlation matrix
# print(correlationMatrix)
# # find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.35)
# # print indexes of highly correlated attributes
print(highlyCorrelated)

df_new <- df_new[,-c(highlyCorrelated)]

#split data
set.seed(123)
sample <- sample.split(df_new,SplitRatio = 0.75)
train <- subset(df_new,sample ==TRUE)
test <- subset(df_new, sample==FALSE)

#cross fold validation due to unbalanced data
control <- trainControl(method="repeatedcv", number=10, repeats=1, sampling = "smote")

#random forest
fit.rf <- train(Target~., data=train, method="rf",preProcess = c("scale", "center"), trControl=control)
#boosting algorithm - Stochastic Gradient Boosting (Generalized Boosted Modeling)
fit.gbm <- train(Target~., data=train, method="gbm", trControl=control)
#svm
fit.svm <- train(Target~., data=train, method="svmRadial", trControl=control)

#------------------
#compare models
#------------------
results <- resamples(list(randomforest = fit.rf, gradboost = fit.gbm, svm = fit.svm))

summary(results)
# boxplot comparison
bwplot(results)
# Dot-plot comparison
dotplot(results)

#test results
test_scores <- predict(fit.rf, test)
confusionMatrix(test_scores, test$Target)