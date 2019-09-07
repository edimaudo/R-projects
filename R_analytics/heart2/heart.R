#remove old data
rm(list=ls())
#packages
packages <- c('tidyverse','caret','mlbench', 'caTools')
#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

df <- read.table(file.choose()) #heart

glimpse(df)

summary(df)

colnames(df) <- c('age','sex','chest_pain_type','resting_blood_pressure', 'serum_cholerstrol',
                  'fasting_blood_sugar','resting_ecg','max_heart_rate','exercise_angina',
                  'oldpeak','slope_peak','num_major_blood_vessels','thal','class')


#check for missing data
missing_data <- apply(df, 2, function(x) any(is.na(x))) #no missing data
print(missing_data)

#factor class column
df$class <- as.factor(df$class)

#normalize data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

class <- df$class
df_cat <- df[,c(2,3,6,7,9,11,12)]
df_cts <- df[,-c(2,3,6,7,9,11,12,14)]

df_cat$sex <- as.factor(df_cat$sex)
df_cat$chest_pain_type <- as.factor(df_cat$chest_pain_type)
df_cat$fasting_blood_sugar <- as.factor(df_cat$fasting_blood_sugar)
df_cat$resting_ecg <- as.factor(df_cat$resting_ecg)
df_cat$exercise_angina <- as.factor(df_cat$exercise_angina)
df_cat$slope_peak <- as.factor(df_cat$slope_peak)
df_cat$num_major_blood_vessels <- as.factor(df_cat$num_major_blood_vessels)



library(dummies)

df_cat_new <- dummy.data.frame(as.data.frame(df_cat), sep = "_")

df_cts_new <- as.data.frame(lapply(df_cts, normalize))

df_new <- cbind(df_cts_new, df_cat_new, class)

#fine tune model
# # calculate correlation matrix
correlationMatrix <- cor(df_new[,1:length(df)-1])
# # summarize the correlation matrix
# print(correlationMatrix)
# # find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)
# # print indexes of highly correlated attributes
print(highlyCorrelated)

df_new <- df_new[,-c(highlyCorrelated)]

#split data into train and test
set.seed(123)
sample <- sample.split(df_new,SplitRatio = 0.75)
train <- subset(df_new,sample ==TRUE)
test <- subset(df_new, sample==FALSE)
control <- trainControl(method="repeatedcv", number=10, repeats=3)
#models
#decision trees
fit.cart <- train(class~., data=train, method="rpart", trControl=control)
#LDA
fit.lda <- train(class~., data=train, method="lda", trControl=control)
#svm
fit.svm <- train(class~., data=train, method="svmRadial", trControl=control)
#random forest
fit.rf <- train(class~., data=train, method="rf", trControl=control)
#bagged cart
fit.treebag <- train(class~., data=train, method="treebag", trControl=control)
#boosting algorithm - Stochastic Gradient Boosting (Generalized Boosted Modeling)
fit.gbm <- train(class~., data=train, method="gbm", trControl=control)

results <- resamples(list(cart = fit.cart, lda = fit.lda,
                          svm = fit.svm, randomforest = fit.rf, 
                          baggedcart = fit.treebag, gradboost = fit.gbm))

summary(results)

# boxplot comparison
bwplot(results)
# Dot-plot comparison
dotplot(results)

test_scores_gbm <- predict(fit.gbm, test)
confusionMatrix(test_scores_gbm, test$class)

test_scores_svm <- predict(fit.svm, test)
confusionMatrix(test_scores_svm, test$class)