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

df <- read.table(file.choose(),sep = ",") #hepatitis

glimpse(df)

summary(df)

#rename columns
names(df) <- c("class",'age','sex',"steroid",'antivirals',
               'fatigue','malaise','anorexia','liver_big',
               "liver_firm",'spleen_palpable','spiders',
               'ascites','varices','bilrubin','alk_phosphates',
               'sgot','albumin','protime','histology')

#fix missing data
df$protime[df$protime == '?'] <- NA

df <- na.omit(df)

#normalize
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#recode histology
df$histology <- as.factor(df$histology)
histology <- df$histology

df_cts <- df %>%
  select('age','bilrubin','alk_phosphates','sgot',
         'albumin','protime')

df_cts <-  as.data.frame(lapply(df_cts, normalize))

df_categories <- df %>%
  select ('class','sex', 'steroid',
          'antivirals','fatigue','malaise',
          'anorexia','liver_big','liver_firm',
          'spleen_palpable','spiders','ascites',
          'varices','albumin')

df_category_new <- dummy.data.frame(as.data.frame(df_categories), sep = "_")

df_new <- rbind(df_category_new, df_cts, histology)

#fine tune model
# # calculate correlation matrix
correlationMatrix <- cor(df_final[,1:26])
# # summarize the correlation matrix
# print(correlationMatrix)
# # find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)
# # print indexes of highly correlated attributes
print(highlyCorrelated)

df_new <- df_new[,-c(highlyCorrelated)]

#split data into train and test
sample <- sample.split(df_new,SplitRatio = 0.75)
train <- subset(df_new,sample ==TRUE)
test <- subset(df_new, sample==FALSE)

resultdata <- function(control, train){
  set.seed(123)
  #LDA
  fit.lda <- train(Glass~., data=train, method="lda", trControl=control)
  #svm
  fit.svm <- train(Glass~., data=train, method="svmRadial", trControl=control)
  #random forest
  fit.rf <- train(Glass~., data=train, method="rf", trControl=control)
  #boosting algorithm - Stochastic Gradient Boosting (Generalized Boosted Modeling)
  fit.gbm <- train(Glass~., data=train, method="gbm", trControl=control)
  
  #------------------
  #compare models
  #------------------
  results <- resamples(list(lda = fit.lda,svm = fit.svm, randomforest = fit.rf, gradboost = fit.gbm))
  
  return (results)
}


