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

#recode histology
df$histology <- as.factor(df$histology)

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

#normalize
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

df_categories <- df %>%
  select (')

c("class",'age','sex',"steroid",'antivirals',
  'fatigue','malaise','anorexia','liver_big',
  "liver_firm",'spleen_palpable','spiders',
  'ascites','varices','bilrubin','alk_phosphates',
  'sgot','albumin','protime','histology')