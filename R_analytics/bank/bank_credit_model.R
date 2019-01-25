#remove old data
rm(list=ls())

#packages
packages <- c('tidyverse','DataExplorer','fastDummies','mlbench','caret','caTools')

#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#load data
df <- read.table(file.choose(), sep=";", header=T) #"bank-full.csv"

#understand data
glimpse(df)

introduce(df)

plot_bar(df)

#one hot encoding
dummy_data <- fastDummies::dummy_cols(df,remove_first_dummy = TRUE)

#data to keep
keep <- c("age","day","balance","campaign","pdays","job_entrepreneur","job_blue-collar","job_unknown","job_retired","job_admin.","job_services",          "job_self-employed","job_unemployed","job_housemaid","job_student","marital_single","marital_divorced","education_secondary","education_primary","default_yes","housing_no","loan_yes","contact_cellular","contact_telephone","month_jun","month_jul", "month_aug","month_oct","month_nov","month_dec","month_jan","month_feb","month_mar","month_apr","month_sep","poutcome_failure","poutcome_other","poutcome_success","y_yes")          
final <- dummy_data[keep]

#split data
set.seed(123)

sample <- sample.split(final,SplitRatio = 0.75)
training <- subset(final,sample ==TRUE)
test <- subset(final, sample==FALSE)

X_train <- training[, 1:38]
y_train <-  as.factor(training[, 39])

X_test <- test[, 1:38]
y_test <- as.factor(test[, 39])

model_classy <- train(X_train, y_train, method='LogitBoost',preProcess=c("center", "scale"))
feature_importance <- varImp(model_classy, scale=FALSE)
plot(feature_importance)
predictions<-predict(object=model_classy,X_test)
table(predictions)
confusionMatrix(predictions,y_test)