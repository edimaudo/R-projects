#objective
# ------------------------
#predict the presence or absence of heart disease given various data 
#about a patient, including resting blood pressure, maximum heart rate, and EKG readings, 
#as well as other information like age and sex.
# ------------------------

# ------------------------
# Attribute Information:
# ------------------------
# -- 1. age       
# -- 2. sex       
# -- 3. chest pain type  (4 values)       
# -- 4. resting blood pressure  
# -- 5. serum cholestoral in mg/dl      
# -- 6. fasting blood sugar > 120 mg/dl       
# -- 7. resting electrocardiographic results  (values 0,1,2) 
# -- 8. maximum heart rate achieved  
# -- 9. exercise induced angina    
# -- 10. oldpeak = ST depression induced by exercise relative to rest   
# -- 11. the slope of the peak exercise ST segment     
# -- 12. number of major vessels (0-3) colored by flourosopy        
# -- 13.  thal: 3 = normal; 6 = fixed defect; 7 = reversable defect     
# 
# Attributes types
# -----------------
#   
# Real: 1,4,5,8,10,12
# Ordered:11,
# Binary: 2,6,9
# Nominal:7,3,13
# 
# Variable to be predicted
# ------------------------
#   Absence (1) or presence (2) of heart disease
# 
# Cost Matrix
# 
# abse  pres
# absence	  0	1
# presence  5	0
# 
# where the rows represent the true values and the columns the predicted.
# 
# No missing values.

#remove old data
rm(list=ls())

#load libraries
packages <- c('ggplot2','corrplot','tidyverse','caret','mlbench','mice', 'caTools','dummies')

for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#load data
df <- read.delim(file.choose(),header=FALSE,sep="")

#update column names
names(df) <- c('age','sex','chest_pain_type','resting_blood_pressure',"serum_cholestrol",'fasting_blood_sugar',
               'resting_ecg',"max_heart","exercise_angina",'oldpeak','slope_peak_exercise','major_vessels',
               'thal','disease')


df.backup <- df

summary(df)

glimpse(df)

#check for imbalanced data
ggplot(data=df, aes(x=factor(disease))) +
  geom_bar() + theme_classic() + xlab("Number of heart diseases")  # fairly balanced

#check correlation
corinfo <- df[,1:13]
corrplot(cor(corinfo), method="number")

#check for data
missing_data <- apply(df, 2, function(x) any(is.na(x))) #no missing data
print(missing_data)

#remove empty data
df <- na.omit(df)

#normalize data function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

df$disease <- recode_factor(df$disease, "1" = "0", "2" = "1")

Target <- df$disease

#categorical variables
df_cat <- df[,c(2,3,6,7,9,11,12,13)]

#one hot encoding
df_cat_new <- dummy.data.frame(as.data.frame(df_cat), sep = "_")


#normalize data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

df_cts <- df[,c(1,4,5,8,10)]
df_cts <- as.data.frame(lapply(df_cts, normalize))

#combine data frame
df_new <- cbind(df_cat_new, df_cts,Target)


#remove highly correlated columns
# # calculate correlation matrix
correlationMatrix <- cor(df_new[,1:length(df_new)-1])
# # summarize the correlation matrix
# print(correlationMatrix)
# # find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)
# # print indexes of highly correlated attributes
print(highlyCorrelated)

#split data
set.seed(123)
sample <- sample.split(df_new,SplitRatio = 0.75)
train <- subset(df_new,sample ==TRUE)
test <- subset(df_new, sample==FALSE)

#cross fold validation
control <- trainControl(method="repeatedcv", number=10, repeats=5)


#random forest
fit.rf <- train(Target~., data=train, method="rf", trControl=control)
Stochastic Gradient Boosting (Generalized Boosted Modeling)
fit.gbm <- train(Target~., data=train, method="gbm", trControl=control)
#svm
fit.svm <- train(Target~., data=train, method="svmRadial", trControl=control)
#nnet
fit.nnet <- train(Target~., data=train, method="nnet", trControl=control)


#------------------
#compare models
#------------------
results <- resamples(list(randomforest = fit.rf, gradboost = fit.gbm, svm = fit.svm, nnet = fit.nnet))

summary(results)
# boxplot comparison
bwplot(results)
# Dot-plot comparison
dotplot(results)

#use test data

test_scores <- predict(fit.gbm, test)
confusionMatrix(test_scores, test$Target)