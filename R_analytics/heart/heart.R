#objective
#predict the presence or absence of heart disease given various data 
#about a patient, including resting blood pressure, maximum heart rate, and EKG readings, 
#as well as other information like age and sex.

c('age','sex','chest_pain_type','resting_blood_pressure',"serum_cholestrol",'fasting_blood_sugar',
  'resting_ecg',"max_heart","exercise_angina",'oldpeak','slope_peak_exercise','major_vessels',
  'thal','disease')
#data description
# Attribute Information:
#   ------------------------
#   -- 1. age       
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
for (package in c('ggplot2', 'corrplot','tidyverse',
                  "cowplot",'lubridate','data.table','caret','mlbench','xgboost','plotrix')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#read file
df <- read.delim(file.choose(),header=FALSE,sep="")

#update column names
names(df) <- c('age','sex','chest_pain_type','resting_blood_pressure',"serum_cholestrol",'fasting_blood_sugar',
               'resting_ecg',"max_heart","exercise_angina",'oldpeak','slope_peak_exercise','major_vessels',
               'thal','disease')

#visualization

#check for imbalanced data
ggplot(data=df, aes(x=factor(disease))) +
  geom_bar() + theme_classic() + xlab("Number of heart diseases")  

df.backup <- df

df <- df.backup

df <- df %>% 
  select('age', 
         'sex', 
         'max_heart', 
         'resting_blood_pressure', 'disease')


df$disease <- recode_factor(df$disease, "1" = "0", "2" = "1")

df <- na.omit(df)

#split data into train and test
library(caTools)
set.seed(123)

sample <- sample.split(df,SplitRatio = 0.75)
train <- subset(df,sample ==TRUE)
test <- subset(df, sample==FALSE)

library(car)
scatterplotMatrix(df[,1:4])

library( 'e1071' )
model <- svm(disease~., train)
res <- predict( model, newdata=train )
res1 <- predict( model, newdata=test )

res.new <- as.data.frame(res)
res1.new <- as.data.frame(res1)

library(SDMTools)
confusion.matrix((train$disease), (res.new$res), threshold = 0.5)
accuracy(train$diease, res.new, threshold = 0.5)

#confusion matrix
table(train$disease, res.new$res)
table(test$diease, res1.new$res1)


#find important columns
#names(getModelInfo())

#Application of a Machine Learning Algorithm to training dataset
# model_gbm<-train(trainSet[,predictors],trainSet[,outcomeName],method='gbm')
# model_rf<-train(trainSet[,predictors],trainSet[,outcomeName],method='rf')
# model_nnet<-train(trainSet[,predictors],trainSet[,outcomeName],method='nnet')
# model_glm<-train(trainSet[,predictors],trainSet[,outcomeName],method='glm')

#Setup a ML algorithm and parameter settings
# fitControl <- trainControl(
#   method = "repeatedcv",
#   number = 5,
#   repeats = 5)

#remember to add verbose=FALSE to avoid endless outputs

#fine tune model
#modelLookup(model='gbm')
#Creating grid
#grid <- expand.grid(n.trees=c(10,20,50,100,500,1000),shrinkage=c(0.01,0.05,0.1,0.5),n.minobsinnode = c(3,5,10),
interaction.depth=c(1,5,10))
# training the model
#model_gbm<-train(trainSet[,predictors],trainSet[,outcomeName],method='gbm',trControl=fitControl,tuneGrid=grid)
# summarizing the model
#print(model_gbm)

#using tune length
#using tune length
#model_gbm<-train(trainSet[,predictors],trainSet[,outcomeName],method='gbm',trControl=fitControl,tuneLength=10)
#print(model_gbm)

#Checking variable importance for GBM
#Variable Importance
#varImp(object=model_gbm)

#Evaluation of trained Algorithm (or Model) and result
#Predictions
#predictions<-predict.train(object=model_gbm,testSet[,predictors],type="raw")
#table(predictions)
#confusionMatrix(predictions,testSet[,outcomeName])
