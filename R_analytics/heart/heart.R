#objective
#predict the presence or absence of heart disease given various data 
#about a patient, including resting blood pressure, maximum heart rate, and EKG readings, 
#as well as other information like age and sex.
#data description
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
for (package in c('ggplot2','corrplot','tidyverse','caret','mlbench')) {
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


#check for imbalanced data
ggplot(data=df, aes(x=factor(disease))) +
  geom_bar() + theme_classic() + xlab("Number of heart diseases")  

df.backup <- df

#df <- df.backup

df <- df %>% 
  select('age', 
         'sex', 
         'max_heart', 
         'resting_blood_pressure', 'disease')


df$disease <- recode_factor(df$disease, "1" = "0", "2" = "1")

#remove empty data
df <- na.omit(df)

library(car)
scatterplotMatrix(df[,1:4])

#normalize data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

df$age <- normalize((df$age))
df$max_heart <- normalize((df$max_heart))
df$resting_blood_pressure <- normalize((df$resting_blood_pressure))

#feature selection
#Remove Redundant Feature remove when absolute correlation >= 0.75
set.seed(7)
# # calculate correlation matrix
correlationMatrix <- cor(df[,1:4])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)
# print indexes of highly correlated attributes
print(highlyCorrelated)
#no columns to remove

#machine learing
#split data into train and test
library(caTools)
set.seed(123)

sample <- sample.split(df,SplitRatio = 0.75)
train <- subset(df,sample ==TRUE)
test <- subset(df, sample==FALSE)

#models
model_gbm<-train(train[,1:4],train[,5],method='gbm')
model_rf<-train(train[,1:4],train[,5],method='rf')
model_nnet<-train(train[,1:4],train[,5],method='nnet')

#Predictions
predictions<-predict.train(object=model_gbm,test,type="raw")
#table(predictions)
confusionMatrix(predictions,test[,5])

predictions_rf<-predict.train(object=model_rf,test,type="raw")
confusionMatrix(predictions_rf,test[,5])

predictions_nnet<-predict.train(object=model_nnet,test,type="raw")
confusionMatrix(predictions_nnet,test[,5])







