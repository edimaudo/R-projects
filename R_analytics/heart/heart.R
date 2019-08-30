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


#check for imbalanced data
ggplot(data=df, aes(x=factor(disease))) +
  geom_bar() + theme_classic() + xlab("Number of heart diseases")  