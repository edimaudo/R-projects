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

