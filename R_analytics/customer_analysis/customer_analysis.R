rm(list=ls())
#packages
packages <- c('tidyverse','caret','mlbench', 'caTools','readxl','unbalanced','dummies')
#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#load data
df <- read_excel(file.choose())

glimpse(df)

summary(df)

#check for missing data

#check for balance

#get 
#TNPS_ANSWER_NLP might be the target variable
#resample data 

#metrics lift and accuracy
#Metrics: Lift >4 and Accuracy > 65

#ggplot(df,aes(x=as.factor(TNPS_Answer_NLP))) + geom_bar()

