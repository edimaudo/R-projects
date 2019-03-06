#objective - achieve Lift >4 and Accuracy > 65 for predicting customer answer

#remove old data
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
df <- read_excel(file.choose()) #data3

glimpse(df)

summary(df)

#backup data
df.backup <- df

#check for missing data

#check for balance
#ggplot(df,aes(x=as.factor(TNPS_Answer_NLP))) + geom_bar()


#cts data

#normalize cts data

#categorical data

#target data - #TNPS_ANSWER_NLP

#combine columns

#remove redudant columns

#split into train and test
set.seed(123)

#apply algorithms

#check metrics - lift and accuracy









