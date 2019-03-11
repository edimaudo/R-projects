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
df <- read_excel(file.choose()) #main in customer analysis folder

glimpse(df)

summary(df)

#backup data
df.backup <- df

#check for missing data
missing_data <- apply(df, 2, function(x) any(is.na(x))) #no missing data
print(missing_data)

#check for balance
ggplot(df,aes(x=as.factor(TNPS_Answer_NLP))) + geom_bar() #data is unbalanced

#data cleanup
#remove user id
df[1] <- NULL

#get month from data
df$TNPS_date <- as.Date(df$TNPS_date)
df$month <- months(df$TNPS_date)
df$TNPS_date <- NULL

#target data - #TNPS_ANSWER_NLP
Target <- df$TNPS_Answer_NLP
df$TNPS_Answer_NLP <- NULL

#categorical data
df_cat <- df[,c()]

#cts data

#normalize cts data
df_cts <- df[, -df_cat] 

#combine columns

#remove redudant columns

#split into train and test
set.seed(123)

#apply algorithms

#check metrics - lift and accuracy









