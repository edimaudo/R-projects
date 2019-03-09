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
missing_data <- apply(df, 2, function(x) any(is.na(x))) #no missing data
print(missing_data)

#check for balance
ggplot(df,aes(x=as.factor(TNPS_Answer_NLP))) + geom_bar() #data is unbalanced

#data cleanup
#remove first column
df[1] <- NULL

#get month from data
df$TNPS_date <- as.Date(df$TNPS_date)
df$month <- months(df$TNPS_date)
df$TNPS_date <- NULL

Target <- df$TNPS_Answer_NLP

df$TNPS_Answer_NLP <- NULL

df_cat <- df[,c()]
df_cts <- df[, -df_cat] 


# 
# n<-ncol(df)
# output<- df$TNPS_Answer_NLP
# output<-as.factor(output)
# input<- df[ ,-c(2)]
# input$TNPS_date <- as.character(input$TNPS_date)
# View(input)
# 
# #Balance the Dataset using ubSMOTE#
# data<-ubBalance(X= input, Y=output, type="ubSMOTE", percOver=300, percUnder=150, verbose=TRUE)
# View(data)

#rebalance data


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









