#remove old data
rm(list=ls())
#packages
packages <- c('ggplot2', 'corrplot','tidyverse','caret','mlbench','mice', 'caTools','dummies')
#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

df <- read.csv(file.choose(), header = TRUE) #bejing data

glimpse(df)

#colnames(df) <- c("No",'Year',"Month","Day","Hour","PM25","dwep",'temp','pres','cbwd',
#                  'lws','lr','ls')

df.backup <- df

#drop No column
df$No <- NULL

#remove NAs
df <- na.omit(df)

#get target variable
Target <- df$PM25
df$PM25 <- NULL

#categorical variables
df_cat <- df[,c(9)]
#df_cat <- lapply(df_cat, function(x) as.factor(as.character(x)))
df_cat_new <- dummy.data.frame(as.data.frame(df_cat), sep = "_")
df <- df[,-c(9)]

#CTS variables




#normalize data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
df_cts <- df[,3:12]
df_cts <- as.data.frame(lapply(df_cts, normalize))

df <- cbind(df,df_cat)