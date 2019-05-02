#remove old data
rm(list=ls())
#packages
packages <- c('ggplot2', 'corrplot','tidyverse','caret','mlbench','mice', 'caTools','readxl','dummies')
#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

df <- read_excel(file.choose()) #data

glimpse(df)

summary(df)

heat <- as.data.frame(df[9])
cold <- as.data.frame(df[10])



#one hot encode X6,X8
df_cat <- df[,c(6,8)]
df_cat <- lapply(df_cat, function(x) as.factor(as.character(x)))
df_cat_new <- dummy.data.frame(as.data.frame(df_cat), sep = "_")
#drop columns
df_cat_new[21] <- NULL
df_cat_new[5] <- NULL

#scale data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
df_cts <- df[,c(1,2,3,4,5,7)]
df_cts <- lapply(df_cts, function(x) as.double(x))
df_cts <- as.data.frame(lapply(df_cts, normalize))



#predict heating

#predict cooling

