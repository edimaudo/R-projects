#remove old data
rm(list=ls())
#packages
packages <- c('ggplot2', 'corrplot','tidyverse','caret','mlbench','mice', 'caTools','readxl')
#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

df <- read.csv(file.choose(), sep = ";") #order

glimpse(df)

summary(df)

Target <- df$Target..Total.orders.


library(dummies)
df_cat <- as.data.frame(df[,1:2])
names(df_cat) <- c("Day","Month")
df_cat$Day <- as.factor(df_cat$Day)
df_cat$Month <- as.factor((df_cat$Month))
df_cat_new <- dummy.data.frame(df_cat, sep = "_")
df_cat_new[5] <- NULL
df_cat_new[length(df_cat_new)] <- NULL

#normalize data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
df_cts <- df[,3:12]
df_cts <- as.data.frame(lapply(df_cts, normalize))

#combine data
df_new <- cbind(df_cat_new, df_cts, Target)


#fine tune model
# # calculate correlation matrix
correlationMatrix <- cor(df_new[,1:length(df_new)])
# # summarize the correlation matrix
# print(correlationMatrix)
# # find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.75)
# # print indexes of highly correlated attributes
print(highlyCorrelated)

df_new <- df_new[,-c(highlyCorrelated)]

#split into train and test
set.seed(123)
sample <- sample.split(df_new,SplitRatio = 0.75)
train <- subset(df_new,sample ==TRUE)
test <- subset(df_new, sample==FALSE)

