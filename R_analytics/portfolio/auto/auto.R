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

df <- read.table(file.choose()) #data

glimpse(df)

#rename columns
colnames(df) <- c('mpg','cylinders','displacement','horsepower','weight','acceleration','model_year',
                  'origin','car_name')

summary(df)

#remove car name
df['car_name'] <- NULL

#one hot encode cylinders, model_year
df_cat <- df[,c(1:10,12:17,19:20)]
df_cat <- lapply(df_cat, function(x) as.factor(as.character(x)))
df_cat_new <- dummy.data.frame(as.data.frame(df_cat), sep = "_")
#drop columns


#scale data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
df_cts <- df[,1:8]
df_cts <- as.data.frame(lapply(df_cts, normalize))

#combine data
strength <- as.data.frame(df[,9])
df_new <- cbind(df_cat_new,df_cts, strength)