#remove old data
rm(list = ls())

#objective build predict churn

#load libraries
for (package in c('ggplot2', 'corrplot','tidyverse',
                  "cowplot",'lubridate','data.table','caret','mlbench','xgboost','plotrix')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}


#load data
df <- read_csv(file.choose())

#data backup
df.backup <- df

#summary 
print(summary(df))

#check for missing data
missing_data <- apply(df, 2, function(x) any(is.na(x))) #no missing data
print(missing_data)

#check for balanced data
ggplot(data=df, aes(x=factor(Subscription))) +
  geom_bar() + theme_classic() + xlab("Type of subscription")



