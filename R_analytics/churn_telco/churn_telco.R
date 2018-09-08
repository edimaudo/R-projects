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

#check churn data
ggplot(data=df, aes(x=factor(`Churn?`))) +
  geom_bar() + theme_classic() + xlab("Churn Amount")

#transform data

#drop Phone column as not needed
df$Phone <- NULL

glimpse(df)

#split columns into categorical and cts data
df_category <- df %>%
  select(State, `Area Code`,`Int'l Plan`,`VMail Plan`)


df_cts <- select(df, -col(df_category))
df_churn <- df_cts$`Churn?` 
df_cts$`Churn?` <- NULL
