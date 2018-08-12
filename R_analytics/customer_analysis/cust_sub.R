#Business background
#We have a list of existing customers which contains various data points for each of them. 
#The data for each customers consists of 3 categorical attributes and two numerical attributes. 
#The list currently consists of 800+ customers but will grow over time to 80k. 

#The provided list is structured as followed:
#Customer ID, Categorical Attribute #1, Categorical Attribute #2, 
#Categorical Attribute #3, Numeric Attribute #1, Numeric Attribute #2, 
#Belongs to Group #1, Belongs to Group #2

#Objective
#Goal is to predict who would be in the different categories

#remove old data
rm(list = ls())

#load libraries
for (package in c('ggplot2', 'corrplot','tidyverse',
                  "cowplot",'lubridate','data.table','caret','mlbench','xgboost','plotrix')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

df <- read.csv(file.choose())

#data summary
print(summary(df))

#rename columns
names(df) <- c('CustomerID', 'DocumentType', 'ExpectedVolume', 'Industry', 
               'FileFirstDays', 'Stacks', 'SubscriptionPlan', 'Subscription')

print(summary(df))

#drop columns due to lack of information
df$SubscriptionPlan <- NULL
df$Industry <- NULL

#drop customer information
df$CustomerID <- NULL

#correlation between cts variables
corinfo <- cor(df[,4:6])
corrplot(corinfo,method='number')

#check for balanced data
ggplot(data=df, aes(x=factor(Subscription))) +
  geom_bar() + theme_classic() + xlab("Type of subscription") #lots more non subscriptions
#model would definitely lean towards that

#recode categorical variables
library(dummies)
df_main_category <- df %>%
  select(DocumentType, ExpectedVolume)
df_main_category.new <- dummy.data.frame(df_main_category, sep = ".")

#drop value from one of the newly created columns to prevent multicollinearity
df_main_category.new$ExpectedVolume.NULL <- NULL
df_main_category.new$DocumentType.others <- NULL

df <- df %>%
  select(FileFirstDays, Stacks, Subscription)





