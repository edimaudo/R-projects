#remove old data
rm(list=ls())

#load libraries
for (package in c('ggplot2', 'corrplot','tidyverse','caret','mlbench','mice', 'caTools')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#normalize data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#load data
df <- read_csv(file.choose())
df$X1 <- NULL

#create backup
df.backup <- df

#summary
print(summary(df))

#look for missing data
missing_data <- apply(df, 2, function(x) any(is.na(x)))
print(missing_data) #savings and check account have missing data

df1 <- na.omit(df)

#fix missing data
colnames(df) <- c('Age','Sex','Job','Housing','Saving_Account','Checking_Account','Credit_Amount','Duration',
             'Purpose','Risk')

df_update <- mice(df)
df_update<-complete(df_update)
df <- df_update

#look at risk profile
ggplot(data=df, aes(x=factor(Risk))) +
  geom_bar() + theme_classic() + xlab("Risk information")

#recode variables
df_cts <- df %>%
  select(Age,Credit_Amount, Duration)

df_cts <- as.data.frame(lapply(df_cts, normalize))

df_categorical <- df %>%
  select(Sex, Job, Housing, Saving_Account, Checking_Account, Purpose)

library(dummies)
df_category.new <- dummy.data.frame(as.data.frame(df_categorical), sep = "_")

df_final <- cbind(df_category.new, df_cts, df$Risk)

#recode risk profile
df_final <- df_final %>%
  rename(Risk = 'df$Risk')

df_final$Risk <- recode_factor(df_final$Risk, "good" = 0, "bad" = 1)

#create models

library(randomForest)

library(gbm)

#output for kaggle - https://www.kaggle.com/uciml/german-credit
