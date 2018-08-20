# #bank data
# 1. Title: Bank Marketing
# 
# 2. Relevant Information:
#   
# The data is related with direct marketing campaigns of a Portuguese banking institution. 
# The marketing campaigns were based on phone calls. Often, 
# more than one contact to the same client was required.
# 
# 
# 3. Number of Instances: 45211 for bank-full.csv (4521 for bank.csv)
# 
# 4. Number of Attributes: 16 + output attribute.
# 
# 5. Attribute information:
#   Input variables:
#   # bank client data:
#   1 - age (numeric)
# 2 - job : type of job (categorical: "admin.","unknown","unemployed","management","housemaid","entrepreneur","student",
#                        "blue-collar","self-employed","retired","technician","services") 
# 3 - marital : marital status (categorical: "married","divorced","single"; note: "divorced" means divorced or widowed)
# 4 - education (categorical: "unknown","secondary","primary","tertiary")
# 5 - default: has credit in default? (binary: "yes","no")
# 6 - balance: average yearly balance, in euros (numeric) 
# 7 - housing: has housing loan? (binary: "yes","no")
# 8 - loan: has personal loan? (binary: "yes","no")
# # related with the last contact of the current campaign:
# 9 - contact: contact communication type (categorical: "unknown","telephone","cellular") 
# 10 - day: last contact day of the month (numeric)
# 11 - month: last contact month of year (categorical: "jan", "feb", "mar", ..., "nov", "dec")
# 12 - duration: last contact duration, in seconds (numeric)
# # other attributes:
# 13 - campaign: number of contacts performed during this campaign and for this client (numeric, includes last contact)
# 14 - pdays: number of days that passed by after the client was last contacted from a previous campaign (numeric, -1 means client was not previously contacted)
# 15 - previous: number of contacts performed before this campaign and for this client (numeric)
# 16 - poutcome: outcome of the previous marketing campaign. Not an indication of purchase, 
# contact showed interest in product (categorical: "unknown","other","failure","success")
# Output variable (desired target):
# 17 - y - has the client subscribed a term deposit? (binary: "yes","no")
# 
# 6. Missing Attribute Values: None
# 
# 7. Objective
# Design a predictive models that would help in designing 
# recommendation on how to optimize targeting moving forward, assuming these same data points 
# will be available. 

#remove old data
rm(list=ls())

#load libraries
for (package in c('ggplot2', 'corrplot','tidyverse','caret','mlbench','Hmisc')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

df <- read.csv(file.choose())

summary(df)

#data cleaning
#data looks fine

#data transformation
df_category <- df %>%
  select(job,marital, education, default, housing, loan, contact, month, poutcome)

library(dummies)
df_category.new <- dummy.data.frame(df_category, sep = ".")
df_category <- NULL

#one hot encoding - remove one column from the different categories
df_category.new$job.unknown <- NULL
df_category.new$marital.divorced <- NULL
df_category.new$education.unknown <- NULL
df_category.new$default.no <- NULL
df_category.new$housing.no <- NULL
df_category.new$loan.no <- NULL
df_category.new$contact.unknown <- NULL
df_category.new$month.dec <- NULL
df_category.new$poutcome.unknown <- NULL

df_others <-df %>%
  select(age, balance, day, duration, campaign, pdays, previous)

#normalize data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#normalize other data
df_others <- as.data.frame(lapply(df_others, normalize))

#combine data
df_main <- cbind(df_category.new, df_others, df$y)

#rename and recode df$y
df_main <- df_main %>%
  rename('y' = `df$y`) %>%
  mutate(y = recode(y, "no"= '0', 'yes' = '1'))

#find which features are important 
#option 1
set.seed(7)
# load the data
data(PimaIndiansDiabetes)
# calculate correlation matrix
correlationMatrix <- cor(PimaIndiansDiabetes[,1:8])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)


#split data into train and test

#design model

