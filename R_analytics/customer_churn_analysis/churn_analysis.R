#===================
## Load Libraries
#===================
rm(list = ls()) #clear environment

# libraries
packages <- c('ggplot2', 'corrplot','tidyverse',"caret","dummies","fastDummies"
              ,'FactoMineR','factoextra','readxl','scales','dplyr','mlbench','caTools',
              'gridExtra','doParallel','readxl')
# load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}


df <- read_csv("customer_churn.csv")

#summary
summary(df)

#backup
df.orig <- df

#check for missing values
missing_data <- apply(df, 2, function(x) any(is.na(x))) 
print("missing data")
print(missing_data) # Total Charges

#remove NAs
df <- na.omit(df)

#convert from string to numeric
df <- as.data.frame(df, stringsAsFactors=F)

#Label Encoder
labelEncoder <-function(x){
  as.numeric(factor(x))-1
}
#normalize data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

df_customerID <- df[1]
df[1] <- NULL

#split data into different categories
df_cts <- df %>%
  select(tenure, TotalCharges)

df_cat <- df %>%
  select(-c(MonthlyCharges,TotalCharges, tenure, Churn))

df_churn <- df %>%
  select(Churn)

#recode data

#combine data
df_new <- cbind(df_cts,df_cat,df_churn)