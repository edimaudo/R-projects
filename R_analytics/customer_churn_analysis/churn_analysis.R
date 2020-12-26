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

