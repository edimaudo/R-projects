#goal is to design a function to predict price

#remove old data
rm(list=ls())

#load libraries
packages <- c('ggplot2', 'corrplot','tidyverse','caret','mlbench','mice', 'caTools', 
              'MASS','Metrics','randomForest','lars','xgboost','Matrix','methods', 'readxl')
#load libraries
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#load data
df <- read_excel(file.choose()) #load the macro enabled file

#check data
glimpse(df)

#summary
print(summary(df))

#look for missing data
missing_data <- apply(df, 2, function(x) any(is.na(x)))
print(missing_data)

#rename columns
colnames(df) <- c('Color','Clarity','Weight',"Price")

#normalize data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#modelling
#split data into category and cts

df_category <- df %>%
  dplyr::select(Color,Clarity)

library(dummies)
df_category.new <- dummy.data.frame(as.data.frame(df_category), sep = "_")
df_category.new <- as.data.frame(df_category.new)

glimpse(df_category.new)

df$Weight <- normalize(df$Weight)
df_final <- cbind(df_category.new, df[,3], df[,4])



#split data into train and test
set.seed(123)
sample <- sample.split(df_final,SplitRatio = 0.75)
training <- subset(df_final,sample ==TRUE)
test <- subset(df_final, sample==FALSE)

#regression modelling
plot(Price ~., data=training)
fit <- lm(Price ~., data=training)
summary(fit)
plot(residuals(fit) ~ ., data=training)

