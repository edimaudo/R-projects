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

# load data
df <- read_excel("sample.xlsx")

#normalize data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

#remove redundant info
df <- df[,c(2:23,25)]

df_cts <- df[,c(1:20,22)]
df_cts <- as.data.frame(lapply(df_cts, normalize))
df_other <- df[,c(21,23)]
df_new <- cbind(df_cts,df_other)

#=================
#model training
#=================

set.seed(2020)
train <- df
#cross fold validation
control <- trainControl(method="repeatedcv", number=10, repeats=10, classProbs = FALSE)

cl <- makePSOCKcluster(4)
registerDoParallel(cl)

#glm
fit.glm <- train(Sales~., data=train, method="glm", trControl = control)
#lasso
fit.lasso <- train(Sales~., data=train, method="lasso", trControl = control)
#ridge
fit.ridge <- train(Sales~., data=train, method="ridge", trControl = control)

stopCluster(cl)
#------------------
#compare models
#------------------
results <- resamples(list(`linear regression` = fit.glm, 
                          `lasso regression` = fit.lasso, 
                          `ridge regression` = fit.ridge))

summary(results)
# boxplot comparison
bwplot(results)
