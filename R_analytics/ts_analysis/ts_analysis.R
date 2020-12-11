# =======================================================
# packages
# =======================================================
rm(list=ls())
packages <- c('ggplot2', 'corrplot','tidyverse','dplyr','tidyr',
              'caret','mlbench','caTools','scales','readxl',
              'doParallel','scales','catboost', 'Matrix','lubridate',
              'xts','TTR','forecast')
# load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

df <- read_excel(file.choose())


#This converts the dataframe into an xts object
df.xts <- xts(x = df$Percentage, order.by = df$Months) 

df.end <- floor(0.6*length(df.xts)) #select the first 60% of the data
df.train <- df.xts[1:df.end,] #assign the first 60% of the data to the train set
df.test <- df.xts[(df.end+1):length(df.xts),] #assign the most recent 40% to the test set

