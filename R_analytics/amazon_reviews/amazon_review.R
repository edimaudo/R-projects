#================
# packages
#================
rm(list=ls())
packages <- c('ggplot2', 'corrplot','tidyverse','dplyr','tidyr',
              'caret','mlbench','scales','proxy','reshape2',
              'caTools','doParallel','scales','catboost', 'Matrix',
              'stringr','reshape2','purrr','lubridate','tidytext','gridExtra',
              'wordcloud2','stringr','tm','performanceEstimation',
              'textmineR','topicmodels','textclean','pals')
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}
#================
# Load data
#================
df <- read.csv(file.choose(),sep = ",")

#----------------
# Check for missing values
#----------------
missing_data <- apply(df, 2, function(x) any(is.na(x)))
print(missing_data)

#----------------
#check columns
#----------------
unique(df$Category)
unique(df$PAccuracyuct)
unique(df$Country)
unique(df$Date)
unique(df$Year)
unique(df$Verified)
