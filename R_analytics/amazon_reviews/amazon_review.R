# =======================================================
# packages
# =======================================================
rm(list=ls())
packages <- c('ggplot2', 'corrplot','tidyverse','dplyr','tidyr',
              'caret','mlbench','scales','proxy','reshape2',
              'caTools','doParallel','scales','catboost', 'Matrix',
              'stringr','reshape2','purrr','lubridate','tidytext','gridExtra',
              'wordcloud2','stringr','tm','SnowballC')
# load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#load data
df <- read.csv(file.choose(),sep = ",")

glimpse(df)

colnames(df)

unique(df$Category)
unique(df$Procuct)
unique(df$Country)
unique(df$Date)
unique(df$Year)
unique(df$Verified)

#select only needed columns
df <- df %>%
  select(Procuct,Country,Month,Year, Verified, Helpful,Title, Body, Rating) %>%
  na.omit()

#check for missing values
missing_data <- apply(df, 2, function(x) any(is.na(x)))
print(missing_data)

