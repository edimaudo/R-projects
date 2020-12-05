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

#drop columns
df <- df%>%
  select(-c(X,X.1,X.2,X.3))

#check for missing data
missing_data <- apply(df, 2, function(x) any(is.na(x)))
print(missing_data)

#check columns
unique(df$attraction_name)
unique(df$category)

#columns to drop attraction_name, category, reviewer_name, 
df <- df %>%
  select(reviewer_location, review_total_contributions,
         date_of_experience, review_language, review_text, rating) %>%
  na.omit()

table(df$rating) # lots of 4 and 5


