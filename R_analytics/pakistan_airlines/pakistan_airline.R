#clear environment
rm(list = ls())

#packages 
packages <- c('ggplot2', 'corrplot','tidyverse','rvest','stringr',
              'lubridate','rebus','magrittr')



#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#create review dataframe
review_df <- data.frame(matrix(ncol = 4, nrow = 0))
#column headers for review dataframe
review_col <- c("title","description","date_information","rating",)
colnames(review_df) <- review_col

