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

url <- "https://www.tripadvisor.com/Airline_Review-d8729129-Reviews-or5-Pakistan-International-Airlines.html"

#function to manage connection closing and reduce 403 issues
CatchupPause <- function(Secs){
  Sys.sleep(Secs) #pause to let connection work
  closeAllConnections()
  gc()
}

#83 pages
for (i in 1:83){
  web_page<-read_html(player_url) 
}