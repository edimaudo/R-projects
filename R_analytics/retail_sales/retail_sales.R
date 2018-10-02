# Context
# This dataset contains lot of historical sales data. 
# It was extracted from a Brazilian top retailer and has many SKUs and many stores. 
# The data was transformed to protect the identity of the retailer.

# Current inventory management models have many solutions to place the correct order,
# but they are all based in a single unknown factor: the demand for the next periods.
# This is why short-term forecasting is so important in retail and consumer goods industry.
# We encourage you to seek for the best demand forecasting model for the next 2-3 weeks. 
# This valuable insight can help many supply chain practitioners 
# to correctly manage their inventory levels.


#objective
#Short term forecasting to optimize in-store inventories

#remove old data
rm(list=ls())

#load libraries
for (package in c('ggplot2', 'corrplot','tidyverse','caret','mlbench', 'forecast', "fpp2","prophet")) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#load data
df <- read_csv(file.choose())

#Rename columns
colnames(df) <- c('Date',"Sale","Stock","Price")
#submit to kaggle