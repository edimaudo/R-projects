#remove old data
rm(list=ls())

#packages
packages = c('tidyverse','caret','mlbench','data.table', 'corplot', 'ggplot2', 'caTools', 'readxl')

#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#read data
df = as.data.frame(read_excel(file.choose())) #excel file

#check for missing data
missing_data <- apply(df, 2, function(x) any(is.na(x))) #no missing data
print(missing_data)