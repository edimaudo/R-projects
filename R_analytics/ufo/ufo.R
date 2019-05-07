#remove old data
rm(list=ls())
#packages
packages <- c('ggplot2', 'corrplot','tidyverse','caret','mlbench','mice', 'caTools','readxl','readr')
#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

df <- read.delim(file.choose(), sep="\t",stringsAsFactors = FALSE ,header=FALSE) 
head(df)
colnames(df) <- c("DateOccurred","DateReported",'Location','ShortDescription',"Duration",
                  "LongDescription")
df$DateOccurred <- as.Date(df$DateOccurred,format = "%Y%m%d")