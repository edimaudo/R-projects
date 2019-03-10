#remove old data
rm(list=ls())
#packages
packages <- c('ggplot2', 'corrplot','tidyverse','caret','mlbench','mice', 'caTools','dummies')
#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

df <- read.csv(file.choose(), header = FALSE) #autism data

glimpse(df)

#rename columns
colnames(df) <- c('A1','A2','A3','A4','A5','A6','A7','A8','A9','A10',
                  'AGE','GENDER','ETHNICITY','JUNDICE','AUTISM','COUNTRY',
                  'USED_APP','RESULT','AGE_DESC','RELATION','TARGET')

df.backup <- df

