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

df <- read.csv(file.choose(), header = FALSE)

glimpse(df)

#rename columns
colnames(df) <- c("Sex","Length","Diameter","Height","Whole_height","Shucked_weight","Viscera_weight",
                  'Shell_weight',"Rings")

#backup data
df.backup <- df

