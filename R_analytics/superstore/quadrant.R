rm(list = ls())

packages <- c('ggplot2','readxl','tidyverse')

#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#load data
#file_path <- "Sample - Superstore.xls"
df <- read_excel(file.choose(),sheet = "Orders",col_names = TRUE)

df$ProfitRatio <- df$Profit/df$Sales



p<-ggplot(df, aes(x=ProfitRatio, y=Discount)) +
  geom_point() +
  lims(x=c(1,10),y=c(1,10)) +
  theme_minimal() +
  coord_fixed() +  
  geom_vline(xintercept = 5) + geom_hline(yintercept = 5) 

p