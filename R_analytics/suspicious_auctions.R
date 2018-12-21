#remove old data
rm(list=ls())

#packages
packages <- c('ggplot2','corrplot','dplyr','data.table')

#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#load data
notification_final <- fread.csv(file.choose(), sep="#")
protocols_final <- read.csv(file.choose())

