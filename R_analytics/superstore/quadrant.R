rm(list = ls())

packages <- c('ggplot2','readxl','')

#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#load data
#file_path <- "Sample - Superstore.xls"
df <- read_excel(file.choose())