#packages 
packages <- c('ggplot2', 'corrplot','tidyverse','shiny','shinydashboard',
              'DT','data.table','readxl')
#load packages
for (package in packages) {
    if (!require(package, character.only=T, quietly=T)) {
        install.packages(package)
        library(package, character.only=T)
    }
}

#load data
df <- read_excel("Sales Detail Report.xlsx")
