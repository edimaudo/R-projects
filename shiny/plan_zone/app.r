#packages 
packages <- c('ggplot2', 'corrplot','tidyverse','ggfortify','shiny','readxl','DT')
#load packages
for (package in packages) {
    if (!require(package, character.only=T, quietly=T)) {
        install.packages(package)
        library(package, character.only=T)
    }
}

#load data
df <- read_excel("zone_data.xlsx")

#pytpe
ptype <- c(sort(unique(df$p_type)))
region <- c(sort(unique(df$region)))


