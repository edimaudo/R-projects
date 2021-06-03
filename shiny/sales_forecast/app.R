rm(list = ls()) #clear environment

# libraries
packages <- c('ggplot2', 'corrplot','tidyverse',"caret","dummies",'readxl',
              'scales','dplyr','mlbench','caTools','forecast','TTR','xts',
              'lubridate')
# load packages
for (package in packages) {
    if (!require(package, character.only=T, quietly=T)) {
        install.packages(package)
        library(package, character.only=T)
    }
}

# load data
df <- read.csv("volume_by_day_type_and_region.csv")
df$revenue <- df$average_price*df$total_volume #revenue

# Drop downs
type_info <- sort(as.vector(unique(df$type))) #type 
region_info <- sort(as.vector(unique(df$region))) #region