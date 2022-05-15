# Electricity forecast 
rm(list = ls()) # clear environment
#=============
# Packages
#=============
packages <- c('ggplot2', 'corrplot','tidyverse','readxl',
              'scales','dplyr','mlbench','caTools',
              'forecast','TTR','xts','lubridate')

for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#=============
# Load Data
#=============
df <- read.csv("DEhourlyload.csv")

#=============
# Data overview
#=============

# Data Summary
summary(df)

#Check for missing variables
missing_data <- apply(df, 2, function(x) any(is.na(x)))
print(missing_data) # No missing data

# Update time column
df$time_updated <- lubridate::dmy_hms(df$Time)

#-----------------------
# Generate scatter plots
#-----------------------
# Only 2019-2020 data

# All data

# All hours on Saturday

# Hour 10 on all days of the week

