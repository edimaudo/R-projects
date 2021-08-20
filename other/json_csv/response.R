# update json to csv

#===================
## Load Libraries
#===================
rm(list = ls()) #clear environment

# Packages
packages <- c('ggplot2', 'corrplot','tidyverse','rjson','jsonlite')

# Load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#===================
# Load data
#===================
# using rjson
df <- fromJSON(file = file.choose())
df.backup <- df

df1 <- as.data.frame(df)

# using jsonlite
df <- jsonlite::fromJSON(file.choose(), flatten = TRUE)
df.backup <- df

df2 <- as.data.frame(df)

#===================
# Convert to csv
#===================