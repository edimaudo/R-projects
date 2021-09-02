
rm(list = ls()) #clear environment

#===================
## Load Packages
#===================
packages <- c('ggplot2', 'corrplot','tidyverse',"caret","dummies","fastDummies",'dplyr',
              'plyr','mlbench','caTools','doParallel')

# Load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}


df <- read_csv(file.choose())

# Delete first column
df[1] <- NULL

# Summary
summary(df)

#Check for missing variables
missing_data <- apply(df, 2, function(x) any(is.na(x)))
print(missing_data)

#drop missing variables
df <- na.omit(df)

