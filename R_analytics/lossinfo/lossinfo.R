# 1,500 general liability claims randomly chosen from late settlement lags that were provided by Insurance Services Office, Inc. 
# Each claim consists of an indemnity payment (Loss) and an allocated loss adjustment expense (ALAE). 
# ALAE are types of insurance company expenses that are specifically attributable to the settlement of individual claims such as lawyers' fees and claims investigation expenses. 
# The dataset also has the limit of the policy (Limit) and an attribute called Censored which gives the
# row names of the indemnity payments that were capped at their policy limit.

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

# Loss Histogram
ggplot(df, aes(x=Loss)) + geom_histogram()

# ALAE Histogram
ggplot(df, aes(x=ALAE)) + geom_histogram()

