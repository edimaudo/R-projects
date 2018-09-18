rm(list=ls())

library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization

df <- USArrests
df <- na.omit(df)

df <- scale(df)