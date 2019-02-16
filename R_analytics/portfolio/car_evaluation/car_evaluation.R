#clear enviroment
rm(list=ls())

#packages
packages <- c('ggplot2', 'corrplot','tidyverse','caret','mlbench','mice', 'caTools')
#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}
#load data
df <- read.table(file.choose(),sep=",")

glimpse(df)

#rename columns
names(df) <- c('buying', 'paint', 'doors', 'persons', 'lug_boot', 'safety','class')

#recode class
df$class <- recode_factor(df$class, "acc" = "2","good" = "3","vgood" = "4", "unacc" = "1")
#recode buying
df$buying <- recode_factor(df$buying, "low" = "1","med" = "2","high" = "3", "vhigh" = "4")
#recode paint
df$paint <- recode_factor(df$paint,  "low" = "1","med" = "2","high" = "3", "vhigh" = "4")
#recode lugboot
df$lug_boot <- recode_factor(df$lug_boot, "small" = "1","med" = "2","big" = "3")
#recode safety
df$safety <- recode_factor(df$safety, "low" = "1","med" = "2","high" = "3")

