# Treatment of Postpartum Depression by Increase of Resveratrol-rich diet.

# Clear environment
rm(list = ls())
#=============
# Package Information
#=============
packages <- c('ggplot2', 'corrplot','tidyverse','readxl','scales','dplyr')
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}
#=============
# Data
#=============
df <- read_excel("book1_fall2021_CN.xlsx", sheet="data")
#=============
# Graph
#=============
ggplot(data = df, aes(x = reorder(Treatment, Outcome), y = Outcome, fill = Experiment)) + 
geom_bar(stat = "identity", position = "dodge") + 
coord_flip() + theme_minimal() + 
ggtitle("Treatment of Postpartum Depression by Increase of Resveratrol-rich diet") + 
xlab("Treatment") + 
ylab("Outcomes")
#=============
# CSV
#=============
write.csv(df,"output.csv")