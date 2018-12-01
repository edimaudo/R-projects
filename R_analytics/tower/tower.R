#remove old data
rm(list=ls())
#packages
packages <- c('tidyverse','lubridate','cluster','ggplot2','factoextra',
              'corrplot','psy','lattice','nFactors','RColorBrewer','scales',
              'NbClust')
#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}
#load data
df <- read.csv(file.choose())
#