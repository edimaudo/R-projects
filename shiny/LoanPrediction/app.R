rm(list=ls()) # clear environment
#packages
packages <- c("tidyverse",'shiny','shinydashboard','ggplot2', 
              'corrplot','caret','mice', 'caTools',
              'dummies','cluster','factoextra','psy','lattice',
              'nFactors','scales','NbClust',
              "fastDummies",'FactoMineR','factoextra','readxl','scales',
              'dplyr','mlbench','caTools','gridExtra','doParallel')
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

