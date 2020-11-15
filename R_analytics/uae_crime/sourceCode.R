#===================
## Load Libraries:
#===================
rm(list = ls()) #clear environment

# libraries
packages <- c('ggplot2', 'corrplot','tidyverse',"caret","dummies","fastDummies",
              'doParallel','extraTrees','FactoMineR','factoextra','readxl','scales',
              'gridExtra','lubridate')
# load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#===================
## Load Data
#===================