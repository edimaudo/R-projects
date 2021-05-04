# Clear environment
rm(list = ls()) 

# libraries
packages <- c('ggplot2', 'corrplot','tidyverse',"caret","dummies","fastDummies"
              ,'FactoMineR','factoextra','readxl','scales','dplyr','mlbench','caTools',
              'gridExtra','doParallel','readxl')

# load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

df <- read_excel(file.choose())

# CONSTANTS
RETIREMENT_AGE <- 60
BASE_RMD <- 25
RATE_OF_RETURN <- 1.07
PENALTY_AGE_401K <- 60
INFLATION_5_YEAR <- 1.1292
PROMO <- 1.3
RETIREMENT_EXPENSE <- 75
INFLATION_RATE <- 1.0246
TAX_BRACKET_INFLATION <- 1.01
LIMIT_INCREASE_401K <- 1.005
MAX_401K_CONTRIBUTION <- 19.5
MAX_401K_CATCHUP <- 26
IRA_CONTRIBUTION <- 6
IRA_CATCHUP <- 6.5
AFTER_TAX_CONTRIBUTION <- 28.75
AFTER_TAX_CATCHUP <- 25.5
MAX_RETIREMENT_ACCOUNT <- 58
MAX_RETIREMENT_CATCHUP <- 64.5
