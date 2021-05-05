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


# #Generate column information
df$Traditional_401K_Contribution <- 0
df$Tradtional_401K_ROTH_Contribution <- 0
df$Google_Match_ROTH_CONVERSION <- 0
df$RMD <- "-"
df$RMD_PERCENT <- 0

#GENERATE ACTION

#GENERATE 401K PENALTY

#Taxable Traditional 401K

# Non Taxable Backdoor roth ira

# Non taxable roth 401K

# Taxable Google Match

# Roth 401k contrib	

# Max 401k contrib

# Non Taxable after tax 401K

# Taxable Regular Income

# SUM BALANCE TAXABLE

# SUM BALANCE NON TAXABLE

# TOTAL (SUM BALANCE TAXABLE + SUM BALANCE NON TAXABLE)