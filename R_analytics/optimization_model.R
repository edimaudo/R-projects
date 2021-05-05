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

lapply

#GENERATE ACTION
# Create an empty list
list <- c()
# Create a for statement to populate the list
for (i in seq(1, 4, by=1)) {
  list[[i]] <- i*i
}
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



tax_calculator <- function(taxable_withdrawal, tax_bracket_inflation){
  standard_deduction <- 25.1
  
  downfloated_taxable_withdrawal <- (taxable_withdrawal / tax_bracket_inflation) - standard_deduction
  
  noadjusted_tax <- 0
  
  taxCalculator <- 0
  
  If (downfloated_taxable_withdrawal <= 0) {
    noadjusted_tax <- 0
  } else if (downfloated_taxable_withdrawal < 19.05){
    noadjusted_tax <- downfloated_taxable_withdrawal * 0.1
  } else if(downfloated_taxable_withdrawal < 77.4) {
    noadjusted_tax <- 1.9 + ((downfloated_taxable_withdrawal - 19.05) * 0.12)
  } else if(downfloated_taxable_withdrawal < 77.4){
    noadjusted_tax <- 1.9 + ((downfloated_taxable_withdrawal - 19.05) * 0.12)
  } else if (downfloated_taxable_withdrawal < 165){
    noadjusted_tax <- 8.9 + ((downfloated_taxable_withdrawal - 77.4) * 0.22)
  } else if (downfloated_taxable_withdrawal < 315){
    noadjusted_tax <- 28.17 + ((downfloated_taxable_withdrawal - 165) * 0.24)
  } else if (downfloated_taxable_withdrawal < 400){
    noadjusted_tax <- 64.17 + ((downfloated_taxable_withdrawal - 315) * 0.32)
  } else if (downfloated_taxable_withdrawal < 600){
    noadjusted_tax <- 91.37 + ((downfloated_taxable_withdrawal - 400) * 0.35)
  } else {
    noadjusted_tax <- 161.37 + ((downfloated_taxable_withdrawal - 600) * 0.37)
  }
  
  taxCalculator <- tax_bracket_inflation * noadjusted_tax
  return (taxCalculator)
  
}














Loop
taxCalculator = tax_bracket_inflation * noadjusted_tax
End Function