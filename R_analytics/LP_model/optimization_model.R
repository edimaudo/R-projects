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

#=============================
# Load data
#=============================
df <- read_excel(file.choose())

#
# Experiment
df1 <- read_excel(file.choose())


#=============================
# Tax calculator function
#=============================
tax_calculator <- function(taxable_withdrawal, tax_bracket_inflation){
  standard_deduction <- 25.1
  downfloated_taxable_withdrawal <- (taxable_withdrawal / tax_bracket_inflation) - standard_deduction
  noadjusted_tax <- 0
  taxCalculator <- 0
  
  if (downfloated_taxable_withdrawal <= 0) {
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

#=============================
# CONSTANTS
#=============================
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




## Create an empty list
# list <- c()
# # Create a for statement to populate the list
# for (i in seq(1, 4, by=1)) {
#   list[[i]] <- i*i
# }

#lapply

#=============================
# Generate data
#=============================
df$Traditional_401K_Contribution <- 0
df$Tradtional_401K_ROTH_Contribution <- 0
df$Google_Match_ROTH_CONVERSION <- 0
df$RMD <- "-"
df$RMD_PERCENT <- 0


#GENERATE ACTION COLUMN
action <- c()
for (i in seq(1, length(df$Power), by= 1)){
  age_outcome <- ""
  if (df$Age[[i]] >= RETIREMENT_AGE  ){
    age_outcome <- "Retired"
  }  else if (df$Age[[i]] >= 50) {
    age_outcome <- "Catchup"
  }else {
    age_outcome <- "Contributing"
  }
  action [[i]] <- age_outcome
}
df$Action <- action

#GENERATE 401K PENALTY column
penalty_401K <- c()
for (i in seq(1, length(df$Power), by= 1)){
  penalty_outcome <- ""
  if (df$Action[[i]]=="Retired" &&  df$Age[[i]] < PENALTY_AGE_401K){
    penalty_outcome <- TRUE
  } else {
    penalty_outcome <- FALSE
  }
  penalty_401K[[i]] <- penalty_outcome
}
df$penalty_401K <- penalty_401K

# Generate Max 401k contrib column
max_401K_contribution <- c()
for (i in seq(1, length(df$Power), by= 1)){
  outcome_value <- 0
  if(df$Action[[i]]=="Contributing"){
    outcome_value <-  round(MAX_401K_CONTRIBUTION* (LIMIT_INCREASE_401K^df$Power[[i]]),2)
  } else if(df$Action[[i]]=="Catchup") {
    outcome_value <- round(MAX_401K_CATCHUP * (LIMIT_INCREASE_401K^df$Power[[i]]),2)
  } else {
    outcome_value = 0
  }
  max_401K_contribution[[i]] <- outcome_value
}
df$max_401K_contribution <- max_401K_contribution

# Roth 401k contrib	
df$roth_401K_contribution <- df$max_401K_contribution - df$Traditional_401K_Contribution

# Non Taxable Backdoor roth IRA
non_taxable_backdoor_roth_ira <- c()
for (i in seq(1, length(df$Power), by= 1)){
  outcome_value <- 0
  if (i == 1){
    outcome_value <- 32
  } else if(df$Action[[i]]=="Contributing" ){
    outcome_value  <- round(RATE_OF_RETURN*non_taxable_backdoor_roth_ira[[i-1]] + 
      (IRA_CONTRIBUTION * LIMIT_INCREASE_401K^df$Power[[i-1]]),2)
  } else if (df$Action[[i]]=="Catchup"){
    outcome_value <- round((RATE_OF_RETURN*non_taxable_backdoor_roth_ira[[i-1]]) + 
      (IRA_CATCHUP * LIMIT_INCREASE_401K^df$Power[[i-1]]),2)
  } else {
    outcome_value <- round(RATE_OF_RETURN*non_taxable_backdoor_roth_ira[[i-1]],2)
  }
  
  non_taxable_backdoor_roth_ira[[i]] <- outcome_value
}

df$non_taxable_backdoor_roth_ira <- non_taxable_backdoor_roth_ira

# Non taxable roth 401K
non_taxable_roth_401k <- c()
for (i in seq(1, length(df$Power), by= 1)){
  outcome_value <- 0
  if (i == 1){
    outcome_value <- df$roth_401K_contribution[[i]]
  } else {
    outcome_value <- round(RATE_OF_RETURN*non_taxable_roth_401k[i-1] + 
                             df$roth_401K_contribution[[i]] + 
                             df$Tradtional_401K_ROTH_Contribution[[i-1]],2)
  }
  non_taxable_roth_401k[[i]] <- outcome_value
  
}
df$non_taxable_roth_401k  <- non_taxable_roth_401k 







# Taxable Google Match

# Non Taxable after tax 401K

# Taxable Regular Income

#Taxable Traditional 401K


#RMD 401K

#RMD Match

# SUM BALANCE TAXABLE

# SUM BALANCE NON TAXABLE

# TOTAL (SUM BALANCE TAXABLE + SUM BALANCE NON TAXABLE)


# Taxable Withdrawal and conversion

# ROth withdrawal

# Retirement income

# Minimum living expense

# Total inflation

# Tax Bracket inflation

# Tax

# Cumulative Tax

# Cumulative feels like tax



#=============================
# Model setup
#=============================




#=============================
# Model solution
#=============================







