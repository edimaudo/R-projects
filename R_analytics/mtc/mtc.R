#===================
## Load Libraries
#===================
rm(list = ls()) #clear environment

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

#===================
## Load Data
#===================
df <- read_excel("mtc.xlsx")

#===================
# Data Overview
#===================

# Summary
summary(df)

# Check for missing variables
missing_data <- apply(df, 2, function(x) any(is.na(x)))
print(missing_data)

# Check for for imbalance
table(df$label)

# drop missing values
df <- na.omit(df)

#=================
# Modeling
#=================

# Select columns
df <- df %>%
  select(CANTIDAD,CANTIDAD_FISICA,CODIGO_CERTIFICADO,NOMBRE_PROVEEDOR,NOMBRE_EQUIPO,
         MODELO, PARTIDA_ARANCELARIA,MARCA_EQUIPO,FACTURA,UM_FISICA_ID,label)

#Label Encoder
labelEncoder <-function(x){
  as.numeric(factor(x))-1
}
#normalize data
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

