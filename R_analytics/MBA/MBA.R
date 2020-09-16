# Objective
# Build MBA for provided data

# =======================================================
rm(list=ls()) #clear environment
# =======================================================
# load packages
# =======================================================
#devtools::install_github("rsquaredacademy/mbar") #update to clean up the data

packages <- c('ggplot2', 'corrplot','tidyverse',
              'dplyr','tidyr','readr','mbar',
              'stringr','colorspace','arules','arulesViz')
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

# =======================================================
# load data
# =======================================================
df <- read.delim(file.choose(),header = TRUE, sep = ",") #dataset
hierarchy <- read.csv(file.choose())

df2 <- df %>%
  inner_join(hierarchy,"MasterBarcodeKey")

# =======================================================
# Market Basket analysis
# =======================================================

set.seed(123) #for reproducibility
#change to dataframe
mba_all <- as.data.frame(df2)

#prep data for apriori algorithm
mba_all_trans <- as(split(mba_all[,"SkuName"], 
                          unique(mba_all[,"InvoiceID"])), "transactions")

#rules using apriori
mba_all_rules <- apriori(mba_all_trans, 
                         parameter = list(supp = 0.001, conf = 0.002,
                                          target = "rules", minlen = 2))

#worked supp 0.001 and confidence 0.002
#mba summary for overall
summary(mba_all_rules)

#inspect top rules by confidence
inspect(sort(mba_all_rules, by = "confidence")[1:25])

#inspect top rules by lift
inspect(sort(mba_all_rules, by = "lift")[1:25])

#visualization
#standard visualization to plot everything
plot(mba_all_rules)

#view graphically by lift
plot(head(sort(mba_all_rules, by = "lift"), n=200), method="graph", control=list(cex=.8))

#view graphically by confidence
plot(head(sort(mba_all_rules, by = "confidence"), n=25), method="graph", control=list(cex=.8))

#view as a matrix
plot(mba_all_rules, method="matrix", measure=c("lift","confidence"), 
     control=list(reorder=TRUE, col=sequential_hcl(200)))

#view by grouped matrix by 
plot(mba_all_rules, method="grouped", measure=c("lift","confidence"), control=list(col=sequential_hcl(100)))

#View rules interactively using the viewer
inspectDT(mba_all_rules)