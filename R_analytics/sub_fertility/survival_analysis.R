# Survivial analysis
# Survival analysis for a cohort of infertile couples attempting to conceive with censoring 
# for conception or withdrawal from study
#  1. Kaplan Meier survival analysis (plotted cumulatively or 1-x) with censoring for 2 events 
#  2. Cumulative competing risks method 
# 

rm(list = ls())
#packages 
packages <- c('ggplot2', 'corrplot','tidyverse','shiny','shinydashboard',
              'caret','dummies','mlbench','tidyr','Matrix','lubridate','survminer',
              'survival','data.table','vtreat', 'rsample','scales','readxl')
#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#load data
df <- read_excel(file.choose())

#backup data
df.backup <- df

#summary statistics
summary(df)

#  1. Kaplan Meier survival analysis (plotted cumulatively or 1-x) with censoring for 2 events 
fit <- survfit(Surv(days_try_conceive,days_to_censor) ~ rrm_outcome, data = df)
print(fit)


#  2. Cumulative competing risks method 
ggsurvplot(fit,
           pval = TRUE, conf.int = TRUE,
           risk.table = TRUE, # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata", # Change line type by groups
           surv.median.line = "hv", # Specify median survival
           ggtheme = theme_bw(), # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF"))
