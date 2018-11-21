#remove old data
rm(list=ls())

#packages
packages <- c('tidyverse','lubridate')

#load libraries
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#load data
mrr <- read.csv(file.choose(),sep = ";", header = TRUE)
contact <- read.csv(file.choose(),sep = ";", header = TRUE)
mrr <- na.omit(mrr)

mrr$UpdateDate <- mdy(mrr$Date.of.subscription)
mrr$month <- month(mdy(mrr$Date.of.subscription))

#- Select the total MRR, and company_id per company
total_mrr <- mrr %>%
  group_by(Company.ID) %>%
  summarise(sum(MRR))
  
#- Show the total added MRR per month (use the "Date of Subscription" column for all date related queries)
month_totalMRR <- mrr %>%
  group_by(month) %>%
  summarise(sum(MRR))
            
#- Show the number of associated contacts and the latest payment method per account 
num_associates <- contact %>%
  group_by(Company.ID) %>%
  summarise(associate_count = n())

lastest_payment <- mrr %>%
  group_by(Company.ID) %>%
  summarise(max_date = max(UpdateDate)) %>%
  inner_join(mrr, by = c("Company.ID" = "Company.ID")) %>%
  select(Company.ID, Payment.method) %>%
  inner_join(num_associates, by= c('Company.ID'='Company.ID')) %>%
  select(Company.ID, Payment.method, associate_count)
  

