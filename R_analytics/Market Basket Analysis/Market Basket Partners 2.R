#scene market basket analysis

#==============================
#notes
#==============================
#verticals
#overall
#high value
#scentourage members
#CARA
#youth
#app users

# association is by users and shops e.g. Swiss chalet
#==============================

#remove all data
rm(list=ls())

#libraries
library(tidyverse)
library(lubridate)
library(stringr)
library(data.table)
library(ggplot2)


#load files
file_names = c(
  "SceneAnalytics.dbo.LK_account_unique_member_identifier_sample10.csv",
  "SceneAnalytics.dbo.SP_AccountBalance.csv",
  "SceneAnalytics.dbo.SP_AccountHistory.csv",
  "SceneAnalytics.dbo.SP_AccountHistoryType.csv",
  "SceneAnalytics.dbo.SP_ActivityStatusScotiaScene_E.csv",
  "SceneAnalytics.dbo.SP_CineplexStore.csv",
  "SceneAnalytics.dbo.SP_CustomerDetail.csv",
  "SceneAnalytics.dbo.SP_CustomerExtension.csv",
  "SceneAnalytics.dbo.SP_DimProxyLocation.csv",
  "SceneAnalytics.dbo.SP_FactAttribute.csv",
  "SceneAnalytics.dbo.SP_FactEnrollment.csv",
  "SceneAnalytics.dbo.SP_LocationCARA.csv",
  "SceneAnalytics.dbo.SP_Location.csv",
  "SceneAnalytics.dbo.SP_Partner_E.csv",
  "SceneAnalytics.dbo.SP_Points.csv",
  "SceneAnalytics.dbo.SP_PointsType.csv",
  "SceneAnalytics.dbo.SP_PointTypeStatistics.csv",
  "SceneAnalytics.dbo.SP_ProxyPointTransaction_10.csv",
  "SceneAnalytics.dbo.SP_QualityActivity.csv",
  "SceneAnalytics.dbo.SP_Source.csv"
)

#clean filenames
filenames <- gsub("\\.csv$","", list.files(pattern="\\.csv$"))
#update working directory based on your Operating system
#setwd("/Users/edima/Documents/Queens/courses/big_data_865/project/sample01/") 
setwd("/Users/edima/Documents/Queens/courses/big_data_865/project/sample02/") 
for(i in filenames){
  assign(i, read.csv(paste(i, ".csv", sep="")))
}

# Questions for Katarina
# 
# Team performed market basket analysis 
# How would you define a high value user
# Does app user 
# What type of insights are you looking for in the different segments?
#   How much detail would you like to see?
#   Would you look to see a proof of concept with respect to a BI tool or tool like R that showcases the information?
#   What is the end use of the tools e.g. marketing campaign?

#Q&A with prof
# - how to properly collect the data
# - validate the market basket analysis
# - type of visuals are good for this type of analysis
# 
# - 
#   
#   
#   #only collect once since it is memory heavy
#   #can still use arules for the visualization
#   
#   
#   possible option is to get multiple data points + them combine them on your local machine and then run the arules and arules viz
# Also consider using the other columns in point example the ex_transactiondescription

#==============================
#market basket analysis
#==============================

#load libraries
library(arules)
library(arulesViz)

#points data
point_data <- SceneAnalytics.dbo.SP_Points %>%
  rename(Ex_sourceid = ex_sourceid) %>%
  left_join(SceneAnalytics.dbo.SP_Partner_E, "Ex_sourceid")

#update Unique ID
point_data <- point_data %>%
  mutate(Unique_member_identifier = as.character(Unique_member_identifier))

#point_data <- point_data %>%
#  filter(!is.na(PartnerName))

#============================
#market basket overall
#============================

all_data <- point_data
all_data <- all_data %>%
  mutate(ex = as.character(PartnerName))

# test_all <- all_data %>%
#   group_by(Unique_member_identifier) %>%
#   summarise(
#     count_id = n_distinct(Unique_member_identifier)
#   )
# 
# test_all <- test_all %>%
#   arrange(desc(count_id) )

#change to dataframe
mba_all <- as.data.frame(all_data)

#prep data for apriori algorithm
mba_all_trans <- as(split(mba_all[,"PartnerName"], 
                          mba_all[,"Unique_member_identifier"]), "transactions")

#rules using apriori
mba_all_rules <- apriori(mba_all_trans, 
                         parameter = list(supp = 0.00006, conf = 0.000025, 
                                          target = "rules", minlen = 2))
summary(mba_all_rules)
inspect(mba_all_rules[1:6])
#============================
#youth (18-24)
#============================

#get customer data
customer_data <- SceneAnalytics.dbo.SP_CustomerDetail %>%
  mutate(Unique_member_identifier = as.character(Unique_member_identifier)) %>%
  filter(age_class == "18-24")

#get youth data
youth_data <- customer_data %>%
  inner_join(point_data, "Unique_member_identifier")

youth_data <- youth_data %>%
  mutate(PartnerName = as.character(PartnerName))

mba_youth <- as.data.frame(youth_data)

#prep data for apriori algorithm
mba_youth_trans <- as(split(mba_youth[,"PartnerName"], 
                            mba_youth[,"Unique_member_identifier"]), "transactions")

#rules
mba_youth_rules <- apriori(mba_youth_trans, 
                           parameter = list(supp = 0.00006, conf = 0.000025, 
                                            target = "rules", minlen = 2))
summary(mba_youth_rules)
inspect(mba_youth_rules[1:4])

#============================
#app users
#============================

#write.csv(SceneAnalytics.dbo.SP_AccountHistoryType$Descr,"typedesc.csv")

account_hist_type <- SceneAnalytics.dbo.SP_AccountHistoryType %>%
  filter(Descr %in% c('Customer logged into Scene API Web Login',
                      'Customer logged into Scene API MobileApp'))

account_hist <- account_hist_type %>% 
  right_join(SceneAnalytics.dbo.SP_AccountHistory, "AccountHistoryTypeID")

account_hist <- account_hist %>%
  mutate(Unique_member_identifier = as.character(Unique_member_identifier))

account_data <- account_hist %>%
  right_join(point_data, "Unique_member_identifier")

#items and transactions
mba_app1 <- as.data.frame(account_data)

#prep data for apriori algorithm
mba_app_trans1 <- as(split(mba_app1[,"PartnerName"], 
                           mba_app1[,"Unique_member_identifier"]), "transactions")

#rules
mba_app_rules1 <- apriori(mba_app_trans1, 
                          parameter = list(supp = 0.00006, conf = 0.000025, 
                                           target = "rules", minlen = 2))
summary(mba_app_rules1)
inspect(mba_app_rules1[1:6])


#-------------------------------------------------------
enrollment <- SceneAnalytics.dbo.SP_FactEnrollment %>%
  rename(SourceID = EnrollmentSourceKey)

enrollment <- enrollment %>%
  left_join(SceneAnalytics.dbo.SP_Source,"SourceID")

#filter for key words
enrollment <- enrollment %>%
  filter(Name %in% c('Mobile','Scene App',"Mobile Apps", "Scotia Mobile Wallet","m.scene.ca"))

#update unique id
app_data <- enrollment %>%
  mutate(Unique_member_identifier = as.character(Unique_member_identifier))

#combine with point data
app_data <- app_data %>%
  inner_join(point_data, "Unique_member_identifier")

#items and transactions
mba_app <- as.data.frame(app_data)

#prep data for apriori algorithm
mba_app_trans <- as(split(mba_app[,"PartnerName"], 
                          mba_app[,"Unique_member_identifier"]), "transactions")

#rules
mba_app_rules <- apriori(mba_app_trans, 
                         parameter = list(supp = 0.00006, conf = 0.000025, 
                                          target = "rules", minlen = 2))
summary(mba_app_rules)
inspect(mba_app_rules[1:2])

#============================
#scentourage
#============================

scentourage_enrollment <- SceneAnalytics.dbo.SP_FactEnrollment %>%
  rename(SourceID = EnrollmentSourceKey)

scentourage_enrollment <- scentourage_enrollment %>%
  left_join(SceneAnalytics.dbo.SP_Source,"SourceID")

#filter for key words
scentourage_enrollment <- scentourage_enrollment %>%
  filter(Name %in% c('SCENEtourage'))

#update unique id
scentourage_data <- scentourage_enrollment %>%
  mutate(Unique_member_identifier = as.character(Unique_member_identifier))

#combine with point data
scentourage_data <- scentourage_data %>%
  inner_join(point_data, "Unique_member_identifier")

#items and transactions
mba_scentourage <- as.data.frame(scentourage_data)

#prep data for apriori algorithm
mba_scentourage_trans <- as(split(mba_scentourage[,"PartnerName"], 
                                  mba_scentourage[,"Unique_member_identifier"]), "transactions")

#rules
mba_scentourage_rules <- apriori(mba_scentourage_trans, 
                                 parameter = list(supp = 0.00006, conf = 0.000025, 
                                                  target = "rules", minlen = 2))
summary(mba_scentourage_rules)
inspect(mba_scentourage_rules[1])

#============================
#Cara (i.e., those who have shopped in any brand)
#============================
cara_location <- SceneAnalytics.dbo.SP_LocationCARA %>% 
  rename(Ex_sourceid = ex_sourceID)

cara_data <- point_data %>%
  left_join(cara_location, "Ex_sourceid")

cara_data <- cara_data %>%
  mutate(PartnerName = as.character(PartnerName))

#change to dataframe
mba_cara <- as.data.frame(cara_data)

#prep data for apriori algorithm
mba_cara_trans <- as(split(mba_cara[,"PartnerName"], 
                           mba_cara[,"Unique_member_identifier"]), "transactions")

#rules
mba_cara_rules <- apriori(mba_cara_trans, 
                          parameter = list(supp = 0.00006, conf = 0.000025, 
                                           target = "rules", minlen = 2))
summary(mba_cara_rules)
inspect(mba_cara_rules[1:6])


# #----------
# #cara other
# #----------
# library(plyr)
# library(dplyr)
# CARA <- full_all(list(SceneAnalytics.dbo.SP_Points, SceneAnalytics.dbo.SP_Partner_E,
#                        SceneAnalytics.dbo.SP_ProxyPointTransaction_10), by = "ex_sourceID", type='union')
# CARA
# 
# # List of  CARA MBA
# MBA <- select(CARA, "ex_transactiondescription",    "PartnerName", "brand")
# MBA
# 
# library("arules")
# # Getting the rules
# rules <- apriori(MBA, parameter = list(supp = 0.001, conf = 0.8))
# 
# # Show the top 5 rules, but only 2 digits
# options(digits=2)
# inspect(rules[1:5])


#============================
#high value (not defined but assuming customer points > 1500)
#============================
high_value_temp <- SceneAnalytics.dbo.SP_AccountBalance %>%
  group_by(Unique_member_identifier) %>%
  summarize(
    total_points = sum(PointsTotal)
  )

high_value_data <- high_value_temp %>%
  mutate(Unique_member_identifier = as.character(Unique_member_identifier))

#combine with point data
high_value_data <- high_value_data %>%
  right_join(point_data, "Unique_member_identifier")

high_value_data <- high_value_data %>%
  mutate(PartnerName = as.character(PartnerName)) %>%
  filter(points > 5000)

#items and transactions
mba_high_value <- as.data.frame(high_value_data)

#prep data for apriori algorithm
mba_high_value_trans <- as(split(mba_high_value[,"PartnerName"], 
                                 mba_high_value[,"Unique_member_identifier"]), "transactions")

#rules
mba_high_value_rules <- apriori(mba_high_value_trans, 
                                parameter = list(supp = 0.00006, conf = 0.000025, 
                                                 target = "rules", minlen = 2))
summary(mba_high_value_rules)
inspect(mba_high_value_rules[1:2])

#============================
#recommendations using recommender lab
#============================
library(recommenderlab)


dat <- as(mba_all_trans, "binaryRatingMatrix")
rec <- Recommender(dat, method = "AR", 
                   parameter=list(supp = 0.00006, conf = 0.000025))
getModel(rec)

pred <- predict(rec, dat[1:5,])
as(pred, "list")

#=====================
#Q&A
#How far can we tweak the market basket analysis if it is not providing any good insights
#can we do more data exploration
