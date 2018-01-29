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

#==============================
#exploratory analysis
#==============================
#customer age ranges
summary(SceneAnalytics.dbo.SP_CustomerDetail$age_class)
ggplot(data = SceneAnalytics.dbo.SP_CustomerDetail, mapping = aes(x=as.factor(age_class) ))+
  geom_bar()
#18-24 and 25-34 and 35-44 account for a large percentage of the market

#province
summary(SceneAnalytics.dbo.SP_CustomerDetail$StateProv)
ggplot(data = SceneAnalytics.dbo.SP_CustomerDetail, mapping = aes(x=as.factor(StateProv) ))+
  geom_bar()
#ontario is the most important market

#summarize city information
cityInfo <- SceneAnalytics.dbo.SP_CustomerDetail %>%
  group_by(City) %>%
  summarise(count= n(), percentage = n() / nrow(SceneAnalytics.dbo.SP_CustomerDetail)) %>%
  arrange(desc(count))
#Toronto, Calgary, Edmonton, Vancouver are very important

#other possible questions if needed
#How many people in each vertical

#vertical questions
#scentourage & 18-24 & app users
#-how often do they go
#-how much do the spend, What is the average spend + do they buy from the concessions, average points
#-what cities and province
#-what cinemas
#-what movie times
#-type of movie goer (purchases ticket online, Tuesday watcher?)
#-do they use other channels (email, sms)
#- how many active user?
#type of points being used?
#type of card being used

#CARA
#type of card being used?
#average transaction
#no of points used, 
#active users
#type of points
# merchant information
# location with respect to points, transactions w.r.t city, province

#high value
#all the questions asked for the other verticals

#===============
#rfm analysis
#===============
rfm_everyone <- SceneAnalytics.dbo.SP_Points %>%
  select(Unique_member_identifier, points, pointdt, pointid, TransAmount)

#drop na in rfm_everyone
rfm_everyone <- rfm_everyone %>%
  drop_na()

#convert date
rfm_everyone <- rfm_everyone %>%
  mutate(pointdt = as.Date(pointdt,"%Y-%m-%d"))

rfm_everyone <- rfm_everyone %>% 
  group_by(Unique_member_identifier) %>% 
  summarise(recency=as.numeric(as.Date("2018-01-22")-max(pointdt)),
            frequency=n_distinct(pointid), monetary= sum(TransAmount)) 

#rfm for high value

summary(rfm_everyone) #get more info from professor about high value
#high value 
rfm_everyone <-rfm_everyone %>%
  arrange(desc(monetary))
print(rfm_everyone[1:50,])
#==============================
#market basket analysis
#==============================

#load libraries
library(arules)
library(arulesViz)
library(arulesSequences)
library(Matrix)

#points data
point_data <- SceneAnalytics.dbo.SP_Points %>%
  rename(Ex_sourceid = ex_sourceid) %>%
  left_join(SceneAnalytics.dbo.SP_Partner_E, "Ex_sourceid")

#update Unique ID
point_data <- point_data %>%
  mutate(Unique_member_identifier = as.character(Unique_member_identifier))

#============================
#market basket overall
#============================

all_data <- point_data
all_data <- all_data %>%
  mutate(PartnerName = as.character(PartnerName))

test_all <- all_data %>%
  group_by(Unique_member_identifier) %>%
  summarise(
    count_id = n_distinct(Unique_member_identifier)
  )

test_all <- test_all %>%
  arrange(desc(count_id) )

#change to dataframe
mba_all <- as.data.frame(all_data)

#prep data for apriori algorithm
mba_all_trans <- as(split(mba_all[,"PartnerName"], 
                          mba_all[,"Unique_member_identifier"]), "transactions")

#rules using apriori
mba_all_rules <- apriori(mba_all_trans, 
                         parameter = list(supp = 0.00006, conf = 0.000025, 
                                          target = "rules", minlen = 1))
summary(mba_all_rules)
inspect(mba_all_rules[1:10])
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
                                            target = "rules", minlen = 1))
summary(mba_youth_rules)
inspect(mba_youth_rules[1:7])

#============================
#app users
#============================

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
                                          target = "rules", minlen = 1))
summary(mba_app_rules)

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
                                                  target = "rules", minlen = 1))
summary(mba_scentourage_rules)

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
                                           target = "rules", minlen = 1))
summary(mba_cara_rules)

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
  filter(points > 1000)

#items and transactions
mba_high_value <- as.data.frame(high_value_data)

#prep data for apriori algorithm
mba_high_value_trans <- as(split(mba_high_value[,"PartnerName"], 
                                 mba_high_value[,"Unique_member_identifier"]), "transactions")

#rules
mba_high_value_rules <- apriori(mba_high_value_trans, 
                                parameter = list(supp = 0.00006, conf = 0.000025, 
                                                 target = "rules", minlen = 1))
summary(mba_high_value_rules)

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