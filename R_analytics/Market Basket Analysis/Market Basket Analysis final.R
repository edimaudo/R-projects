#==============================
#scene market basket analysis
#==============================
#objectives
# - Perform market basket analysis to find what people are buying
#- Help Scene find new potential partners
#==============================
#verticals
#==============================
#overall
#high value
#scentourage members
#CARA
#youth
#app users

#remove all data
rm(list=ls())

#load libraries
library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(colorspace)
library(arules)
library(arulesViz)

#list of files
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

#update working directory
setwd("/Users/edima/Documents/Queens/courses/big_data_865/project/sample02/") 

#clean filenames
filenames <- gsub("\\.csv$","", list.files(pattern="\\.csv$"))
#load the files
for(i in filenames){
  assign(i, read.csv(paste(i, ".csv", sep="")))
}

#points information
point_data <- SceneAnalytics.dbo.SP_Points %>%
  #filter(pointtypeid %in% c('11','1282','1252','1290',
  #'1322','1253','1254','12','1283','1323')) %>% #black card
  rename(Ex_sourceid = ex_sourceid) %>%
  left_join(SceneAnalytics.dbo.SP_Partner_E, "Ex_sourceid")

#================
#data cleaning
#================

#clean ex_transaction data
Clean_String <- function(extransaction){
  temp <- extransaction
  temp <- stringr::str_replace_all(temp,"s/[\x80-\xFF]//g","") #remove multibyte string
  #remove special characters
  temp <- stringr::str_replace_all(temp,"[:$()-*#0123456789\\s]", " ")
  temp <- stringr::str_replace_all(temp, '[-#\'\\._/]', '') # special characters
  #remove white space on both sides
  temp <- trimws(temp,"b")
  #shrink down to just a single white space
  temp <- stringr::str_replace_all(temp,"[\\s]+", " ")
  temp <- stringr::str_replace_all(temp, ' +', ' ')
  temp <- stringr::str_replace_all(temp, 'TIM HORTONS  BC', 'TIM HORTONS') #%>%
  temp <- stringr::str_replace_all(temp, ' QTH', '') #%>% # Not sure what this is, but I saw it
  temp <- stringr::str_replace_all(temp, 'CGCTIM HORTONS', 'TIM HORTONS') #%>%
  temp <- stringr::str_replace_all(temp, 'TIM HORTONS QPS', 'TIM HORTONS') #%>%
  temp <- stringr::str_replace_all(temp, 'TIM HORTONS ON', 'TIM HORTONS') #%>%
  temp <- stringr::str_replace_all(temp, 'TIM HORTONS  ON', 'TIM HORTONS') #%>%
 
  temp <- stringr::str_replace_all(temp, 'LANGARA COLLEGE QPS  BC', 'LANGARA COLLEGE') #%>%
  temp <- stringr::str_replace_all(temp, 'MCDONALDS Q', 'MCDONALDS') #%>%
  temp <- stringr::str_replace_all(temp, 'WENDYS QR', 'WENDYS') #%>%
  temp <- stringr::str_replace_all(temp, 'WENDYS Q R', 'WENDYS') #%>%
  temp <- stringr::str_replace_all(temp, '(TORONTO|VANCOUVER|OTTAWA|BRAMPTON|WINDSOR|BURLINGTON|
                                    IRVING|LONDON|SCARBOROUGH|BURNABY|MISSISSAUGA|
                                    |VERDUN|SURREY|KITCHENER|NORTH YORK|CONCORD|FORT STJOHN )', '') #%>% # Big city names
  temp <- stringr::str_replace_all(temp, '( AB | BC | MB| NB| NL| NS | NT | NU | ON | PE | QC | SK | YT)', '') #%>% #provinces

  temp <- stringr::str_replace_all(temp, 'VIRGIN MOBILE  QC', 'VIRGIN MOBILE') #%>%
  temp <- stringr::str_replace_all(temp, 'COMPASS VENDING  BC', 'COMPASS VENDING') #%>% 
  temp <- stringr::str_replace_all(temp, 'FRESHCO  ON', 'FRESHCO') #%>% 
  temp <- stringr::str_replace_all(temp, 'PIZZA PIZZA  ON', 'PIZZA PIZZA') #%>%
  temp <- stringr::str_replace_all(temp, 'PRESTO  ON', 'PRESTO') #%>%
  temp <- stringr::str_replace_all(temp, 'STARBUCKS  ON', 'STARBUCKS') #%>%
  temp <- stringr::str_replace_all(temp, 'DOLLARAMA  ON', 'DOLLARAMA') #%>%
  temp <- stringr::str_replace_all(temp, 'TIM HORTONS EDMONTON AB', 'TIM HORTONS') #%>%
  temp <- stringr::str_replace_all(temp, 'FREEDOM MOBILE INC ON', 'FREEDOM MOBILE') #%>%
  temp <- stringr::str_replace_all(temp, 'MCDONALDS  BC', 'MCDONALDS')
  temp <- stringr::str_replace_all(temp, 'MCDONALDS  ON', 'MCDONALDS')
  temp <- stringr::str_replace_all(temp, 'SUBWAY  ON', 'SUBWAY')
  temp <- stringr::str_replace_all(temp, 'NEEDS QPS', 'NEEDS')
  temp <- stringr::str_replace_all(temp, 'VESTA CHATR BC', 'VESTA CHATR')
  temp <- stringr::str_replace_all(temp, 'CINEPLEX QPS', 'CINEPLEX')
  temp <- stringr::str_replace_all(temp, 'FIDO Mobile ON', 'FIDO MOBILE')
  temp <- stringr::str_replace_all(temp, 'Cineplex PremiumVIP coupon adj', 'CINEPLEX')
  temp <- stringr::str_replace_all(temp, 'ROGERS ON', 'ROGERS')
  temp <- stringr::str_replace_all(temp, 'SOBEYS QPS', 'SOBEYS')
  temp <- stringr::str_replace_all(temp, 'SHELL C', 'SHELL')
  temp <- stringr::str_replace_all(temp, 'COSTCO WHOLESALE W', 'COTSCO WHOLESALE')
  temp <- stringr::str_replace_all(temp, 'CARA Points Earned', 'CARA')
  temp <- stringr::str_replace_all(temp, 'CIRCLE K IRVING QPS', 'CIRCLE K IRVING')
  temp <- stringr::str_replace_all(temp, 'UBER TRIP LCKE HELP.UBER', 'UBER')
  temp <- stringr::str_replace_all(temp, 'UBER TRIP F O HELP.UBERhelp.uber.com', 'UBER')
  temp <- stringr::str_replace_all(temp, 'UBER TRIP OVGES HELP.UBERhelp.uber.com', 'UBER')
  temp <- stringr::str_replace_all(temp, 'UBER BV', 'UBER')
  temp <- stringr::str_replace_all(temp, 'SCENE Admit', 'SCENE')
  temp <- stringr::str_replace_all(temp, 'SCENE $ OFF CONCESSIONS', 'SCENE')
  temp <- stringr::str_replace_all(temp, 'SCENE Premium Admit', 'SCENE')
  temp <- stringr::str_replace_all(temp, 'SCENE ACCT OPEN NEW CUST', 'SCENE')
  temp <- stringr::str_replace_all(temp, 'TIM HORTONS  ON', 'TIM HORTONS') #%>%
  temp <- stringr::str_replace_all(temp, 'INDIAN FOODLAND  ON', 'INDIAN FOODLAND') #%>%
  temp <- stringr::str_replace_all(temp, 'BURGER KING FORT BC', 'BURGER KING')
  temp <- stringr::str_replace_all(temp, 'SAFEWAY  BC', 'SAFEWAY')
  temp <- stringr::str_replace_all(temp, 'CA VENDING  ON', 'CA VENDING')
  temp <- stringr::str_replace_all(temp, 'CINEPLEX  ON', 'CINEPLEX')
  temp <- stringr::str_replace_all(temp, 'WALMART SUPERCENTER  ON', 'WALMART')
  temp <- stringr::str_replace_all(temp, 'SCENEtourage Movie Visit', 'SCENEtourage')
  temp <- stringr::str_replace_all(temp, 'SCENE app exclusive offer', 'SCENE')
  temp <- stringr::str_replace_all(temp, 'OSCAR SCENE ACCT OPEN', 'SCENE')
  return (temp)
}

#update Unique ID
point_data <- point_data %>%
  mutate(Unique_member_identifier = as.character(Unique_member_identifier))

point_data$ex_transactiondescription <- Clean_String(point_data$ex_transactiondescription)

#remove from transaction as they are random
filter_out = c("STEPGOALCOMPLETE","STEP_GOAL_COMPLETE","STEPSENABLED","SIGNUP","PAYROLL PAC, PAC PAD TXN",
               "INTERVENTION","VENDING","CONCESSIONS POPCORNMAIS ECLATE","CONCESSIONS DRINKSBOISSONS",
               "Outtakes Combos Points","ACTION_BONUS","GOOGLE Scopely Inc gcohelppay NS",
               "CONCESSIONS POPCORN MAIS ECLATE","CONCESSIONS - OUTTAKES-RESTOPLEX",
               "Online bonus en ligne","STEPS_ENABLED","ACTIONBONUS","Outtakes Combo Points",
               "ADULT TICKETS PREMIUM",'CONCESSIONS COMBO','CONCESSIONS POPTOPIA',
               'CONCESSIONS NACHOS','CONCESSIONS COUPONS',
               'CONCESSIONS FOOD ALIMENTS','CONCESSIONS DRINKS BOISSONS',
               'CONCESSIONS CANDY/BONBONS',"ADULT TICKETS-BILLETS POUR ADULTE",
               "Child Tickets Premium", "CONCESSIONS - CANDY/BONBONS",
               "CONCESSIONS - POPCORN-MAIS ECLATE","CONCESSIONS - DRINKS-BOISSONS",
               "Child Tickets","CONCESSIONS - COMBO","Timeplay - Timeplay - Timeplay reward. Rank",
               "CONCESSIONS - POPCORN-MAIS ECLATE", "Ve","CONCESSIONS DRINKSBOISSONS",
               "Timeplay - Timeplay - Timeplay reward. Rank , Ve","ADULT TICKETS VIP",
               "Concession Combo","CONCESSIONS CANDYBONBONS","ADULT TICKETSBILLETS POUR ADULTE",
               "CONCESSIONS OUTTAKESRESTOPLEX")

point_data <- point_data %>%
  filter(!ex_transactiondescription %in% filter_out)

#==============================
#market basket analysis
#==============================
set.seed(123) #for reproducibility
#============================
#market basket overall
#============================

all_data <- point_data
all_data <- all_data %>%
  mutate(PartnerName = as.character(PartnerName), 
         ex_transactiondescription = as.character(ex_transactiondescription))

#change to dataframe
mba_all <- as.data.frame(all_data)

#prep data for apriori algorithm
mba_all_trans <- as(split(mba_all[,"ex_transactiondescription"], 
                          unique(mba_all[,"Unique_member_identifier"])), "transactions")

#rules using apriori
mba_all_rules <- apriori(mba_all_trans, 
                         parameter = list(supp = 0.06, conf = 0.75, 
                                          target = "rules", minlen = 2))
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
plot(head(sort(mba_all_rules, by = "lift"), n=25), method="graph", control=list(cex=.8))
#view graphically by confidence
plot(head(sort(mba_all_rules, by = "confidence"), n=25), method="graph", control=list(cex=.8))

#view as a matrix
plot(mba_all_rules, method="matrix", measure=c("lift","confidence"), 
     control=list(reorder=TRUE, col=sequential_hcl(200)))

#view by grouped matrix by 
plot(mba_all_rules, method="grouped", measure=c("lift","confidence"), control=list(col=sequential_hcl(100)))

#View rules interactively using the viewer
inspectDT(mba_all_rules)

#all partners by transaction amount
# all_data_temp <- all_data %>%
#   group_by(ex_transactiondescription) %>%
#   summarize(
#     total_transaction = sum(TransAmount)
#   ) %>%
#   arrange(desc(total_transaction))
# View(all_data_temp)

#============================
#youth (18-24)
#============================

#get customer data
customer_data <- SceneAnalytics.dbo.SP_CustomerDetail %>%
  mutate(Unique_member_identifier = as.character(Unique_member_identifier)) %>%
  filter(age_class == "18-24")

#get youth data
youth_data <- customer_data %>%
  left_join(point_data, "Unique_member_identifier")

youth_data <- youth_data %>%
  mutate(PartnerName = as.character(PartnerName), 
         ex_transactiondescription = as.character(ex_transactiondescription))

mba_youth <- as.data.frame(youth_data)

#prep data for apriori algorithm
mba_youth_trans <- as(split(mba_youth[,"ex_transactiondescription"], 
                            unique(mba_youth[,"Unique_member_identifier"])), "transactions")

#rules
mba_youth_rules <- apriori(mba_youth_trans, 
                           parameter = list(supp = 0.5, conf = 0.75, 
                                            target = "rules", minlen = 2))
summary(mba_youth_rules)

#inspect top rules by confidence
inspect(sort(mba_youth_rules, by = "confidence")[1:25])

#inspect top rules by lift
inspect(sort(mba_youth_rules, by = "lift")[1:25])

#visualization
#standard visualization to plot everything
plot(mba_youth_rules)

#view graphically by lift
plot(head(sort(mba_youth_rules, by = "lift"), n=25), method="graph", control=list(cex=.8))
#view graphically by confidence
plot(head(sort(mba_youth_rules, by = "confidence"), n=25), method="graph", control=list(cex=.8))

#view as a matrix
plot(mba_youth_rules, method="matrix", measure=c("lift","confidence"), 
     control=list(reorder=TRUE, col=sequential_hcl(200)))

#view by grouped matrix by 
plot(mba_youth_rules, method="grouped", measure=c("lift","confidence"), control=list(col=sequential_hcl(100)))

#View rules interactively using the viewer
inspectDT(mba_youth_rules)


#============================
#app users
#============================

account_hist_type <- SceneAnalytics.dbo.SP_AccountHistoryType %>%
  filter(Descr %in% c('Customer logged into Scene API MobileApp'))

account_hist <- account_hist_type %>% 
  inner_join(SceneAnalytics.dbo.SP_AccountHistory, "AccountHistoryTypeID")

account_hist <- account_hist %>%
  mutate(Unique_member_identifier = as.character(Unique_member_identifier))

account_data <- account_hist %>%
  inner_join(point_data, "Unique_member_identifier")

#items and transactions
mba_app <- as.data.frame(account_data)

#prep data for apriori algorithm
mba_app_trans <- as(split(mba_app[,"ex_transactiondescription"], 
                            unique(mba_app[,"Unique_member_identifier"])), "transactions")

#rules
mba_app_rules <- apriori(mba_app_trans, 
                          parameter = list(supp = 0.06, conf = 0.75, 
                                           target = "rules", minlen = 2))
summary(mba_app_rules)

#inspect top rules by confidence
inspect(sort(mba_app_rules, by = "confidence")[1:25])

#inspect top rules by lift
inspect(sort(mba_app_rules, by = "lift")[1:25])

#visualization
#standard visualization to plot everything
plot(mba_app_rules)

#view graphically by lift
plot(head(sort(mba_app_rules, by = "lift"), n=25), method="graph", control=list(cex=.8))
#view graphically by confidence
plot(head(sort(mba_app_rules, by = "confidence"), n=25), method="graph", control=list(cex=.8))

#view as a matrix
plot(mba_app_rules, method="matrix", measure=c("lift","confidence"), 
     control=list(reorder=TRUE, col=sequential_hcl(200)))

#view by grouped matrix by 
plot(mba_app_rules, method="grouped", measure=c("lift","confidence"), control=list(col=sequential_hcl(100)))

#View rules interactively using the viewer
inspectDT(mba_app_rules)

#all partners by transaction amount
# account_data_temp <- account_data %>%
#   group_by(ex_transactiondescription) %>%
#   summarize(
#     total_transaction = sum(TransAmount)
#   ) %>%
#   arrange(desc(total_transaction))


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
  #right_join(point_data, "Unique_member_identifier")
  inner_join(point_data, "Unique_member_identifier")

#items and transactions
mba_scentourage <- as.data.frame(scentourage_data)

#prep data for apriori algorithm
mba_scentourage_trans <- as(split(mba_scentourage[,"ex_transactiondescription"], 
                                  unique(mba_scentourage[,"Unique_member_identifier"])),
                            "transactions")

#rules
mba_scentourage_rules <- apriori(mba_scentourage_trans, 
                                 parameter = list(supp = 0.06, conf = 0.75, 
                                                  target = "rules", minlen = 2))
summary(mba_scentourage_rules)

#inspect top rules by confidence
inspect(sort(mba_scentourage_rules, by = "confidence")[1:2])

#inspect top rules by lift
inspect(sort(mba_scentourage_rules, by = "lift")[1:2])

#all partners by transaction amount
# scentourage_data_temp <- scentourage_data %>%
#   group_by(ex_transactiondescription) %>%
#   summarize(
#     total_transaction = sum(TransAmount)
#   ) %>%
#   arrange(desc(total_transaction))

#============================
#Cara (i.e., those who have shopped in any brand)
#============================
cara_location <- SceneAnalytics.dbo.SP_LocationCARA %>% 
  rename(Ex_sourceid = ex_sourceID)

cara_data <- point_data %>%
  right_join(cara_location, "Ex_sourceid")

cara_data <- cara_data %>%
  mutate(PartnerName = as.character(PartnerName))

#change to dataframe
mba_cara <- as.data.frame(cara_data)

#prep data for apriori algorithm
mba_cara_trans <- as(split(mba_cara[,"ex_transactiondescription"], 
                           unique(mba_cara[,"Unique_member_identifier"])), "transactions")

#rules
mba_cara_rules <- apriori(mba_cara_trans, 
                          parameter = list(supp = 0.06, conf = 0.75,
                                           target = "rules", minlen = 2))

summary(mba_cara_rules)

#visualize rules
#plot(mba_cara_rules)
#plotly_arules(mba_cara_rules)

#all partners by transaction amount
# cara_data_temp <- cara_data %>%
#   group_by(ex_transactiondescription) %>%
#   summarize(
#     total_transaction = sum(TransAmount)
#   ) %>%
#   arrange(desc(total_transaction))
# View(cara_data_temp)

#===============
#high value
#===============
rfm_everyone <- point_data %>%
  select(Unique_member_identifier, points, pointdt, pointid, TransAmount)

#convert date
rfm_everyone <- rfm_everyone %>%
  mutate(pointdt = as.Date(pointdt,"%Y-%m-%d"))

library(tidyverse)
#create rfm model
rfm_temp <- rfm_everyone %>%
  group_by(Unique_member_identifier) %>%
  summarise(recency=-(as.numeric(as.Date(today())-max(pointdt))),
            frequency=n_distinct(pointid), monetary= sum(TransAmount)) %>%
  drop_na()

summary(rfm_temp)

#plot rfm model
p <- ggplot(rfm_temp, aes(monetary, frequency))
p + geom_point()

p <- ggplot(rfm_temp, aes(monetary, recency))
p + geom_point()

#rfm rank scoring
# sort data set for Recency with Recency (ascending) - Frequency (descending) - Monetary (descending)
rfm_temp <- rfm_temp %>%
  arrange(recency, desc(frequency), desc(monetary))
rfm_temp$rankR <- cut(rfm_temp$recency,5,labels=F)
rfm_temp <- rfm_temp %>%
  mutate(recency = 0-recency)
# sort data set for Frequency with Recency (descending) - Frequency (ascending) - Monetary (descending)
rfm_temp <- rfm_temp %>%
  arrange(desc(recency), frequency, desc(monetary))
rfm_temp$rankF <- cut(rfm_temp$frequency,5,labels=F) 
# sort data set for Monetary with Recency (descending) - Frequency (descending) - Monetary (ascending)
rfm_temp <- rfm_temp %>%
  arrange(desc(recency), desc(frequency), monetary)
rfm_temp$rankM <- cut(rfm_temp$monetary,5,labels=F)
rfm_temp$Score <- with(rfm_temp, paste0(rankR, rankF, rankM))
rfm_temp <- rfm_temp %>%
  mutate(Score = as.integer(Score))

#use the highest two ranks
rfm_temp_ <- rfm_temp %>%
  arrange(desc(rankR), desc(rankF), desc(rankM)) %>%
  filter(Score %in% c(444,555))

high_value_data <- rfm_temp

#combine with point data
high_value_data <- high_value_data %>%
  inner_join(point_data, "Unique_member_identifier")

high_value_data <- high_value_data %>%
  mutate(PartnerName = as.character(PartnerName))

high_value_data1 <- high_value_data %>%
  select(Unique_member_identifier, ex_transactiondescription)
mba_high_value <- as.data.frame(high_value_data1)

#prep data for apriori algorithm
mba_high_value_trans <- as(split(mba_high_value[,"ex_transactiondescription"], 
                                 unique(mba_high_value[,"Unique_member_identifier"])), "transactions")

#rules
mba_high_value_rules <- apriori(mba_high_value_trans, 
                                parameter = list(supp = 0.06, conf = 0.75, 
                                                 target = "rules", minlen = 2))
summary(mba_high_value_rules)

#inspect top rules by confidence
inspect(sort(mba_high_value_rules, by = "confidence")[1:25])

#inspect top rules by lift
inspect(sort(mba_high_value_rules, by = "lift")[1:25])

#==============================
#exploratory analysis
#==============================
#customer age ranges
summary(SceneAnalytics.dbo.SP_CustomerDetail$age_class)
ggplot(data = SceneAnalytics.dbo.SP_CustomerDetail, mapping = aes(x=as.factor(age_class)))+
  geom_bar() + 
  #18-24 and 25-34 and 35-44 account for a large percentage of the market
  
  #province
  summary(SceneAnalytics.dbo.SP_CustomerDetail$StateProv)
ggplot(data = SceneAnalytics.dbo.SP_CustomerDetail, mapping = aes(x=as.factor(StateProv)))+
  geom_bar() + 
  #ontario is the most important market
  
#summarize city information
#remove multibyte string 
SceneAnalytics.dbo.SP_CustomerDetail$City <- stringr::str_replace_all(SceneAnalytics.dbo.SP_CustomerDetail$City,
                                                                        "s/[\x80-\xFF]//g","") 

cityInfo <- SceneAnalytics.dbo.SP_CustomerDetail %>%
  mutate(City = toupper(City))
cityInfo1 <- cityInfo %>%
  group_by(City) %>%
  summarise(count= n(), percentage = n() / nrow(SceneAnalytics.dbo.SP_CustomerDetail)) %>%
  arrange(desc(count))
#Toronto, Calgary, Edmonton, Vancouver are very important

#-type of points being used?
temp_points <- point_data %>%
  left_join(SceneAnalytics.dbo.SP_PointsType, "pointtypeid")
temp_points1 <- temp_points %>%
  group_by(descr) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

#-top 3 partners
temp_points_partners <- point_data %>%
  group_by(PartnerName) %>%
  summarise(count = n()) %>%
  top_n(3)

#average transaction, total transaction, #no of points used
temp_summary_trans <- point_data %>%
  summarise(
    total_points = sum(points),avg_points = mean(points), 
    total_transaction = sum(TransAmount), avg_transaction  = mean(TransAmount)
  )






