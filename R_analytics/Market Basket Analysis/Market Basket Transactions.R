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

#load libraries
library(arules)
library(arulesViz)
library(tidyverse)
library(lubridate)
library(stringr)
library(data.table)
library(ggplot2)
library(Matrix)


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
# - How would you define a high value user
# - Does app user focus only on IOS and android? 
# - What type of insights are you looking for in the different segments?
# - How much detail would you like to see?
# - Would you look to see a proof of concept with respect to a BI tool or tool like 
# - R that showcases the information?
# - What is the end use of the tools e.g. marketing campaign?

#Q&A with prof
# - how to properly collect the data
# - validate the market basket analysis
# - type of visuals are good for this type of analysis
#   #only collect once since it is memory heavy
#   #can still use arules for the visualization
#   tweak connection setting increase size to 50G + 2G + experiment
#   possible option is to get multiple data points + them combine them on your 
#local machine and then run the arules and arules viz
# Also consider using the other columns in point example the ex_transactiondescription


#recommendation using channels 
#- apps, emails and website


#build rfm model based on black card use
#do a mix of those in the top of recency, frequency and monetary
#do those individuals then combine to find those in the different deciles

#===============
#rfm analysis
#===============
#onl
rfm_everyone <- SceneAnalytics.dbo.SP_Points %>%
  select(Unique_member_identifier, points, pointdt, pointid, TransAmount)

#drop na in rfm_everyone
rfm_everyone <- rfm_everyone %>%
  drop_na()

#convert date
rfm_everyone <- rfm_everyone %>%
  mutate(pointdt = as.Date(pointdt,"%Y-%m-%d"))

rfm_temp <- rfm_everyone %>% 
  group_by(Unique_member_identifier) %>% 
  summarise(recency=as.numeric(as.Date("2018-02-01")-max(pointdt)),
            frequency=n_distinct(pointid), monetary= sum(TransAmount)) 

#rfm for high value

summary(rfm_temp) #get more info from professor about high value

glimpse(rfm_everyone)
#high value 
rfm_everyone <-rfm_everyone %>%
  arrange(desc(monetary))
print(rfm_everyone[1:50,])



#==============================
#market basket analysis
#==============================

#remove inactive customers
#can also look at the merchants perspective and then categorise

#to clean transaction data
Clean_String <- function(extransaction){
  temp <- extransaction
  #remove everything that is not a number or a letter
  temp <- stringr::str_replace_all(temp,"[:$()-*#0123456789\\s]", " ")
  #remove white space on both sides
  temp <- trimws(temp,"b")
  #shrink down to just a single white space
  temp <- stringr::str_replace_all(temp,"[\\s]+", " ")
  #remove multibyte string
  temp <- stringr::str_replace_all(temp,"s/[\x80-\xFF]//g","")
  return (temp)
  
  #' Remove everything that is not a number or letter 
  #all_data$ex_transactiondescription <- stringr::str_replace_all(all_data$ex_transactiondescription,"[-*#0123456789\\s]", " ")
  #all_data$ex_transactiondescription <- stringr::str_replace_all(all_data$ex_transactiondescription,"[^a-zA-Z\\s]", "")
  #remove white space on both sides
  #all_data$ex_transactiondescription <- trimws(all_data$ex_transactiondescription,"b")
  # Shrink down to just one white space
  #all_data$ex_transactiondescription <- stringr::str_replace_all(all_data$ex_transactiondescription,"[\\s]+", " ")
  #remove multibyte
  #all_data$ex_transactiondescription <- stringr::str_replace_all(all_data$ex_transactiondescription,"s/[\x80-\xFF]//g","")
  #' # Lowercase
  #' temp <- tolower(string)
  #' #' Remove everything that is not a number or letter (may want to keep more 
  #' #' stuff in your actual analyses). 
  #' temp <- stringr::str_replace_all(temp,"[^a-zA-Z\\s]", " ")
  #' #fix multibyte string
  #' temp <- stringr::str_replace_all(temp,"s/[\x80-\xFF]//g", " ")
  #' # Shrink down to just one white space
  #' temp <- stringr::str_replace_all(temp,"[\\s]+", "")
  #' # Split it
  #' #temp <- stringr::str_split(temp, " ")[[1]]
  #' # Get rid of trailing "" if necessary
  #' indexes <- which(temp == "")
  #' if(length(indexes) > 0){
  #'   temp <- temp[-indexes]
  #' } 
  #' return(temp)
}

#points data #add ordering at ciniplext info as well
point_data <- SceneAnalytics.dbo.SP_Points %>%
  filter(pointtypeid %in% c('11','1282','1252','1290','1322','1253','1254','12','1283','1323')) %>%
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
  mutate(PartnerName = as.character(PartnerName), 
         ex_transactiondescription = as.character(ex_transactiondescription))

#clean transaction
all_data$ex_transactiondescription <- Clean_String(all_data$ex_transactiondescription)

filter_out = c("STEP_GOAL_COMPLETE"," ",
               "INTERVENTION","CARA Points Earned", 
               "SCENE $ OFF CONCESSIONS",
               "CONCESSIONS POPCORN MAIS ECLATE",
               "Online bonus en ligne",
               "ADULT TICKETS PREMIUM","UBER TRIP LCKE HELP.UBER",
               "SCENE Admit","TWITCH INTERACTIVE, IN","UBER TRIP OVGES HELP.UBERhelp.uber.com",
               "UBER TRIP F O HELP.UBERhelp.uber.com",'CONCESSIONS COMBO','CONCESSIONS POPTOPIA',
               'CONCESSIONS NACHOS','CONCESSIONS COUPONS',
               'CONCESSIONS FOOD ALIMENTS','CONCESSIONS DRINKS BOISSONS',
               'CONCESSIONS CANDY/BONBONS')

#remove irrelevant data
all_data <- all_data %>%
  filter(!ex_transactiondescription %in% filter_out)

unique(all_data$ex_transactiondescription)

write.csv(all_data$ex_transactiondescription,"ex_transactiondescription.csv")

#change to dataframe
mba_all <- as.data.frame(all_data)

#prep data for apriori algorithm
mba_all_trans <- as(split(mba_all[,"ex_transactiondescription"], 
                          unique(mba_all[,"Unique_member_identifier"])), "transactions")

#rules using apriori
mba_all_rules <- apriori(mba_all_trans, 
                         parameter = list(supp = 0.006, conf = 0.25, 
                                          target = "rules", minlen = 2))
summary(mba_all_rules)
inspect(mba_all_rules[1:100])

inspect(sort(mba_all_rules, by = "lift")[1:5])

#take first 100000
write(mba_all_rules[1:100000], file = "mba_all_rules.csv",
      sep = ",", quote = TRUE, row.names = FALSE)

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
  mutate(PartnerName = as.character(PartnerName), 
         ex_transactiondescription = as.character(ex_transactiondescription))

#clean string
# Shrink down to just one white space
youth_data$ex_transactiondescription <- stringr::str_replace_all(youth_data$ex_transactiondescription,"[\\s]+", " ")
#remove multibyte
youth_data$ex_transactiondescription <- stringr::str_replace_all(youth_data$ex_transactiondescription,"s/[\x80-\xFF]//g","")
#remove white space on both sides
youth_data$ex_transactiondescription <- trimws(youth_data$ex_transactiondescription,"b")


mba_youth <- as.data.frame(youth_data)

#prep data for apriori algorithm
mba_youth_trans <- as(split(mba_youth[,"ex_transactiondescription"], 
                            unique(mba_youth[,"Unique_member_identifier"])), "transactions")

#rules
mba_youth_rules <- apriori(mba_youth_trans, 
                           parameter = list(supp = 0.006, conf = 0.25, 
                                            target = "rules", minlen = 2))
summary(mba_youth_rules)
inspect(mba_youth_rules[1:10])

#============================
#app users
#============================

account_hist_type <- SceneAnalytics.dbo.SP_AccountHistoryType %>%
  filter(Descr %in% c('Customer logged into Scene API MobileApp'))

account_hist <- account_hist_type %>% 
  right_join(SceneAnalytics.dbo.SP_AccountHistory, "AccountHistoryTypeID")

account_hist <- account_hist %>%
  mutate(Unique_member_identifier = as.character(Unique_member_identifier))

account_data <- account_hist %>%
  right_join(point_data, "Unique_member_identifier")

#clean string
# Shrink down to just one white space
account_data$ex_transactiondescription <- stringr::str_replace_all(account_data$ex_transactiondescription,"[\\s]+", " ")
#remove multibyte
account_data$ex_transactiondescription <- stringr::str_replace_all(account_data$ex_transactiondescription,"s/[\x80-\xFF]//g","")
#remove white space on both sides
account_data$ex_transactiondescription <- trimws(account_data$ex_transactiondescription,"b")
#Encoding(account_data$ex_transactiondescription) <- "latin1"

#items and transactions
mba_app1 <- as.data.frame(account_data)

#prep data for apriori algorithm
mba_app_trans1 <- as(split(mba_app1[,"ex_transactiondescription"], 
                           unique(mba_app1[,"Unique_member_identifier"])), "transactions")

#rules
mba_app_rules1 <- apriori(mba_app_trans1, 
                          parameter = list(supp = 0.06, conf = 0.25, 
                                           target = "rules", minlen = 2))
summary(mba_app_rules1)
inspect(mba_app_rules1[1:6])

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

#clean string
# Shrink down to just one white space
scentourage_data$ex_transactiondescription <- stringr::str_replace_all(scentourage_data$ex_transactiondescription,"[\\s]+", " ")
#remove multibyte
scentourage_data$ex_transactiondescription <- stringr::str_replace_all(scentourage_data$ex_transactiondescription,"s/[\x80-\xFF]//g","")
#remove white space on both sides
scentourage_data$ex_transactiondescription <- trimws(scentourage_data$ex_transactiondescription,"b")
#Encoding(account_data$ex_transactiondescription) <- "latin1"


#items and transactions
mba_scentourage <- as.data.frame(scentourage_data)

#prep data for apriori algorithm
mba_scentourage_trans <- as(split(mba_scentourage[,"ex_transactiondescription"], 
                                  unique(mba_scentourage[,"Unique_member_identifier"])), "transactions")

#rules
mba_scentourage_rules <- apriori(mba_scentourage_trans, 
                                 parameter = list(supp = 0.06, conf = 0.25, 
                                                  target = "rules", minlen = 2))
summary(mba_scentourage_rules)
inspect(mba_scentourage_rules[1:10])

#============================
#Cara (i.e., those who have shopped in any brand)
#============================
cara_location <- SceneAnalytics.dbo.SP_LocationCARA %>% 
  rename(Ex_sourceid = ex_sourceID)

cara_data <- point_data %>%
  left_join(cara_location, "Ex_sourceid")

cara_data <- cara_data %>%
  mutate(PartnerName = as.character(PartnerName))

#clean string
# Shrink down to just one white space
cara_data$ex_transactiondescription <- stringr::str_replace_all(cara_data$ex_transactiondescription,"[\\s]+", " ")
#remove multibyte
cara_data$ex_transactiondescription <- stringr::str_replace_all(cara_data$ex_transactiondescription,"s/[\x80-\xFF]//g","")
#remove white space on both sides
cara_data$ex_transactiondescription <- trimws(cara_data$ex_transactiondescription,"b")

#change to dataframe
mba_cara <- as.data.frame(cara_data)

#prep data for apriori algorithm
mba_cara_trans <- as(split(mba_cara[,"PartnerName"], 
                           unique(mba_cara[,"Unique_member_identifier"])), "transactions")

#rules
mba_cara_rules <- apriori(mba_cara_trans, 
                          parameter = list(supp = 0.06, conf = 0.25, 
                                           target = "rules", minlen = 2))
summary(mba_cara_rules)
inspect(mba_cara_rules[1:6])

#============================
#high value (not defined but assuming customer points > 5000) - use rfm modelling
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

#clean string
# Shrink down to just one white space
high_value_data$ex_transactiondescription <- stringr::str_replace_all(high_value_data$ex_transactiondescription,"[\\s]+", " ")
#remove multibyte
high_value_data$ex_transactiondescription <- stringr::str_replace_all(high_value_data$ex_transactiondescription,"s/[\x80-\xFF]//g","")
#remove white space on both sides
high_value_data$ex_transactiondescription <- trimws(high_value_data$ex_transactiondescription,"b")


#items and transactions
mba_high_value <- as.data.frame(high_value_data)

#prep data for apriori algorithm
mba_high_value_trans <- as(split(mba_high_value[,"PartnerName"], 
                                 unique(mba_high_value[,"Unique_member_identifier"])), "transactions")

#rules
mba_high_value_rules <- apriori(mba_high_value_trans, 
                                parameter = list(supp = 0.06, conf = 0.25, 
                                                 target = "rules", minlen = 2))
summary(mba_high_value_rules)
inspect(mba_high_value_rules[1:2])


