
rm(list = ls())
#packages 
packages <- c('ggplot2', 'corrplot','tidyverse','lubridate',
              'caret','dummies','mlbench','tidyr','Matrix',
              'data.table','vtreat', 'rsample','scales')
#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#load data
df <- read.csv("cohort.csv")

glimpse(df)

unique(df$Status)

#Tweak sign up date to date
df$Sign_up <- as.Date(df$Sign_up, format = "%m/%d/%Y")

#remove NA
df <- na.omit(df)

#get year
df$year <- as.numeric(format(df$Sign_up, '%Y'))

#filter for only 2019
cohorts2019 <- df[df$Year==2019,]

join.date <- aggregate(Sign_up~User_id,cohorts2019,min, na.rm = TRUE)

colnames(join.date)[2] <- "Join_Date"

# Merge the Join date data to the cohort2011 data frame
cohorts2019 <- merge(cohorts2019, join.date, by.x = "User_id",by.y = "User_id", all.x = TRUE)

cohorts2019$Cohort <- as.numeric(format(cohorts2019$Join_Date, "%m"))

rm(join.date)

cohorts2019$Age_by_Day <- as.numeric(difftime(cohorts2019$InvoiceDate,cohorts2019$Join_Date,units = c("days")))

# Dividing the days by 30 to get the number of months
cohorts2019$Age_by_Month <- floor(cohorts2019$Age_by_Day/30)

# Dumping the day element from the join date column
cohorts2019$Join_Date <- format(cohorts2019$Join_Date, "%Y-%m")

# Now we remove the day element from the InvoiceDate data since
# this Cohort Analysis is based on monthly activity.
cohorts2019$Sign_up <- format(cohorts2019$InvoiceDate, "%Y-%m")
# Its important that we remove the day data so that we can remove
# extra observations in the months where customers have multiple
# observations in a single month due to having multiple orders in
# a single month. We'll use the function duplicated for this later


# We relabel the cohort column data to something more intuitive for the sake
# of the report consumers, then factor them since these are sequential
groups <- c("Jan Cohorts",
            "Feb Cohorts",
            "Mar Cohorts",
            "Apr Cohorts",
            "May Cohorts",
            "Jun Cohorts",
            "Jul Cohorts",
            "Aug Cohorts",
            "Sep Cohorts",
            "Oct Cohorts",
            "Nov Cohorts",
            "Dec Cohorts")

for(i in 1:12){
  cohorts2011[cohorts2019$Cohort==i,"Cohort"] <- groups[i]
}
rm(i,groups)

cohorts2019$Cohort <- factor(cohorts2019$Cohort,ordered = T,levels =c("Jan Cohorts",
                                                                      "Feb Cohorts",
                                                                      "Mar Cohorts",
                                                                      "Apr Cohorts",
                                                                      "May Cohorts",
                                                                      "Jun Cohorts",
                                                                      "Jul Cohorts",
                                                                      "Aug Cohorts",
                                                                      "Sep Cohorts",
                                                                      "Oct Cohorts",
                                                                      "Nov Cohorts",
                                                                      "Dec Cohorts"))

dupes <- which(duplicated(cohorts2019[,c(-5,-6)]))

# Removing the duplicate observations
cohorts2011 <- cohorts2019[-dupes,]

# Dropping to the dupes vector
# for memory efficiency
rm(dupes)


# Creating rows for each cohort group
# Creating columns for each value in the Age_by_Month column;0-11
# The default aggregation setup for dcast is, fun.aggregate = length
cohorts.wide <- reshape2::dcast(cohorts2019,Cohort~Age_by_Month,
                                value.var="User_id",
                                fun.aggregate = length)