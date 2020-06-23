#============================
#Generates visualizations
#Does RFM
#Does Cohort analysis
#============================
rm(list = ls())

#packages 
packages <- c('ggplot2', 'corrplot','tidyverse','scales',"DT","ggfortify",'rfm',
              'lubridate')
#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#load data
conversion <- read_csv("conversion.csv")
attribution <- read_csv("attribution.csv")

#combine data
conversion_attribution_df <- conversion %>%
  inner_join(attribution,"Conv_ID")

#update date information
conversion_attribution_df$year = as.numeric(format(conversion_attribution_df$Conv_Date , "%Y"))
conversion_attribution_df$month = (format(conversion_attribution_df$Conv_Date , "%m"))
conversion_attribution_df$year_month = as.factor(format(conversion_attribution_df$Conv_Date , "%Y-%m"))

#revenue by year
revenue_year <- conversion_attribution_df %>%
  group_by(year) %>%
  summarise(total_revenue = sum(Revenue)) %>%
  select(year,total_revenue)

ggplot(data=revenue_year , aes(x=as.factor(year), y=total_revenue)) +
  geom_bar(stat="identity", width = 0.4,fill="steelblue") + theme_classic() +
  labs(x = "Year", y = "Total revenue ($)") +
  scale_y_continuous(labels = comma) +
  scale_x_discrete() +
  theme(legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 10))

#revenue by year month
revenue_year_month <- conversion_attribution_df %>%
  group_by(year_month) %>%
  summarise(total_revenue = sum(Revenue)) %>%
  select(year_month,total_revenue)

ggplot(data=revenue_year_month , aes(x=as.factor(year_month), y=total_revenue)) +
  geom_bar(stat="identity", width = 0.4,fill="steelblue") + theme_classic() +
  labs(x = "Year-month", y = "Total revenue ($)") +
  scale_y_continuous(labels = comma) +
  scale_x_discrete() +
  theme(legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1))

#revenue by year and channel
revenue_year_channel <- conversion_attribution_df %>%
  group_by(year,Channel) %>%
  summarise(total_revenue = sum(Revenue)) %>%
  select(year,Channel,total_revenue)


ggplot(data=revenue_year_channel, aes(x=as.factor(year), y=total_revenue, fill=Channel)) +
  geom_bar(stat="identity",width = 0.4) + theme_classic() + 
  labs(x = "Year", y = "Total revenue ($)") +
  scale_y_continuous(labels = comma) +
  scale_x_discrete() +
  theme(legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 10)) +
  labs(fill = "Channel")

#revenue by year month and channel
revenue_year_month_channel <- conversion_attribution_df %>%
  group_by(year_month,Channel) %>%
  summarise(total_revenue = sum(Revenue)) %>%
  select(year_month,Channel,total_revenue)

ggplot(data=revenue_year_month_channel, 
       aes(x=as.factor(year_month), y=total_revenue, fill=Channel)) +
  geom_bar(stat="identity",width = 0.4) + theme_classic() + 
  labs(x = "Year-month", y = "Total revenue ($)") +
  scale_y_continuous(labels = comma) +
  scale_x_discrete() +
  theme(legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(fill = "Channel")

ggplot(data=revenue_year_channel, aes(x=as.factor(Channel), y=total_revenue, fill=as.factor(year))) +
  geom_bar(stat="identity",width = 0.4) + theme_classic() + 
  labs(x = "Channel", y = "Total revenue ($)") +
  scale_y_continuous(labels = comma) +
  scale_x_discrete() +
  theme(legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 10)) +
  labs(fill = "Year")

#Revenues by Channel
revenue_channel <- conversion_attribution_df %>%
  group_by(Channel) %>%
  summarise(total_revenue = sum(Revenue)) %>%
  select(Channel,total_revenue)

ggplot(data=revenue_channel , aes(x=as.factor(Channel), y=total_revenue)) +
  geom_bar(stat="identity", width = 0.4,fill="steelblue") + theme_classic() +
  labs(x = "Channel", y = "Total revenue ($)") +
  scale_y_continuous(labels = comma) +
  scale_x_discrete() +
  theme(legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 10))



#channels by users
channel_user <- conversion_attribution_df %>%
  group_by(Channel,User_ID,year) %>%
  summarise(user_count = n()) %>%
  select(User_ID, Channel,user_count, year)

ggplot(data=channel_user , aes(x=as.factor(Channel), y=user_count, fill=as.factor(year))) +
  geom_bar(stat="identity", width = 0.4,position=position_dodge(),) + theme_classic() +
  labs(x = "Channel", y = "User count") +
  scale_y_continuous(labels = comma) +
  scale_x_discrete() +
  theme(legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 10)) +
  labs(fill = "Year")

#RFM analysis
rfm_data <- na.omit(conversion_attribution_df)
rfm_data_orders <- rfm_data %>%
  mutate("customer_id" = User_ID, "order_date" = Conv_Date, "revenue" = Revenue) %>%
  select(customer_id, order_date, revenue) %>%
  distinct()

analysis_date <- lubridate::as_date("2019-01-01")
rfm_result <- rfm_table_order(rfm_data_orders, customer_id, order_date, revenue, analysis_date)

rfm_heatmap(rfm_result)

rfm_segment(rfm_result)

#cohort analysis
cohort_data <- na.omit(conversion_attribution_df)
cohort_data <- cohort_data %>%
  select(User_ID,Conv_Date,year) %>%
  filter(year==2017) %>%
  distinct()

#cohort creation
join.date <- aggregate(Conv_Date~User_ID,cohort_data,min, na.rm = TRUE)
colnames(join.date)[2] <- "Join_Date"
cohort_data <- merge(cohort_data, join.date, by.x = "User_ID",by.y = "User_ID", all.x = TRUE)
cohort_data$Cohort <- as.numeric(format(cohort_data$Join_Date, "%m"))
rm(join.date)

#cohort age
cohort_data$Age_by_Day <- as.numeric(difftime(cohort_data$Conv_Date,cohort_data$Join_Date,units = c("days")))
cohort_data$Age_by_Month <- floor(cohort_data$Age_by_Day/30)
# Dumping the day element from the join date column
cohort_data$Join_Date <- format(cohort_data$Join_Date, "%Y-%m")
# this Cohort Analysis is based on monthly activity.
cohort_data$Conv_Date <- format(cohort_data$Conv_Date, "%Y-%m")

groups <- c("Jan Cohorts","Feb Cohorts","Mar Cohorts","Apr Cohorts",
            "May Cohorts","Jun Cohorts","Jul Cohorts","Aug Cohorts",
            "Sep Cohorts","Oct Cohorts","Nov Cohorts","Dec Cohorts")

for(i in 1:12){
  cohort_data[cohort_data$Cohort==i,"Cohort"] <- groups[i]
}
rm(i,groups)

cohort_data$Cohort <- factor(cohort_data$Cohort,ordered = T,levels =c("Jan Cohorts",
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


dupes <- which(duplicated(cohort_data[,c(-5,-6)]))

# Removing the duplicate observations
cohorts2011 <- cohort_data[-dupes,]
rm(dupes)


# Creating rows for each cohort group
# Creating columns for each value in the Age_by_Month column;0-11
# The default aggregation setup for dcast is, fun.aggregate = length
cohorts.wide <- reshape2::dcast(cohort_data,Cohort~Age_by_Month,
                                value.var="User_ID",
                                fun.aggregate = length)

cw.retention <- cohorts.wide
cw.churn <- cohorts.wide

# Creating 19 breaks and 20 rgb color values ranging from blue to white
breaks <- quantile(cohorts.wide[,3:13], probs = seq(.05, .95, .05), na.rm = TRUE)
colors <- sapply(round(seq(155, 80, length.out = length(breaks) + 1), 0),
                 function(x){ rgb(x,x,155, maxColorValue = 155) } )


# The Retention Mixpanel with counts
DT::datatable(cohorts.wide,
              class = 'cell-border stripe',
              rownames = FALSE,
              options = list(
                ordering=F,
                dom = 't',
                pageLength = 12) ) %>%
  formatStyle("0",
              backgroundColor = 'lightgrey',
              fontWeight = 'bold') %>%
  formatStyle(names(cohorts.wide[c(-1,-2)]),fontWeight = 'bold',
              color = 'white', backgroundColor = styleInterval(breaks,colors))