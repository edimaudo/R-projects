rm(list = ls())

#packages 
packages <- c('ggplot2', 'corrplot','tidyverse','scales',"DT","ggfortify")
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
  geom_bar(stat="identity", width = 0.4) + theme_classic() +
  labs(x = "Channel", y = "User count") +
  scale_y_continuous(labels = comma) +
  scale_x_discrete() +
  theme(legend.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 10)) +
  labs(fill = "Year")

#RFM analysis

#cohort analysis


