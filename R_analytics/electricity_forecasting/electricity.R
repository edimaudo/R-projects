# Electricity forecast 
rm(list = ls()) # clear environment
#=============
# Packages
#=============
packages <- c('ggplot2', 'corrplot','tidyverse','readxl','feasts','tsibble',
              'scales','dplyr','mlbench','caTools','reshape2','forcats',
              'forecast','TTR','xts','lubridate')

for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}


#=============
# Data overview
#=============
# Load Data
df <- read.csv("DEhourlyload.csv") 

# Data Summary
summary(df)

# Check for missing variables
missing_data <- apply(df, 2, function(x) any(is.na(x)))
print(missing_data) # No missing data

# Date fields
df$Time <- lubridate::dmy_hms(df$Time)
df$Year <- lubridate::year(df$Time)
df$Day <- weekdays(as.Date(df$Time))
df$Hour <- lubridate::hour(df$Time)
df$Month <- lubridate::month(df$Time, label = TRUE)
df$Date <- as.Date(df$Time)

#=============
# Generate scatter plots 2019-2020 data
#=============

# All data
filtered_df <- df %>%
  filter(Year %in% c(2019,2020)) %>%
  group_by(Time) %>%
  select(Time, Load, Price)

ggplot(filtered_df, aes(Price,Load)) + 
  geom_point(size = 0.5, color="#bc5090") +
  theme_minimal() + scale_y_continuous(labels = comma) +
  labs(x = "Price", y = "Load", title="Load and Price Plot") + 
  theme(legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 0, hjust = 1))

# All hours on Saturday
filtered_day_df <- df %>%
  filter(Year %in% c(2019,2020),Day =='Saturday') %>%
  select(Time, Load, Price)

ggplot(filtered_day_df, aes(Price,Load)) + 
  geom_point(size = 0.5, color="#bc5090") +
  theme_minimal() + scale_y_continuous(labels = comma) +
  labs(x = "Price", y = "Load", title="Load and Price Plot on Saturdays") + 
  theme(legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 0, hjust = 1))

# Hour 10 on all days of the week
filtered_hour_df <- df %>%
  filter(Year %in% c(2019,2020),Hour == 10) %>%
  select(Time, Load, Price)

ggplot(filtered_hour_df, aes(Price,Load)) + 
  geom_point(size = 0.5, color="#bc5090") +
  theme_minimal() + scale_y_continuous(labels = comma) +
  labs(x = "Price", y = "Load", title="Load and Price Plot on Hour 10") + 
  theme(legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 0, hjust = 1))



#=============
# Daily and weekly  seasonal plots 2019-2020 data
#=============
# Week
weekly_data <- df %>%
  filter(Year %in% c(2019,2020)) %>%
  group_by(Day) %>%
  summarize(Load = mean(Load), Price=mean(Price)) %>%
  arrange(Price,Load) %>%
  mutate(Day= factor(Day, levels = c("Monday","Tuesday","Wednesday","Thursday",
                          "Friday","Saturday","Sunday"))) %>%
  select(Day,Price,Load)

# Weekly load
ggplot(weekly_data, aes(Day,Load,group=1)) + 
  geom_point(size = 0.5, color="#bc5090") + geom_line() + 
  theme_minimal() + scale_y_continuous(labels = comma) +
  labs(x = "Day", y = "Avg. Load", title="Load Amount") + 
  theme(legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 0, hjust = 1))

# Weekly Price
ggplot(weekly_data, aes(Day,Price,group=1)) + 
  geom_point(size = 0.5, color="#bc5090") + geom_line() + 
  theme_minimal() + scale_y_continuous(labels = comma) +
  labs(x = "Time", y = "Avg. Price", title="Price Amount") + 
  theme(legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 0, hjust = 1))

# Daily
daily_data <- df %>%
  filter(Year %in% c(2019,2020)) %>%
  group_by(Hour) %>%
  summarize(Load = mean(Load), Price=mean(Price)) %>%
  select(Hour,Price,Load)

# Daily Load
ggplot(daily_data, aes(Hour,Load,group=1)) + 
  geom_point(size = 0.5, color="#bc5090") + geom_line(size = 0.5, color="#bc5090") + 
  theme_minimal() + scale_y_continuous(labels = comma) +
  labs(x = "Time", y = "Avg. Load", title="Load Amount") + 
  theme(legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 0, hjust = 1))

# Daily Price
ggplot(daily_data, aes(Hour,Price,group=1)) + 
  geom_point(size = 0.5, color="#bc5090") + geom_line(size = 0.5, color="#bc5090") + 
  theme_minimal() + scale_y_continuous(labels = comma) +
  labs(x = "Time", y = "Avg. Price", title="Price Amount") + 
  theme(legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 0, hjust = 1))

#=============
# Naive models
#=============

#-----------------------
# 2021 data Naive #1 model forecast 
#-----------------------

#overall
naive_df <- df %>%
  filter(Year %in% c(2021)) %>%
  select(Time, Price)
naive.xts <- xts(x = naive_df$Price, order.by = naive_df$Time) 
naive.data <- apply.daily(naive.xts,mean)
naive.end <- floor(length(naive.data)-7)
naive.train <- naive.data[1:naive.end,] 
naive.start <- c(year (start(naive.train)), month(start(naive.train)),
                 day(start(naive.train)))
naive.end <- c(year(end(naive.train)), month(end(naive.train)), 
               day(end(naive.train)))
naive.train <- ts(naive.train, start = naive.start, 
                  end = naive.end,frequency = 12)
fit <- forecast::naive(naive.train,h=1,level = c(80, 95))
fit
fit %>%
  autoplot()

accuracy_data <- accuracy(fit)
naive_1_mae <- accuracy_data[,3]
naive_1_rmse <- accuracy_data[,2]

# hour

#-----------------------
# 2021 data Naive #2 model forecast
#-----------------------



#=============
# AR Models
#=============



#=============
# MLP
#=============