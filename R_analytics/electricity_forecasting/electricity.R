# Electricity forecast 
rm(list = ls()) # clear environment
#=============
# Packages
#=============
packages <- c('ggplot2', 'corrplot','tidyverse','readxl','feasts','tsibble',
              'scales','dplyr','mlbench','caTools','reshape2',
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
df$Month <- lubridate::month(df$Time)
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
# Seasonal and weekly plots 2019-2020 data
#=============
load_data <- df %>%
  filter(Year %in% c(2019,2020)) %>%
  select(Time, Load, Price,Date) %>%
  as_tsibble(key = c(Time,Load, Price), index=Date)
  
# Weekly Load
load_data %>% feasts::gg_season(Load,period = "week") +
  labs(y="Load ",title = "Weekly Load Amount")

# Daily Load
load_data %>% feasts::gg_season(Load,period = "day") +
  labs(y="Load", title = "Daily Load Amount")

# Weekly Price
load_data %>% feasts::gg_season(Price,period = "week") +
  labs(y="Price ",title = "Weekly Price Amount")

# Daily Price
load_data %>% feasts::gg_season(Price,period = "day") +
  labs(y="Price", title = "Daily Price Amount")

#=============
# Naive models
#=============
filtered_df <- df %>%
  filter(Year %in% c(2021))
#-----------------------
# 2021 data Naive #1 model forecast 
#-----------------------


#-----------------------
# 2021 data Naive #2 model forecast
#-----------------------



#=============
# AR Models
#=============



#=============
# MLP
#=============