# Electricity forecast 
rm(list = ls()) # clear environment
#=============
# Packages
#=============
packages <- c('ggplot2', 'corrplot','tidyverse','readxl',
              'scales','dplyr','mlbench','caTools','reshape',
              'forecast','TTR','xts','lubridate')

for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#=============
# Load Data
#=============
df <- read.csv("DEhourlyload.csv")

#=============
# Data overview
#=============

# Data Summary
summary(df)

# Check for missing variables
missing_data <- apply(df, 2, function(x) any(is.na(x)))
print(missing_data) # No missing data

# Time update 
df$Time <- lubridate::dmy_hms(df$Time)
df$Year <- lubridate::year(df$Time)
df$Day <- weekdays(as.Date(df$Time))
df$Hour <- lubridate::hour(df$Time)

#-----------------------
# Generate scatter plots
#-----------------------
# Only 2019-2020 data

# All data
filtered_df <- df %>%
  filter(Year %in% c(2019,2020)) %>%
  select(Time, Load, Price)

# ggplot(filtered_df, aes(Time, y = value, color = variable)) + 
#   geom_point(aes(y =Load, col = "Load")) + 
#   geom_point(aes(y = Price, col = "Price"))

# Load Plot
load_plot <- xts::xts(filtered_df$Load, order.by = filtered_df$Time) 
load_plot %>%
  autoplot() + labs(y="Load",title="2019 - 2020 data")

# Price Plot
price_plot <- xts::xts(filtered_df$Price, order.by = filtered_df$Time) 
price_plot %>%
  autoplot() + labs(y="Price",title="2019 - 2020 data")


# All hours on Saturday
filtered_day_df <- df %>%
  filter(Year %in% c(2019,2020),Day =='Saturday') %>%
  select(Time, Load, Price)

# Load Plot
load_plot <- xts::xts(filtered_day_df$Load, order.by = filtered_day_df$Time) 
load_plot %>%
  autoplot() + labs(y="Load",title="2019 - 2020 data - Saturday")

# Price Plot
price_plot <- xts::xts(filtered_day_df$Price, order.by = filtered_day_df$Time) 
price_plot %>%
  autoplot() + labs(y="Price",title="2019 - 2020 data - Saturday")

# Hour 10 on all days of the week
filtered_hour_df <- df %>%
  filter(Year %in% c(2019,2020),Hour == 10) %>%
  select(Time, Load, Price)

load_plot <- xts::xts(filtered_hour_df$Load, order.by = filtered_hour_df$Time) 
load_plot %>%
  autoplot() + labs(y="Load",title="2019 - 2020 data - Hour 10")

price_plot <- xts::xts(filtered_hour_df$Price, order.by = filtered_hour_df$Time) 
price_plot %>%
  autoplot() + labs(y="Price",title="2019 - 2020 data - Hour 10")
