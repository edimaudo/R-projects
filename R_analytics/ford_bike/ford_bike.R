#remove old data
rm(list = ls())

# List of data file names
file_names = list.files(pattern = ".csv")
file_names

# Read data (this will take a second)
df_list = lapply(file_names,
                 function(x) read.csv(x, stringsAsFactors = FALSE))

# Assign names to data frames in the list
names(df_list) = file_names

# Extract column names from the first data frame
names(df_list[[1]])

#data conversion
# Cast certain columns as numeric to enable row binding
df_list[[6]]$start_station_id = as.numeric(df_list[[6]]$start_station_id)
df_list[[6]]$end_station_id = as.numeric(df_list[[6]]$end_station_id)
df_list[[7]]$start_station_id = as.numeric(df_list[[7]]$start_station_id)
df_list[[7]]$end_station_id = as.numeric(df_list[[7]]$end_station_id)
df_list[[8]]$start_station_id = as.numeric(df_list[[8]]$start_station_id)
df_list[[8]]$end_station_id = as.numeric(df_list[[8]]$end_station_id)

# Bind data frames by rows
library(dplyr)   # for data manipulation
df = bind_rows(df_list)

# Glimpse at df
glimpse(df)

# Format data structure
library(stringr)   # for regular expression

df = df %>%
  # Extract day and month from start_time
  mutate(start_month = sub(" .*", "", start_time)) %>%
  mutate(start_month = as.POSIXct(start_month)) %>%
  mutate(start_day = weekdays(start_month)) %>%
  mutate(start_month = format(start_month, "%B")) %>%
  
  # Extract hour from start_time
  mutate(start_hour = sub("^.{11}", "", start_time)) %>%
  mutate(start_hour = str_extract(start_hour, "^[0-9]{2}")) %>%
  
  # Convert birth year to age
  mutate(member_age = 2018 - member_birth_year) %>%
  
  # Convert duration_sec to duration_min
  mutate(duration_min = duration_sec / 60) %>%
  
  # Convert characters to factors
  mutate_if(is.character, as.factor) %>%
  
  # Remove columns not used in the analysis
  select(-c(start_time, end_time, start_station_id, end_station_id, bike_id, member_birth_year, duration_sec))

# Glimpse at cleaned df
glimpse(df)

# Organize month
df$start_month = factor(df$start_month, 
                        levels = c("January", "February", "March", "April", "May", "June", "July", "August"))

# Count usage by month
month_counts = table(df$start_month)

# Plot bar graph
bar = barplot(month_counts,
              ylim = c(0, 220000),
              xlab = "Months in 2018",
              ylab = "Number of Rides",
              main = "Bike Usage by Month",
              cex.names = 0.8,
              cex.axis = 0.8)
text(x = bar, y = month_counts,   # Add labels
     label = month_counts, pos = 3, cex = 0.8, col = "red") 

# Organize day of the week
df$start_day = factor(df$start_day,
                      levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

# Count usage by day
day_counts = table(df$start_day)

# Plot bar graph
bar = barplot(day_counts,
              ylim = c(0, 230000),
              xlab = "Day of the Week",
              ylab = "Number of Rides",
              main = "Bike Usage by Day of the Week",
              cex.axis = 0.8,
              xaxt = "n")
text(x = bar, y = day_counts,   # Add labels
     label = day_counts, pos = 3, cex = 0.8, col = "red") 
text(x = bar, y = -6000, cex = 0.8,   # Rotate x-axis labels
     labels = names(day_counts), srt = 45, adj = 1, xpd = TRUE)

# Plot bar graph
library(ggplot2)   # for plotting functions
ggplot(df, aes(start_month)) + 
  geom_bar() +
  facet_grid(. ~ user_type) +
  xlab("Months in 2018") + 
  ylab("Number of Rides") +
  ggtitle("Comparison of Usage Between Customers and Subscribers") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))   # Rotate x-axis labels

# Organize data by gender
df_gender = df %>%
  filter(member_gender %in% c("Male", "Female")) %>%   # Isolate "Male" and "Female" genders
  mutate(member_gender = droplevels(member_gender))   # Drop unused levels

# Plot barplot
ggplot(df_gender, aes(start_month, fill = member_gender)) +
  geom_bar(position = "fill") +
  xlab("Months in 2018") + 
  ylab("Proportion of Rides") +
  ggtitle("Bike Usage by Gender over Time") +
  scale_fill_discrete("Gender") +
  theme_minimal()

# Plot histogram
ggplot(df, aes(member_age)) +
  geom_histogram(binwidth = 2, alpha = 0.5) +
  scale_x_continuous(limits = c(10, 100),
                     breaks = seq(10, 100, by = 4)) +
  xlab("Age in Years") + 
  ylab("Number of Rides") +
  ggtitle("Total Rides by Age") +
  theme_minimal()

# Plot histogram
ggplot(df, aes(duration_min)) +
  geom_histogram(binwidth = 2, alpha = 0.5) +
  scale_x_continuous(limits = c(0, 100),
                     breaks = seq(0, 100, by = 2)) +
  xlab("Ride Duration in Minutes") + 
  ylab("Number of Rides") +
  ggtitle("Duration of Rides") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))   # Rotate x-axis labels

# Organize usage by hour
hour = df %>%
  group_by(start_hour) %>%
  summarise(total = n())

# Plot line graph
ggplot(hour, aes(x = start_hour, y = total, group = 1)) + 
  geom_line() +
  geom_point(color = "red") +
  xlab("Time in Hours") + 
  ylab("Number of Rides") +
  ggtitle("Total Rides by Hour") +
  theme_minimal()

