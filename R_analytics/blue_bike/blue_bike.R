#remove old data
rm(list=ls())

#load libraries
for (package in c('data.table', 'tidyverse','leaflet','leaflet.extras','geosphere',
                  'lubridate','gridExtra','grid','pysch')) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#load the data set
data <- as.data.frame(fread(file.choose()))
#load station data
station <- read.csv(file.choose())

#visualize stations
map1 <- station %>% 
  leaflet() %>%
  setView(lng = -71.0589, lat = 42.3601, zoom = 13) %>% 
  addTiles() %>%
  addMarkers(lng = station$Longitude, lat = station$Latitude, popup = station$Station)
map1

glimpse(data)


#convert second to minutes and round it non decimal points
data$minutes <- round(data$tripduration/60, 0)

#calculate the age from birth year
data$age <- 2018 - data$`birth year`

#recoding gender variables
data$gender <- as.factor(recode(data$gender, "0" = "Female", "1" = "Male", "2" = "Prefer not to say"))

#convert the time and date variable to correct format
data$starttime <- ymd_hms(data$starttime)
data$stoptime <- ymd_hms(data$stoptime)

#separate date and time
data <- data %>%
  mutate_at(vars(starttime), ymd_hms) %>%
  mutate_at(vars(starttime), funs("start_date" = date(.)))

data <- data %>%
  mutate_at(vars(stoptime), ymd_hms) %>%
  mutate_at(vars(stoptime), funs("stop_date" = date(.)))

#extracting day, weekday and hour
data$day <- day(data$starttime)
data$weekday <- wday(data$starttime, label = TRUE)
data$hour <- hour(data$starttime)

# writing function for the distance formula
dist <- function(long1, lat1, long2, lat2) {
  R <- 6371
  radian <-  pi / 180
  a1 <- lat1 * radian
  a2 <- long1 * radian
  b1 <- lat2 * radian
  b2 <- long2 * radian
  diff_long <- b2 - a2
  diff_lat <- b1 - a1
  a <- (sin(diff_lat/2))^2 + cos(a1) * cos(b1) * (sin(diff_long/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  d <- R * c
  return(d)
}

# return the calculation into 2 decimals and data frame
a <- as.data.frame(round(dist(data$`start station longitude`, data$`start station latitude`, data$`end station longitude`, data$`end station latitude`),2))

#binding two data frame
new_data <- cbind(data, a)

#select relevant variable
bike_data <- select(new_data, "tripduration", "start_date", "stop_date", "start station latitude", "start station longitude", "end station latitude", "end station longitude", "gender", "minutes", "age", "round(dist(data$`start station longitude`, data$`start station latitude`, data$`end station longitude`, data$`end station latitude`), 2)", "day", "weekday", "hour")

#rename variabe
colnames(bike_data) <- c("tripduration", "start_date", "stop_date", "start_lat", "start_long", "end_lat", "end_long", "gender", "minutes", "age", "distances", "day", "weekday", "hour")

# convert into factor
bike_data$weekday <- as.factor(bike_data$weekday)

glimpse(bike_data)

group <- bike_data %>%
  group_by(start_date) %>%
  summarise(total = n())

group %>% ggplot(aes(x = start_date, y = total)) +
  geom_line(lty = 1) +
  geom_point() +
  labs(title = "Total of Blue Bikes Usage in July, 2018",
       x = "Date",  y = "Total of Blue Bikes Used") +
  theme_bw() +
  annotate(geom="text", x = as.Date("2018-07-03"), y = 5000, label = "
           mean = 7833.87\n 
           sd = 1508.95\n
           min = 3466\n
           max = 9860",
           color = "red",
           size = 3.5)

days <- bike_data %>%
  group_by(weekday, start_date) %>%
  summarise(total = n()) %>%
  filter(start_date < as_date("2018-07-08"))

day2 <- bike_data %>%
  group_by(weekday, start_date) %>%
  summarise(n = n()) %>%
  filter(start_date < as_date("2018-07-15") & start_date > as_date("2018-07-07"))

day3 <- bike_data %>%
  group_by(weekday, start_date) %>%
  summarise(n1 = n()) %>%
  filter(start_date < as_date("2018-07-22") & start_date > as_date("2018-07-14"))

day4 <- bike_data %>%
  group_by(weekday, start_date) %>%
  summarise(n2 = n()) %>%
  filter(start_date < as_date("2018-07-29") & start_date > as_date("2018-07-21"))


p1 <- ggplot(days, aes(x = weekday, y = total)) +
  geom_col(fill = "red") +
  geom_label(aes(label = total), color = "black", hjust = 1) +
  coord_flip() +
  labs(title = "From 1st July to 7th July",
       x = "Weekday", 
       y = "Total") +
  theme_bw()

p2 <- ggplot(day2, aes(x = weekday, y = n)) +
  geom_col(fill = "red") +
  geom_label(aes(label = n), color = "black", hjust = 1) +
  coord_flip() +
  labs(title = "From 8th July to 14th July",
       x = "Weekday", 
       y = "Total") +
  theme_bw()

p3 <- ggplot(day3, aes(x = weekday, y = n1)) +
  geom_col(fill = "red") +
  geom_label(aes(label = n1), color = "black", hjust = 1) +
  coord_flip() +
  labs(title = "From 15th July to 21st July",
       x = "Weekday", 
       y = "Total") +
  theme_bw()

p4 <- ggplot(day4, aes(x = weekday, y = n2)) +
  geom_col(fill = "red") +
  geom_label(aes(label = n2), color = "black", hjust = 1) +
  coord_flip() +
  labs(title = "From 22nd July to 28th July",
       x = "Weekday", 
       y = "Total") +
  theme_bw()


grid.arrange(p1,p2,p3,p4, top = textGrob("Total of Blue Bikes Usage in Four Different Weeks",gp = gpar(fontsize = 15, font = 3)))


Sun <- bike_data %>%
  group_by(hour, start_date) %>%
  summarise(total = n()) %>%
  filter(start_date < as_date("2018-07-02"))

Mon <- bike_data %>%
  group_by(hour, start_date) %>%
  summarise(total = n()) %>%
  filter(start_date < as_date("2018-07-03") & start_date > as_date("2018-07-01"))

Tue <- bike_data %>%
  group_by(hour, start_date) %>%
  summarise(total = n()) %>%
  filter(start_date < as_date("2018-07-04") & start_date > as_date("2018-07-02"))

Wed <- bike_data %>%
  group_by(hour, start_date) %>%
  summarise(total = n()) %>%
  filter(start_date < as_date("2018-07-05") & start_date > as_date("2018-07-03"))

Thu <- bike_data %>%
  group_by(hour, start_date) %>%
  summarise(total = n()) %>%
  filter(start_date < as_date("2018-07-06") & start_date > as_date("2018-07-04"))

Fri <- bike_data %>%
  group_by(hour, start_date) %>%
  summarise(total = n()) %>%
  filter(start_date < as_date("2018-07-07") & start_date > as_date("2018-07-05"))

Sat <- bike_data %>%
  group_by(hour, start_date) %>%
  summarise(total = n()) %>%
  filter(start_date < as_date("2018-07-08") & start_date > as_date("2018-07-06"))

m1 <- Sun %>% ggplot(aes(x = hour, y = total)) +
  geom_line(lty = 2) +
  geom_point() +
  labs(title = "Sunday", x = "Hour",  y = "Total") +
  theme_bw()

m2 <- Mon %>% ggplot(aes(x = hour, y = total)) +
  geom_line(lty = 2) +
  geom_point() +
  labs(title = "Monday", x = "Hour",  y = "Total") +
  theme_bw()

m3 <- Tue %>% ggplot(aes(x = hour, y = total)) +
  geom_line(lty = 2) +
  geom_point() +
  labs(title = "Tuesday", x = "Hour",  y = "Total") +
  theme_bw()

m4 <- Wed %>% ggplot(aes(x = hour, y = total)) +
  geom_line(lty = 2) +
  geom_point() +
  labs(title = "Wednesday", x = "Hour",  y = "Total") +
  theme_bw()

m5 <- Thu %>% ggplot(aes(x = hour, y = total)) +
  geom_line(lty = 2) +
  geom_point() +
  labs(title = "Thursday", x = "Hour",  y = "Total") +
  theme_bw()

m6 <- Fri %>% ggplot(aes(x = hour, y = total)) +
  geom_line(lty = 2) +
  geom_point() +
  labs(title = "Friday", x = "Hour",  y = "Total") +
  theme_bw()

m7 <- Sat %>% ggplot(aes(x = hour, y = total)) +
  geom_line(lty = 2) +
  geom_point() +
  labs(title = "Saturday", x = "Hour",  y = "Total") +
  theme_bw()

grid.arrange(m1,m2,m3,m4,m5,m6,m7, top = textGrob("Total of Blue Bikes Usage in Hour for One Week",gp = gpar(fontsize = 15,font = 3)))


gender <- bike_data %>%
  group_by(gender) %>%
  summarise(total = n()) %>%
  filter(gender != "Prefer not to say")

gender %>% ggplot(aes(x = gender, y = total, fill = gender)) +
  geom_col() +
  geom_text(aes(label = total), vjust = -0.1) +
  labs(main = "Gender on Blue Bikes Usage",
       x = "Gender", 
       y = "Total") + 
  guides(fill = FALSE) +
  theme_bw()

ggplot(data = bike_data, aes(x = distances)) +
  geom_density(alpha = 0.3, fill = "black") +
  guides(fill = FALSE) +
  labs(title = "Desntiy Plot of Distance", 
       x = "Distance", 
       y = "Density") +
  annotate(geom ="text", x = 6, y = 0.3, label = "
           mean = 1.97\n 
           sd = 1.37\n
           min = 0\n
           max = 11.24",
           color = "red",
           size = 4) +
  theme_bw()

gender_distance <- bike_data %>%
  group_by(gender, distances) %>%
  summarise(total = n())  %>%
  filter(gender != "Prefer not to say")

gender_distance %>% ggplot(aes(x = distances)) +
  geom_density(alpha = 0.3, fill = "black") +
  guides(fill = FALSE) +
  labs(title = "Density Plot of Distance Based on Gender" ,
       x = "Distance",
       y = "Density") +
  facet_wrap(~ gender) 

t.test(gender_distance$distances ~ gender_distance$gender)

gender_distance %>%
  ggplot(aes(x = gender, y = distances, fill = gender)) +
  geom_boxplot() +
  labs(title = "Boxplot of Gender vs Distance",
       x = "Gender", 
       y = "Distances") +
  guides(fill=guide_legend(title = "Gender")) +
  theme_bw()

#starting location
#mapping 
bike_data %>% 
  leaflet() %>%
  setView(lng = -71.0589, lat = 42.3601, zoom = 13) %>% 
  addTiles() %>%
  addHeatmap(lng = bike_data$start_long, lat = bike_data$start_lat, max = 2, radius = 15)

#ending location
#mapping
bike_data %>% 
  leaflet() %>%
  setView(lng = -71.0589, lat = 42.3601, zoom = 13) %>% 
  addTiles() %>%
  addHeatmap(lng = bike_data$end_long, lat = bike_data$end_lat, max = 2, radius = 15)