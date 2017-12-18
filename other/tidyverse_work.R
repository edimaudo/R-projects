library(nycflights13)
library(tidyverse)

nycflights13::flights

#Filters
#Had an arrival delay of two or more hours
arrivalDelay <- filter(flights, arr_delay >= 2)
#Flew to Houston (IAH or HOU)
flewHouston <- filter(flights, dest == "IOH"| dest=="HOU")
#Were operated by United, American, or Delta
operatedBy <- filter(flights,  carrier %in% c('AA','DL','UA'))
#Departed in summer (July, August, and September)
departedIn <- filter(flights, month %in% c(7,8,9))
#Arrived more than two hours late, but didn’t leave late
arrivednotLeave <- filter(flights, arr_delay > 2 & dep_delay == 0)
#Were delayed by at least an hour, but made up over 30 minutes in flight
delayMadeUp <- filter(flights, dep_delay >=1 & arr_delay < -30)
#Departed between midnight and 6am (inclusive)
depart126 <- filter(flights, dep_time >= 000  & dep_time < 601)

#Arrange
# sort flights to find the most delayed flights. Find the flights that left earliest
mostDelayed <- arrange(flights, aesc(dep_delay))
#find fastest flight
fastestFlight <- arrange(flights, aesc(arr_delay))
#