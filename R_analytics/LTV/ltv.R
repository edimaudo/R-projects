library("readxl")

#remove all data
rm(list=ls())

mydata <- read_excel(file.choose())

str(mydata)

startDate_forcast <- as.Date("20160401","%Y%m%d")
endDate_forcast <- as.Date("20160531","%Y%m%d")

history <- GetDataFrame(mydata,startDate_history,endDate_history)
forcast <- GetDataFrame(mydata,startDate_forcast,endDate_forcast)