#remove old data
rm(list=ls())

#packages
packages <- c('ggplot2','corrplot','dplyr','data.table')

#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#load data
notification_final_mo <- fread(file.choose(), sep="#")
notification_final_m <- fread(file.choose(), sep="#")
notification_final_sp <- fread(file.choose(), sep="#")
protocols_final_mo <- fread(file.choose(), sep="#")
protocols_final_m <- fread(file.choose(), sep="#")
protocols_final_sp <- fread(file.choose(), sep="#")

fields_extract_not_zk <- function(df){
  final_output <- df
  final_output <- final_output %>%
    select(notificationnumber,versionnumber,publishdate,customerrequirement_maxprice,
           notificationcommission_p1date,notificationcommission_p2date)
}

notification_final_mo <- fields_extract_not_zk(notification_final_mo)
notification_final_m <- fields_extract_not_zk(notification_final_m)
notification_final_sp <- fields_extract_not_zk(notification_final_sp)

#combine notification data

#sort notification data

#keeps unique observations

#filter data

#create new variable
