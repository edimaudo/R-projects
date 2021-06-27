# User content sentiment analysis
rm(list = ls()) #clear environment

# libraries
packages <- c('ggplot2', 'corrplot','tidyverse',"caret","dummies",'readxl','shiny','shinydashboard',
              'scales','dplyr','mlbench','caTools','forecast','TTR','xts',
              'lubridate')
# load packages
for (package in packages) {
    if (!require(package, character.only=T, quietly=T)) {
        install.packages(package)
        library(package, character.only=T)
    }
}

#=============
# load data
#=============
df <- read.csv("reviews_v2.csv")

#=============
# data cleaning
#=============
df[df==0] <- NA #assigne 0 to NA
df <- na.omit(df) #remove na

#=============
#load data
#=============
df$printer <- ifelse(df$appId == "com.hp.printercontrol", 'HP',
                  ifelse(df$appId == "jp.co.canon.bsd.ad.pixmaprint", 'Canon',
                         ifelse(df$appId == "epson.print", 'Epson', 'Epson Smart')))
                         
printer_dropdown <- c('Canon','Epson','Epson Smart','HP')