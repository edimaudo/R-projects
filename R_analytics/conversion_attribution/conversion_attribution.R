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

conversion_attribution_df$year = as.numeric(format(conversion_attribution_df$Conv_Date , "%Y"))
conversion_attribution_df$month = (format(conversion_attribution_df$Conv_Date , "%m"))
conversion_attribution_df$year_month = as.factor(format(conversion_attribution_df$Conv_Date , "%Y-%m"))

#revenue by year

#revenue by year month

#revenue by year and channel

#revenue by year month and channel

#channels by users


#RFM analysis


#LTV analysis


#cohort analysis

#customer segmentation

#rfm analysis

#cohort analysis
