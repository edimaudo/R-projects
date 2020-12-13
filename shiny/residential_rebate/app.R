rm(list = ls())

#packages 
packages <- c('ggplot2', 'corrplot','tidyverse','shiny','shinydashboard',
              'dplyr','readxl')
#load packages
for (package in packages) {
    if (!require(package, character.only=T, quietly=T)) {
        install.packages(package)
        library(package, character.only=T)
    }
}


df <- read_excel("FY_Summary.xlsx") #load data
df[is.na(df)] <- 0 #replace na with 0
area <- c(sort(unique(df$Area)))
fiscal_year <- c(sort(unique(df$`Fiscal Year`)))
rebate_type <- c(sort(unique(df$Type)))
colnms=c("July","August","September","October","November",
         "December","January","February","March","April","May","June")

df$month_total <- rowSums(df[,colnms])

#todo
# Introduction
# - create readme & add it to introduction
# Summary
# - build summary table with fiscal year as the dropdown for Summary section
#  - It should have a dataable with the different columns as per the excel sheet and the calculations
# Trend
#- line chart trend for 2012 to 2020 using program type filters
#- District Devices/		Gallons Saved		AF Saved		AF Saved by year
#- funding by fiscal year

#app
ui <- dashboardPage(
    dashboardHeader(title = "Water Service Program"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Introduction", tabName = "Introduction", icon = icon("th")),
            menuItem("Summary", tabName = "Summary", icon = icon("dashboard")),
            menuItem("Trends", tabName = "Trends", icon = icon("dashboard")),
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "Introduction",includeMarkdown("intro.md"),hr()),
            tabItem(tabName = "Summary"),
            tabItem(tabName = "Trends")
        )
    )
    
)

#server info
server <- function(input, output) {
    

}

shinyApp(ui, server)