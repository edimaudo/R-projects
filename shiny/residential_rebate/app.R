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

#load data
df <- read_excel("FY_Summary.xlsx")
#replace na with 0
df[is.na(df)] <- 0

#city information

#fiscal year

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
    
    #line chart trend for 2012 to 2020 using program type filters
    #District Devices/		Gallons Saved		AF Saved		AF Saved by year
    #funding by fiscal year
}

shinyApp(ui, server)