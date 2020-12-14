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
df$month_total <- rowSums(df[,colnms]) #aggregate months

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
            tabItem(tabName = "Introduction",includeMarkdown("readme.md"),hr()),
            tabItem(tabName = "Summary"),
            tabItem(tabName = "Trends")
        )
    )
    
)

#server info
server <- function(input, output) {
    

}

shinyApp(ui, server)