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
    
}

shinyApp(ui, server)