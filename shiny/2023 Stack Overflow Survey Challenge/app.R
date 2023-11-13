## 2023 StackOverflow Survey Analysis

## Environment setup
rm(list = ls())

## Load Libraries
packages <- c('ggplot2', 'corrplot','tidyverse',"caret",
              'scales','ggfortify','DT',
              'shiny','shinydashboard','lubridate','caret',
              'mlbench','mice','countrycode','highcharter',"gridExtra",
              'stopwords','tidytext','stringr','TTR','xts',
              'reshape2', 'textmineR','topicmodels','textclean')
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

## Load Data
df <- read_csv("survey_results_public.csv")

## UI dropdowns
developer_style <- sort(unique(df$MainBranch))
age <- sort(unique(df$Age))
employment <- sort(unique(df$Employment))
work_style <- sort(unique(df$RemoteWork))
education <- sort(unique(df$EdLevel))
devtype <- sort(unique(df$DevType))
orgsize <- sort(unique(df$OrgSize))
country <- sort(unique(df$Country))
indistry <- sort(unique(df$Industry))


## UI 
ui <- dashboardPage(
  dashboardHeader(title = "2023 Stack Overflow Survey Insights"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "about", icon = icon("th")),
      menuItem("Demographics", tabName = "overview", icon = icon("th")),
      menuItem("Experience", tabName = "overview", icon = icon("th")),
      menuItem("Learning", tabName = "overview", icon = icon("th")),
      menuItem("Tooling", tabName = "overview", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "about",includeMarkdown("readme.md"),hr())
    
   )
 )
)

## Server
server <- function(input, output, session) {}