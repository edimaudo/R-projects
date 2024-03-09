#================================================================================
# Shiny web app which provides insights into Ocean Protocol's Discord server, 
# providing a deeper understanding of the community's dynamics and predicting future activity pattern
#================================================================================
rm(list = ls())
################
#packages 
################
packages <- c(
  'ggplot2', 'corrplot','tidyverse','shiny','shinydashboard','DT',
  'mlbench','caTools','gridExtra','doParallel','grid','forecast',
  'caret','dummies','mlbench','tidyr','Matrix','lubridate',
  'data.table', 'rsample','scales','stopwords','tidytext','stringr',
  'reshape2', 'textmineR','topicmodels','textclean'
)
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

################
#Load data
################
df<- read.csv("Ocean Discord Data Challenge Dataset.csv")

# channel setup
channel <- sort(unique(df$Channel))
channel <- c("All",channel)

# Date setup
df$Date2 <- lubridate::mdy(substring(df$Date,1,10))
df$year <- lubridate::year(df$Date2)
df$quarter <- lubridate::quarter(df$Date2)
df$month <- lubridate::month(df$Date2)
df$week <- lubridate::week(df$Date2)
df$day <- lubridate::mday(df$Date2)
df$dayofweek <- lubridate::wday(df$date, label=TRUE)


################
#UI
################
ui <- dashboardPage(
  dashboardHeader(title = "Ocean Discord Data Challenge"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "about", icon = icon("th")),
      menuItem("General Trends", tabName = "general", icon = icon("th")),
      menuItem("Community Activity", tabName = "community", icon = icon("th")),
      menuItem("Server Activity", tabName = "server", icon = icon("th")),
      menuItem("Community Questions", tabName = "questions", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "about",includeMarkdown("readme.md"),hr()),
      tabItem(tabName = "general",
              sidebarLayout(
                sidebarPanel(
                  selectInput("channelInput", "Channel", choices = c("Red","White"), selected = "Red"),
                  selectInput("channelSubCategoryInput", "Channel Sub-category", choices = c("Red","White"), selected = "Red"),
                  submitButton("Submit")
                ),
                mainPanel(
                )
              )
      )
    )
   )
  ) 
 

################
# Define server logic 
################
server <- function(input, output,session) {
  
}




shinyApp(ui, server)