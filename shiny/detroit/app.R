#================================================================================
# Shiny web app for Detroit Open Data Challenge 2024
# The goal is to create data visualization using the Open Data Portal data
#================================================================================
rm(list = ls())
################
# Packages 
################
packages <- c(
  'ggplot2', 'corrplot','tidyverse','shiny','shinydashboard',
  'DT','Matrix','lubridate','data.table','plotly'
)
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

################
# Load data
################
youth<- read.csv("Youth_Risk_Behavioral_Surveillance_System_2017.csv")
gonorrhea<- read.csv("Number_of_Gonorrhea_Cases_by_Age_Group.csv")
chlamydia<- read.csv("Number_of_Chlamydia_Cases_by_Age_Group.csv")


################
# Data Setup
################
age_group <- c("Under-15","15-19","20-24")
year <- c(2001:2017)

colnames(gonorrhea)[3] <- "Under-15"
colnames(gonorrhea)[4] <- "15-19"
colnames(gonorrhea)[5] <- "20-24"

colnames(chlamydia)[3] <- "Under-15"
colnames(chlamydia)[4] <- "15-19"
colnames(chlamydia)[5] <- "20-24"


################
# Application UI
################
ui <- dashboardPage(
  dashboardHeader(title = "Detroit Open Data Challenge"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "about", icon = icon("th")),
      menuItem("Youth Sexual Health", tabName = "general", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "about",includeMarkdown("readme.md"),hr()),
      #-------------
      # Trend UI
      #-------------
      tabItem(tabName = "general",
              
              sidebarLayout(
                sidebarPanel(
                  sliderInput("year_input", "Year:",min = year[1] , 
                              max = year[length(year)], value = year[1], step = 1),
                  checkboxGroupInput("age_input", "Age Group", choices = age_group,
                                     selected=age_group),
                  submitButton("Submit")
                ),
                mainPanel(
                  fluidRow(
                    h4("Youth Gonorrhea Trend",style="text-align: center;"),
                    plotlyOutput("youth_gonorrhea_trend"),
                    verbatimTextOutput("hover"),
                    h4("Youth Chlamydia Trend",style="text-align: center;"),
                    plotlyOutput("youth_chlamydia_trend"),
                  ),
                  fluidRow(
                    h4("Youth Sexual Behavior",style="text-align: center;"),
                    dataTableOutput("behaviour_table")
                  
                )
              )
            )
    
        )
    )
  )
)    
################
# Server
################
server <- function(input, output,session) {
  
  #==========
  # gonorrhea
  #==========
  output$youth_gonorrhea_trend <- renderPlotly({
    
  })
  
  #==========
  # chlamydia
  #==========
  output$youth_chlamydia_trend <- renderPlotly({
    
  })
  
  #==========
  # youth behaviour
  #==========
  output$behaviour_table <- renderDataTable({
    youth_behvaior <- youth %>%
      filter(Health_Topic == 'Sexual Behaviors') %>%
      select(Statement, Male_Percent, Female_Percent)
  })
}

shinyApp(ui, server)