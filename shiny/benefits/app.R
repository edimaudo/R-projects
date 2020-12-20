#build business logic for all three dashboard
#test

rm(list = ls())

#packages 
packages <- c('ggplot2', 'corrplot','tidyverse',
              'shiny','shinydashboard','shinyWidgets',
              'dplyr','readxl')
#load packages
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

#load data 
last_year_source_data <- read_excel("BENEFIT NEW SALES TRACKER.xlsx",sheet = "LAST YEAR SOURCE")
target_source <- read_excel("BENEFIT NEW SALES TRACKER.xlsx", sheet = "TARGET SOURCE")
mtd_source <- read_excel("BENEFIT NEW SALES TRACKER.xlsx", sheet = "MTD SOURCE")
mtd_daily <- read_excel("BENEFIT NEW SALES TRACKER.xlsx", sheet = "MTD DAILY")


mtd_store <- c(sort(unique(mtd_daily$STORE)))

#=================================
# Define UI for application
ui <- dashboardPage(
  dashboardHeader(title = "Benefit New Sales Tracker"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "introduction", icon = icon("th")),
      menuItem("SKU Analysis", tabName = "sku", icon = icon("dashboard")),
      menuItem("MTD Dashboard", tabName = "mtd", icon = icon("dashboard")),
      menuItem("Growth Dashboard", tabName = "growth", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "introduction",includeMarkdown("readme.md"),hr()),
      tabItem(tabName = "sku",
              sidebarLayout(
                sidebarPanel(
                  selectInput("classInput", "Class", choices = c()),
                  selectInput("storeInput", "Store", choices = c())
                ),
                mainPanel(
                  h2("SKU Analysis",style="text-align: center;"),
                  fluidRow(
                    #DT::dataTableOutput("rebateOutput"),
                    #DT::dataTableOutput("rebateOutput")
                  )
                )
              )
      ), 
      tabItem(tabName = "mtd",
              sidebarLayout(
                sidebarPanel(
                  selectInput("storeInput","Store",choices=c())
                ),
                mainPanel(
                  h2("Benefits Sales Dashboard",style="text-align: center;")
                  #DT::dataTableOutput("rebateOutput")
                )
              )
      ),
      tabItem(tabName ="growth",
              sidebarLayout(
                sidebarPanel(
                  selectInput("storeInput","Store",choices = mtd_store)
                ),
                mainPanel(
                  h2("Daily Growth Dashboard",style="text-align: center;"),
                  #DT::dataTableOutput("rebateOutput")
                  fluidRow(
                    DT::dataTableOutput('mtd_dailygrowth_makeup_Output'),
                    DT::dataTableOutput('mtd_dailygrowth_skincare_Output')
                  )
                )
              )
      )
    )
  )
  
)

#=================================
#server info
#=================================
server <- function(input, output) {
  
  output$mtd_dailygrowth_makeup_Output <- DT::renderDataTable({
    dailygrowth_makeup <- mtd_daily %>%
      
    
    DT::datatable(dailygrowth_makeup)
  })
  
  output$mtd_dailygrowth_skincare_Output <- DT::renderDataTable({
    
  })
  
}

shinyApp(ui, server)