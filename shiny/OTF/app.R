################
# Packages
################
rm(list = ls()) #clear environment
packages <- c('ggplot2', 'corrplot','tidyverse','readxl','SnowballC','wordcloud', 'RColorBrewer',
              'shiny','shinydashboard','scales','dplyr','mlbench','caTools',
              'forecast','lubridate')
for (package in packages) {
  if (!require(package, character.only=T, quietly=T)) {
    install.packages(package)
    library(package, character.only=T)
  }
}

################
# Load data
################
df <- read_excel("otf.xlsx")


################
# Application UI
################
#------------------
# UI drop-downs
#------------------
organization <- c(sort(unique(df$Organization_Name)))
city <- budget_fund <- c(sort(unique(df$Recipient_org_city_update)))
year <- c(sort(unique(df$Fiscal_year_update)))
grant_program <- c(sort(unique(df$Grant_program)))
age_group <- c(sort(unique(df$Age_group_update)))
budget_fund <- c(sort(unique(df$Budget_fund_update)))
program_area <- c(sort(unique(df$Program_area_update)))

#------------------
# UI
#------------------
ui <- dashboardPage(
  dashboardHeader(title = "OTF Insights"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Summary", tabName = "summary", icon = icon("th")), 
      menuItem("City", tabName = "city", icon = icon("th")),
      menuItem("Organization", tabName = "organization", icon = icon("th")),
    )
  ),
  dashboardBody(
    tabItems(
    )
  )
)


################
# Server
################
server <- function(input, output,session) {
  
  #---------------
  # Summary
  #---------------
  
  # Age groups
  
  # Recipient org city
  
  # Area served
  
  # Population served
  
  # of grant programs
  
  # Program area
  
  # Budget fund
  
  # Province served
  
  # Grants across fiscal Year (Amount applied)
  
  # Grants across fiscal Year (Amount given)
  

  
  #---------------
  # City Insights
  #---------------
  
  # Grants by
    # Age groups
    
    # Area served
    
    # Population served
    
    # of grant programs
    
    # Program area
    
    # Budget fund
  
    # geogrpahical region
  
    # Grants across fiscal Year (Amount applied)
  
  # top 10 words
  
  # top 10 topics
  
  #---------------
  # Organization Insights
  #---------------
# Grants by
  
  # Age groups
  
  # Area served
  
  # Population served
  
  # of grant programs
  
  # Program area
  
  # geogrpahical region
  
  # Budget fund
  
  # Grants across fiscal Year (Amount applied)
  
  # top words
  
  # Average sentiment of descriptions
}

shinyApp(ui, server)