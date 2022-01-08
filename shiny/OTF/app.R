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

#--------------
# data information
#--------------
organization <- c(sort(unique(df$Organization_name)))
city <- budget_fund <- c(sort(unique(df$Recipient_org_city_update)))
year <- as.integer(c(sort(unique(df$Fiscal_year_update))))
grant_program <- c(sort(unique(df$Grant_program)))
age_group <- c(sort(unique(df$Age_group_update)))
budget_fund <- c(sort(unique(df$Budget_fund_update)))
program_area <- c(sort(unique(df$Program_area_update)))
geo_area <- c(sort(unique(df$Geographical_area_served)))
population <- c(sort(unique(df$Population_served)))

################
# Application UI
################
#------------------
# UI
#------------------
ui <- dashboardPage(
  dashboardHeader(title = "OTF Insights"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Summary", tabName = "summary", icon = icon("th")), 
      menuItem("City", tabName = "city", icon = icon("th")),
      menuItem("Organization", tabName = "organization", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      #--------------
      # Summary
      #--------------
      tabItem(tabName = "summary",
        mainPanel(
          h2("Overview",style="text-align: center; font-style: bold;"), 
          tags$div(
            tags$p(
            tags$a(href="https://otf.ca", "Ontario Trillium Fund (OTF)"),
            "First is an agency of the Government of Ontario, and one of Canada’s leading 
            granting foundations. OTF awarded $108 million to 629 projects last year to 
            build healthy and vibrant communities in Ontario."
            ),
          ),
          fluidRow(
            valueBoxOutput("ageBox"),
            valueBoxOutput("cityBox"),
            valueBoxOutput("areaBox"),
            valueBoxOutput("populationBox"),
            valueBoxOutput("grantBox"),
            valueBoxOutput("programBox")
          ), 
          fluidRow(
            plotOutput("grantPlot", height = 300)
            #box(title = "Grant Amount", status = "primary", 
            #    plotOutput("grantPlot", height = 250, width="100%"))
          )
        )
      ),
      #--------------
      # City
      #--------------
      tabItem(tabName = "city",
              sidebarLayout(
                sidebarPanel(
                  selectInput("cityInput", "City", choices = city),
                  sliderInput("yearInput","Year",min=min(year),max=max(year),
                              value = c(min(year),max(year)),step =1,ticks = FALSE)
                ),
                mainPanel(
                  h2("Customer",style="text-align: center; font-style: bold;"), 
                  fluidRow(
                    valueBoxOutput("salesBox"),
                    valueBoxOutput("partsBox"),
                    valueBoxOutput("quantityBox")
                  )
                )
              )
            )
      #--------------
      # Organization
      #--------------
          )
        )
      )


################
# Server
################
server <- function(input, output,session) {
  
  #===============
  # Summary
  #===============
  #--------------
  # Tab-boxes
  #--------------
  output$ageBox <- renderValueBox({
    valueBox(
      paste0(length(age_group)), "Age Groups", icon = icon("list"),
      color = "purple"
    )
  })
  output$cityBox <- renderValueBox({
    valueBox(
      paste0(length(city)), "Cities", icon = icon("list"),
      color = "purple"
    )
  })
  output$areaBox <- renderValueBox({
    valueBox(
      paste0(length(geo_area)), "Geo. Areas", icon = icon("list"),
      color = "purple"
    )
  })
  output$populationBox <-  renderValueBox({
    valueBox(
      paste0(length(population)), "Population Served", icon = icon("list"),
      color = "purple"
    )
  })
  output$grantBox <-  renderValueBox({
    valueBox(
      paste0(length(grant_program)), "Grant Programs", icon = icon("list"),
      color = "purple"
    )
  })
  output$programBox <-  renderValueBox({
    valueBox(
      paste0(length(program_area)), "Program Areas", icon = icon("list"),
      color = "purple"
    )
  })
  output$budgetBox <-  renderValueBox({
    valueBox(
      paste0(length(budget_fund)), "Budget Fund Areas", icon = icon("list"),
      color = "purple"
    )
  })
  
  #--------------
  # Grants across fiscal Year (Amount given)
  #--------------
  output$grantPlot <- renderPlot({
    grant_df <- df %>%
      group_by(Fiscal_year_update) %>%
      summarise(grant_total = sum(Amount_awarded)) %>%
      select(Fiscal_year_update, grant_total)
    
    ggplot(grant_df, aes(as.factor(Fiscal_year_update),grant_total)) + 
      geom_bar(stat="identity", width = 0.5, fill="#bc5090") +
      theme_minimal() + scale_y_continuous(labels = comma) +
      labs(x = "Year", y = "Total Grant Amount") + 
      theme(legend.text = element_text(size = 12),
            legend.title = element_text(size = 12),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 12),
            axis.text.x = element_text(angle = 45, hjust = 1))
  })
  

  
  #===============
  # City Insights
  #===============
  
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
  
  #===============
  # Organization Insights
  #===============
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