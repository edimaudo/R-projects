#=============
# Denso Sales Analysis
#=============

rm(list = ls()) #clear environment
#=============
# Packages
#=============
packages <- c('ggplot2', 'corrplot','tidyverse','readxl',
              'shiny','shinydashboard','scales','dplyr','mlbench','caTools',
              'forecast','lubridate')
for (package in packages) {
    if (!require(package, character.only=T, quietly=T)) {
        install.packages(package)
        library(package, character.only=T)
    }
}

#=============
# Load data
#=============
df <- read_excel("denso.xlsx")


#=============
# Application UI
#=============

#=============
# UI dropdowns
#=============
customer_info <- c("All",sort(unique(df$`Customer Name`)))
year_info <- c(sort(unique(df$Year)))


ui <- dashboardPage(
    dashboardHeader(title = "Company Insights"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Customer", tabName = "about", icon = icon("th")), 
            menuItem(" Customer Comparison", tabName = "health", icon = icon("th"))
        )
    ),
    dashboardBody(
        tabItems(
        ################
        # Company
        tabItem(tabName = "Customer",
                sidebarLayout(
                    sidebarPanel(
                        selectInput("customerInput",
                                    "Customer", choices = customer_info),
                        sliderInput("yearInput","Year",min=min(year_info),max=max(year_info),
                                    value = c(min(year_info),max(year_info)),
                                    step =1,ticks = TRUE)
                    ),
                    mainPanel(
                        h2("Customer",style="text-align: center; font-style: bold;"), 
                        fluidRow(
                            # tabBox(
                            #     title = "Cancer care",
                            #     id = "tabset1",
                            #     tabPanel("Incidence", plotOutput("incidencePlot", height = 150)),
                            #     tabPanel("Death", plotOutput("deathPlot", height = 150)),
                            #     tabPanel("Top 5 Cancer sites", 
                            #              plotOutput("cancerSitePlot", height = 150))
                            # )
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

}


shinyApp(ui, server)

