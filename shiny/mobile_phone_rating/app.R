#=========
# Mobile Phone Ratings
#=========

packages <- c('ggplot2', 'corrplot','tidyverse','readxl',
              'shiny','shinydashboard','scales','dplyr','mlbench','caTools',
              'forecast','TTR','xts','lubridate','plotly')

for (package in packages) {
    if (!require(package, character.only=T, quietly=T)) {
        install.packages(package)
        library(package, character.only=T)
    }
}

#=========
# Load data
#=========
df <- read.csv("mobile phone rating.csv")

#----------
# UI Dropdown
#----------
model <- sort(c(unique(df$model)))

#=========
# UI
#=========
ui <- dashboardPage(
    dashboardHeader(title = "Mobile phone rating"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Phone Ratings", tabName = "rating", icon = icon("dashboard")),
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "rating",
                    sidebarLayout(
                        sidebarPanel(
                            sliderInput("Years", "Years:", min = 1999, max = 2019, 
                                        step=1, ticks = FALSE, sep="")
                        ),
                        mainPanel(
                            fluidRow(
                                h2("Grants and Amount Awarded",style="text-align: center;"),
                                plotOutput("grantAwarded")
                            )
                    )
                
            )
        )
    )
  )
)
#=========
# Server
#=========
server <- function(input, output,session) {
    
}

shinyApp(ui, server)


fluidRow(
    infoBoxOutput("yearInfo"),
    infoBoxOutput("grantInfo")