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
            menuItem("Phone Ratings", tabName = "rating", icon = icon("dashboard"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "rating",
                    sidebarLayout(
                        sidebarPanel(
                            selectInput("deviceInput", "Device",choices=model),
                            br(),
                            submitButton("Submit")
                        ),
                        mainPanel(
                            h2("Device Insights",style="text-align: center;"),
                            column(width=2,
                            infoBoxOutput("launchInfo"),
                            infoBoxOutput("priceInfo")
                            ),
                            
                            fluidRow(
                                plotOutput("propertiesPlot")
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
    
    
    # Launch Information
    output$launchInfo <- renderInfoBox({
        
        launch_df <- df %>%
            filter(model == input$deviceInput) %>%
            select(launch)
        
        infoBox(
            "Launch Date", paste0(launch_df), icon = icon("list"),
            color = "blue"
        )
    })
    
    # Price Information
    output$priceInfo <- renderInfoBox({
        
    })
    
    
    # Properties Plot
    
}

shinyApp(ui, server)



   