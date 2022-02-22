#=========
# Mobile Phone Ratings
#=========

packages <- c('ggplot2','tidyverse','shiny','shinydashboard',
              'scales','dplyr','lubridate','reshape2')
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
                            fluidRow(
                                column(width=12,
                                       infoBoxOutput("launchInfo"),
                                       infoBoxOutput("priceInfo")
                                )
                            ),
                            fluidRow(
                                #plotOutput("propertiesPlot")
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
            "Launch Date", paste0(launch_df$launch), icon = icon("thumbs-up"),
            color = "blue", fill = TRUE
        )
    })
    
    # Price Information
    output$priceInfo <- renderInfoBox({
        price_df <- df %>%
            filter(model == input$deviceInput) %>%
            select(price)
        infoBox(
            "Price", paste0("$ ",price_df$price), icon = icon("credit-card"),
            color = "green", fill = TRUE
        )
        
    })
    
    
    # Properties Plot
    output$propertiesPlot <- renderPlot({
        properties_df <- df %>%
            filter(model == input$deviceInput) %>%
            select(price, camera, selfie, audio, display, battery)
        
        #melt data frame into long format
        plot_df <- melt(properties_df,  id.vars = 'price', variable.name = 'series')
        
        #create line plot for each column in data frame
        ggplot(df, aes(index, value)) +
          geom_bar()
            
        
    })
    
}

shinyApp(ui, server)



   