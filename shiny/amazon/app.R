################
# Packages
################
packages <- c('ggplot2','corrplot','tidyverse','readxl', 
              'RColorBrewer','shiny','shinydashboard','scales','dplyr',
              'forecast','lubridate','stopwords','tidytext','stringr',
              'reshape2', 'textmineR','topicmodels','textclean')
for (package in packages) {
    if (!require(package, character.only=T, quietly=T)) {
        install.packages(package)
        library(package, character.only=T)
    }
}
################
# Load data
################
df <- read.csv("Amazon_Reviews_Vitamin_C.csv")

#=============
# Text analytics
#=============








################
# UI
################
ui <- dashboardPage(
    dashboardHeader(title = "Amazon Review Analysis"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Summary", tabName = "summary", icon = icon("dashboard")),
            menuItem("Explore", tabName = "explore", icon = icon("th")),
            menuItem("Text Analysis", tabName = "text", icon = icon("clock"))#,
            #menuItem("Term Frequency", tabName = "term", icon = icon("th")),
            #menuItem("Topic Modeling", tabName = "topic", icon = icon("th"))
        )
    ),
    dashboardBody(
        tabItems(
            #===============
            # Summary
            #===============
            tabItem(tabName = "summary",
                        mainPanel(
                            h1("Summary",style="text-align: center;"), 
                            fluidRow(
                                valueBoxOutput("productBox", width = 3),
                                valueBoxOutput("countryBox",width = 3),
                                valueBoxOutput("yearBox",width = 3),
                                valueBoxOutput("ratingBox",width = 3),
                            ),
                            
                             tabsetPanel(type = "tabs",
                                         tabPanel(h4("Avg. Rating by Month",
                                                     style="text-align: center;"), 
                                                  plotOutput("avgMonthRatingPlot")),
                                         tabPanel(h4("Average Rating by Year",
                                                     style="text-align: center;"), 
                                                  plotOutput("avgYearRatingPlot"))
                             )
                        )
                    ),
                    
            
            #==============
            #Explore UI
            #==============
            tabItem(tabName = "explore",
                    sidebarLayout(
                        sidebarPanel(
                            # checkboxGroupInput("printerInput", "Printers",
                            #                    choices = printer_info, 
                            #                    selected = printer_info),
                            submitButton("Submit")
                        ),
                        mainPanel(
                            h1("Analysis",style="text-align: center;"), 
                            # tabsetPanel(type = "tabs",
                            #             tabPanel(h4("Average Printer Score",
                            #                         style="text-align: center;"), 
                            #                      plotOutput("avgPrinterScoreplot")),
                            #             tabPanel(h4("Printer Score Count",
                            #                         style="text-align: center;"), 
                            #                      plotOutput("countPrinterScoreplot")),
                            #             tabPanel(h4("Average Printer Score over time",
                            #                         style="text-align: center;"), 
                            #                      plotOutput("avgPrinterScoreYearplot"))
                            # )
                        )
                    )
            ), 
            #==============
            # Text analytics UI
            #==============
            tabItem(tabName = "text",
                    sidebarLayout(
                        sidebarPanel(
                            # checkboxGroupInput("printerInput", "Printers",
                            #                    choices = printer_info, 
                            #                    selected = printer_info),
                            # checkboxGroupInput("ratingInput", "Ratings",
                            #                    choices = score_info, 
                            #                    selected = score_info),
                            # submitButton("Submit")
                        ),
                        mainPanel(
                            #h1("Sentiment Analysis",style="text-align: center;"),
                            #plotOutput("sentimentplot")
                        )
                    )
            )

        )
    )
)

################
# Server
################

# Define server logic 
server <- function(input, output,session) {
    
 
    
    #===============
    # Summary logic
    #===============
    output$productBox <- renderValueBox({
        valueBox(
            paste0(length(unique(df$Procuct))), "Products", icon = icon("list"),
            color = "green"
        )
    })
    
    output$countryBox <- renderValueBox({
        valueBox(
            paste0(length(unique(df$Country))), "Countries", icon = icon("list"),
            color = "green"
        )
    })
    
    output$yearBox <- renderValueBox({
        valueBox(
            paste0(length(unique(df$Year))), "Years", icon = icon("list"),
            color = "green"
        )
    })
    
    output$ratingBox <- renderValueBox({
        output <- mean(na.omit(df$Rating))
        output <- format(round(output, 1), nsmall = 2)
        valueBox(
            paste0(output), "Avg. Ratings", icon = icon("list"),
            color = "green"
        )
    })
    
    output$avgMonthRatingPlot <- renderPlot({
        
        
    })
    
    output$avgYearRatingPlot <- renderPlot({
        
        
    })
    
    #==============
    #Explore logic
    #==============
    
    #==============
    # Text analytics logic
    #==============
}


shinyApp(ui, server)