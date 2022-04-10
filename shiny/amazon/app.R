################
# Packages
################
rm(list = ls()) #clear environment
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

#--------------
# UI
#--------------
ui <- dashboardPage(
    dashboardHeader(title = "Amazon Review Analysis"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Summary", tabName = "summary", icon = icon("th")),
            menuItem("Analysis", tabName = "analysis", icon = icon("th")),
            menuItem("Sentiment Analysis", tabName = "sentiment", icon = icon("th")),
            menuItem("Term Frequency", tabName = "term", icon = icon("th")),
            menuItem("Topic Modeling", tabName = "topic", icon = icon("th"))
        )
    ),
    dashboardBody(
        tabItems(
            #===============
            # Summary
            #===============
            tabItem(tabName = "summary",
                        mainPanel(
                            h1("Summart",style="text-align: center;"), 
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
                    ),
                    
            
            #==============
            #Analysis UI
            #==============
            tabItem(tabName = "analysis",
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
            # Sentiment Analysis UI
            #==============
            tabItem(tabName = "sentiment",
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
                            h1("Sentiment Analysis",style="text-align: center;"),
                            plotOutput("sentimentplot")
                        )
                    )
            ),
            #==============
            # Term frequency
            #==============
            tabItem(tabName = "term",
                    #sidebarLayout(
                    #sidebarPanel(
                    #checkboxGroupInput("printerInput", "Printers",
                    #                   choices = printer_info, 
                    #                   selected = printer_info),
                    #submitButton("Submit")
                    # ),
                    mainPanel(
                        h1("TF-IDF",style="text-align: center;"), 
                        plotOutput("termplot")
                        
                    )
                    # )
            ),
            #==============
            # Topic modeling
            #==============
            tabItem(tabName = "topic",
                    sidebarLayout(
                        sidebarPanel(
                            # checkboxGroupInput("printerInput", "Printers",
                            #                    choices = printer_info, 
                            #                    selected = printer_info),
                            # checkboxGroupInput("ratingInput", "Ratings",
                            #                    choices = score_info, 
                            #                    selected = score_info),
                            submitButton("Submit")
                        ),
                        mainPanel(
                            h1("Topic Modeling",style="text-align: center;"),
                            DT::dataTableOutput("termtable")
                        )
                    )
            )
        )
    )
    
)