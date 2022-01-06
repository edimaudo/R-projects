rm(list = ls())
##################
# packages 
##################
packages <- c('ggplot2', 'corrplot','tidyverse','shiny','shinydashboard','readxl',
              'scales','dplyr','mlbench','caTools',
              'forecast','lubridate','tidytext',
              'SnowballC','wordcloud', 'RColorBrewer')
for (package in packages) {
    if (!require(package, character.only=T, quietly=T)) {
        install.packages(package)
        library(package, character.only=T)
    }
}

##################
# data
##################
df <- read.csv("clean.csv")


##################
# UI
##################
ui <- dashboardPage(skin = "yellow",
    dashboardHeader(title = "Platform analysis"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Summary", tabName = "summary", icon = icon("clock")),
            menuItem("Category", tabName = "category", icon = icon("th")),
            menuItem("Sub Category", tabName = "subcategory", icon = icon("th"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "summary",
                    fluidRow(
                        h2("Summary Insights",style="text-align: center;"),
                        tabBox(
                            title="Categories",
                            id = "tabset1",
                            width = "100%",
                            selected = "Top 10 by Price",
                            tabPanel("Top 10 by Price", plotOutput("salesComparePlot", height = 150)),
                            tabPanel("Bottom 10 by Price", plotOutput("salesComparePlot", height = 150)),
                            tabPanel("Top 10 by Ratings", plotOutput("salesComparePlot", height = 150)),
                            tabPanel("Bottom 10 by Ratings", plotOutput("salesComparePlot", height = 150)),
                            tabPanel("Top 10 by Sales", plotOutput("salesComparePlot", height = 150)),
                            tabPanel("Bottom 10 by Sales", plotOutput("salesComparePlot", height = 150))
                        ),
                        tabBox(
                            title="Sub-Categories",
                            id = "tabset2",
                            width = "100%",
                            selected = "Top 10 by Price",
                            tabPanel("Top 10 by Price", plotOutput("salesComparePlot", height = 150)),
                            tabPanel("Bottom 10 by Price", plotOutput("salesComparePlot", height = 150)),
                            tabPanel("Top 10 by Ratings", plotOutput("salesComparePlot", height = 150)),
                            tabPanel("Bottom 10 by Ratings", plotOutput("salesComparePlot", height = 150)),
                            tabPanel("Top 10 by Sales", plotOutput("salesComparePlot", height = 150)),
                            tabPanel("Bottom 10 by Sales", plotOutput("salesComparePlot", height = 150))
                        )
                    )
            )
        ),
        tabItems(
            tabItem(tabName = "category",
                    fluidRow()
            )
        ),
        tabItems(
            tabItem(tabName = "subcategory",
                    fluidRow()
            )
        )
    )
)

##################
# Server
##################
server <- function(input, output) {
    
}


shinyApp(ui, server)