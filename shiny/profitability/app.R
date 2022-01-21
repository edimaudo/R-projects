##################
# packages 
##################
packages <- c('ggplot2', 'corrplot','tidyverse','shiny','shinydashboard',
              'scales','dplyr','mlbench','caTools',
              'forecast','lubridate')
for (package in packages) {
    if (!require(package, character.only=T, quietly=T)) {
        install.packages(package)
        library(package, character.only=T)
    }
}

##################
# data
##################
df <- read.csv("Profitability.csv")

#----------------
# data updates
#----------------
df$DateComplete <- as.Date(df$DateComplete, format =  "%d/%m/%Y")
df$DateLogged <- as.Date(df$DateLogged, format =  "%d/%m/%Y")

##################
# UI
##################

#----------------
# UI dropdown
#----------------
date_info <- c(sort(unique(df$DateLogged)))

ui <- dashboardPage(skin = "yellow",
                    dashboardHeader(title = "Platform analysis"),
                    dashboardSidebar(
                        sidebarMenu(
                            menuItem("Summary", tabName = "summary", icon = icon("th"))
                        )
                    ),
                    dashboardBody(
                        tabItems(
                            #----------------
                            # Summary
                            #----------------
                            tabItem(tabName = "summary",
                                    fluidRow(
                                        h2("Summary Insights",style="text-align: center;"),
                                        tabBox(
                                            title="Categories",
                                            id = "tabset1",
                                            width = "10%",
                                            selected = "Price",
                                            tabPanel("Price", plotOutput("categoryPricePlot",height = 250)),
                                            tabPanel("Ratings", plotOutput("categoryRatingPlot",height = 250)),
                                            tabPanel("Sales", plotOutput("categorySalesPlot",height = 250))
                                        ),
                                        tabBox(
                                            title="Sub-Categories",
                                            id = "tabset2",
                                            width = "10%",
                                            selected = "Price",
                                            tabPanel("Price", plotOutput("subcategoryPricePlot", height = 250)),
                                            tabPanel("Ratings", plotOutput("subcategoryRatingPlot", height = 250)),
                                            tabPanel("Sales", plotOutput("subcategorySalesPlot", height = 250))
                                            
                                        )
                                    )
                            ),


##################
# Server
##################
server <- function(input, output, session) {}


shinyApp(ui, server)