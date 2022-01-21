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
                    menuItem("Dashboard", tabName = "dashboard", icon = icon("th"))
                )
            ),
            dashboardBody(
                tabItems(
                #----------------
                # Dashboard
                #----------------
                    tabItem(tabName = "dashboard",
                            sidebarLayout(
                                sidebarPanel(
                                    sliderInput("dateInput","Dates",min=min(date_info),
                                                max=max(date_info),
                                                value = c(min(date_info),max(date_info)),
                                                step =1,ticks = FALSE)   
                                  ),
                                mainPanel(
                                    h2("Insights",style="text-align: center; font-style: bold;"), 
                                    

                                    
                                )
                            )
                        )
                    )
            )
)

##################
# Server
##################
server <- function(input, output, session) {}


shinyApp(ui, server)